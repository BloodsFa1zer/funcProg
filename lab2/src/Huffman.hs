{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Pure byte-oriented Huffman coding.
--
-- A file is compressed independently of the others: count byte
-- frequencies, build the Huffman tree, emit the bit codes. Only the
-- frequency table is stored in the archive — 'buildTree' is
-- deterministic, so the decoder rebuilds the identical tree.
module Huffman
  ( Compressed(..)
  , compress
  , decompress
  ) where

import           Control.Monad.ST (ST, runST)
import           Data.Array (Array, listArray, (!), (//))
import           Data.Array.ST (STUArray, getAssocs, newArray, readArray,
                                writeArray)
import           Data.Binary (Binary (..))
import           Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder, toLazyByteString, word8)
import qualified Data.ByteString.Lazy as BL
import           Data.List (insertBy, sortOn)
import           Data.Ord (comparing)
import           Data.Word (Word64, Word8)

-- | One compressed blob: the frequency table (to rebuild the tree),
-- the exact number of meaningful bits, and the packed bits themselves.
-- Strict fields: constructing the value forces the compression work,
-- so it cannot leak as a thunk out of a worker thread.
data Compressed = Compressed
  { cFreqs  :: ![(Word8, Word64)]
  , cBitLen :: !Word64
  , cBytes  :: !BS.ByteString
  }

instance Binary Compressed where
  put (Compressed fs bl bs) = put fs >> put bl >> put bs
  get = Compressed <$> get <*> get <*> get

-- Frequency counting ----------------------------------------------------------

byteFreqs :: BS.ByteString -> [(Word8, Word64)]
byteFreqs bs = runST $ do
  counts <- newArray (0, 255) 0 :: ST s (STUArray s Int Word64)
  let go !i | i >= BS.length bs = pure ()
            | otherwise = do
                let b = fromIntegral (BS.index bs i)
                c <- readArray counts b
                writeArray counts b (c + 1)
                go (i + 1)
  go 0
  found <- getAssocs counts
  pure [ (fromIntegral b, c) | (b, c) <- found, c > 0 ]

-- Huffman tree ----------------------------------------------------------------

data HTree
  = Leaf !Word64 !Word8
  | Node !Word64 HTree HTree

weight :: HTree -> Word64
weight (Leaf w _)   = w
weight (Node w _ _) = w

-- | Deterministic tree construction: leaves sorted by weight (ties keep
-- byte order), then the two lightest trees merge until one remains.
-- A lone symbol gets a dummy sibling, so the root is always a Node and
-- a one-symbol file costs one bit per byte — no special cases later.
buildTree :: [(Word8, Word64)] -> Maybe HTree
buildTree []       = Nothing
buildTree [(b, c)] = Just (Node c (Leaf c b) (Leaf 0 b))
buildTree fs       = Just (go (sortOn weight [Leaf c b | (b, c) <- fs]))
  where
    go []        = error "buildTree: impossible"
    go [t]       = t
    go (a:b:rest) =
      go (insertBy (comparing weight) (Node (weight a + weight b) a b) rest)

-- | Bit code of every byte as (bits, length), where the code is stored
-- in the low @length@ bits and emitted most significant bit first.
-- A Word64 buffer is safe up to 57-bit codes (7 carried bits + code
-- must fit in 64); depth d needs at least Fibonacci(d+2) input bytes,
-- so exceeding 57 would take a terabyte-scale file.
codeArray :: HTree -> Array Word8 (Word64, Int)
codeArray root = listArray (0, 255) (repeat (0, 0)) // walk root 0 0
  where
    walk (Leaf _ b) !acc !len = [(b, (acc, len))]
    walk (Node _ l r) !acc !len =
      walk l (acc `shiftL` 1) (len + 1)
        ++ walk r ((acc `shiftL` 1) .|. 1) (len + 1)

-- Encoding --------------------------------------------------------------------

compress :: BS.ByteString -> Compressed
compress input =
  case buildTree freqs of
    Nothing   -> Compressed [] 0 BS.empty
    Just tree ->
      let codes = codeArray tree
          -- frequency times code length: O(256), no extra input pass
          bits  = sum [ c * fromIntegral (snd (codes ! b)) | (b, c) <- freqs ]
      in Compressed freqs bits (packBits codes input)
  where
    freqs = byteFreqs input

-- | Pack the bit codes of all input bytes, most significant bit first;
-- the last byte is padded with zeroes (cBitLen marks the real end).
packBits :: Array Word8 (Word64, Int) -> BS.ByteString -> BS.ByteString
packBits codes input = BL.toStrict (toLazyByteString (go 0 0 0))
  where
    go !i !buf !n
      | i >= BS.length input =
          if n > 0
            then word8 (fromIntegral ((buf `shiftL` (8 - n)) .&. 0xff))
            else mempty
      | otherwise =
          let (bits, len) = codes ! BS.index input i
          in flush ((buf `shiftL` len) .|. bits) (n + len) (i + 1)
    -- emit full bytes from the top of the n-bit buffer
    flush !buf !n !i
      | n >= 8 =
          let n' = n - 8
          in word8 (fromIntegral ((buf `shiftR` n') .&. 0xff))
               <> flush (buf .&. ((1 `shiftL` n') - 1)) n' i
      | otherwise = go i buf n

-- Decoding --------------------------------------------------------------------

-- | Precondition (checked by Archive.readArchive): cBitLen fits the
-- payload, i.e. cBitLen <= 8 * length cBytes.
decompress :: Compressed -> BS.ByteString
decompress (Compressed freqs bitLen payload) =
  case buildTree freqs of
    Nothing   -> BS.empty
    Just root -> BL.toStrict (toLazyByteString (go root 0))
  where
    bitAt :: Word64 -> Bool
    bitAt i = testBit (BS.index payload (fromIntegral (i `shiftR` 3)))
                      (7 - fromIntegral (i .&. 7))
    go :: HTree -> Word64 -> Builder
    go root !i
      | i >= bitLen = mempty
      | otherwise   = walk root i
      where
        walk (Leaf _ b) !j   = word8 b <> go root j
        walk (Node _ l r) !j = walk (if bitAt j then r else l) (j + 1)
