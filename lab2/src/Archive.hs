-- | Archive format: a magic number and a list of entries, serialized
-- with Data.Binary. Every entry keeps its own Huffman frequency table,
-- which is what makes per-file parallel compression possible.
--
-- readArchive is the trust boundary: structural decoding AND semantic
-- validation (entry names, bit lengths) happen here, so the rest of the
-- program can rely on well-formed entries.
module Archive
  ( Entry(..)
  , writeArchive
  , readArchive
  ) where

import           Data.Binary (Binary (..), decodeOrFail, encodeFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Word (Word32, Word64)
import           System.FilePath (takeFileName)
import           Huffman (Compressed (..))

data Entry = Entry
  { entryName :: !String      -- ^ file name inside the archive
  , entrySize :: !Word64      -- ^ original size in bytes
  , entryData :: !Compressed
  }

instance Binary Entry where
  put (Entry n s d) = put n >> put s >> put d
  get = Entry <$> get <*> get <*> get

magic :: Word32
magic = 0x48554631 -- "HUF1"

writeArchive :: FilePath -> [Entry] -> IO ()
writeArchive path entries = encodeFile path (magic, entries)

readArchive :: FilePath -> IO (Either String [Entry])
readArchive path = do
  raw <- BL.readFile path
  pure $ case decodeOrFail raw of
    Left (_, _, err) -> Left err
    Right (rest, _, (m, entries))
      | m /= magic         -> Left "not a HUF1 archive"
      | not (BL.null rest) -> Left "trailing garbage after archive data"
      | otherwise          -> mapM checkEntry entries

-- | Reject entries that could crash or escape later: path components
-- in the name, or a bit length that overruns the stored payload.
checkEntry :: Entry -> Either String Entry
checkEntry e
  | null name || takeFileName name /= name =
      Left ("bad entry name: " ++ show name)
  | cBitLen d > 8 * fromIntegral (BS.length (cBytes d)) =
      Left ("corrupted entry " ++ name ++ ": bit length exceeds payload")
  | otherwise = Right e
  where
    name = entryName e
    d    = entryData e
