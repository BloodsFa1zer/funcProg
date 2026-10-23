-- | Lab 2, variant 1: multithreaded compression of a set of files
-- into one archive with Huffman coding, plus a benchmark mode that
-- shows the speedup on a multiprocessor system.
module Main (main) where

import           Control.Concurrent (getNumCapabilities, setNumCapabilities)
import           Control.Exception (IOException, catch, evaluate)
import           Control.Monad (forM, forM_, unless, when)
import qualified Data.ByteString as BS
import           Data.List (nub, (\\))
import           GHC.Clock (getMonotonicTime)
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           System.Directory (createDirectoryIfMissing, getFileSize)
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.FilePath (takeFileName, (</>))
import           System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Archive (Entry (..), readArchive, writeArchive)
import           Huffman (Compressed (..), compress, decompress)
import           Pool (parMapPool)

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout LineBuffering
  args <- getArgs
  readableIOErrors $ case args of
    ("pack" : archive : rest) -> do
      (k, files) <- jobsAndFiles rest
      when (null files) (die "Не вказано жодного файлу для стиснення.")
      checkUniqueNames files
      pack k archive files
    ("unpack" : archive : outdir : rest) | outdir /= "-j" -> do
      (k, _) <- jobsAndFiles rest
      unpack k archive outdir
    ("bench" : rest) -> do
      (kmax, files) <- jobsAndFiles rest
      when (null files) (die "Не вказано жодного файлу для заміру.")
      bench kmax files
    _ -> die usage

-- | One place that turns raw IO errors (missing input file, permission
-- denied) into a readable message instead of a GHC exception dump.
readableIOErrors :: IO () -> IO ()
readableIOErrors act = act `catch` \e ->
  die ("Помилка вводу-виводу: " ++ show (e :: IOException))

usage :: String
usage = unlines
  [ "Використання:"
  , "  lab2 pack   <архів> <файл...> [-j N]   стиснути файли в архів"
  , "  lab2 unpack <архів> <каталог> [-j N]   розпакувати архів у каталог"
  , "  lab2 bench  <файл...>         [-j N]   заміряти прискорення за потоками"
  , "За замовчуванням N = кількість ядер (+RTS -N)."
  ]

-- | Extract "-j N" from the argument list; default is the core count.
-- Parsed through Integer: Read Int silently wraps out-of-range input.
jobsAndFiles :: [String] -> IO (Int, [String])
jobsAndFiles args = case break (== "-j") args of
  (files, []) -> do
    k <- getNumCapabilities
    accept k files
  (before, _ : nStr : after)
    | Just n <- readMaybe nStr :: Maybe Integer
    , n >= 1, n <= 1024 -> accept (fromInteger n) (before ++ after)
  _ -> die "Після -j потрібне число потоків (1..1024)."
  where
    accept k files = do
      when ("-j" `elem` files) (die "Ключ -j можна вказати лише один раз.")
      pure (k, files)

-- | Entries are stored by base name, so duplicates would silently
-- overwrite each other at unpack time.
checkUniqueNames :: [FilePath] -> IO ()
checkUniqueNames files = do
  let names = map takeFileName files
      dups  = nub (names \\ nub names)
  unless (null dups) $
    die ("Однакові імена файлів в одному архіві неприпустимі: " ++ unwords dups)

timed :: IO a -> IO (Double, a)
timed act = do
  t0 <- getMonotonicTime
  r  <- act
  t1 <- getMonotonicTime
  pure (t1 - t0, r)

-- | Read and compress one file. Entry/Compressed have strict fields and
-- the pool forces the result, so the work happens in the worker thread
-- and the raw file bytes are not retained after it returns.
compressFile :: FilePath -> IO Entry
compressFile path = do
  raw <- BS.readFile path
  pure $! Entry (takeFileName path) (fromIntegral (BS.length raw)) (compress raw)

pack :: Int -> FilePath -> [FilePath] -> IO ()
pack k archive files = do
  printf "Стискаю %d файлів у %d потоків...\n" (length files) k
  (dt, entries) <- timed (parMapPool k compressFile files)
  writeArchive archive entries
  forM_ entries $ \e -> do
    let csize = BS.length (cBytes (entryData e))
    printf "  %-28s %10d -> %10d байт (%.1f%%)\n"
      (entryName e) (entrySize e) csize (ratio (entrySize e) csize)
  total <- getFileSize archive
  let orig = sum (map entrySize entries)
  printf "Архів %s: %d -> %d байт (%.1f%%), час стиснення: %.2f с\n"
    archive orig total (ratio orig total) dt
  where
    ratio :: (Integral a, Integral b) => a -> b -> Double
    ratio o c | o == 0    = 100
              | otherwise = 100 * fromIntegral c / fromIntegral o

unpack :: Int -> FilePath -> FilePath -> IO ()
unpack k archive outdir = do
  result <- readArchive archive
  case result of
    Left err -> die ("Не вдалося прочитати архів: " ++ err)
    Right entries -> do
      createDirectoryIfMissing True outdir
      printf "Розпаковую %d файлів у %d потоків...\n" (length entries) k
      (dt, _) <- timed $ parMapPool k restore entries
      printf "Готово за %.2f с, файли в %s\n" dt outdir
  where
    restore e = do
      let out = decompress (entryData e)
      when (fromIntegral (BS.length out) /= entrySize e) $
        ioError (userError ("пошкоджений запис: " ++ entryName e))
      BS.writeFile (outdir </> entryName e) out

-- | Compress the same preloaded data with 1, 2, 4, ... threads and
-- report the speedup against the single-threaded run. Files are read
-- once up front so only CPU work is measured; one untimed warm-up pass
-- keeps the cold-start cost out of the k=1 baseline.
bench :: Int -> [FilePath] -> IO ()
bench kmax files = do
  raws <- forM files $ \f -> do
    raw <- BS.readFile f
    _ <- evaluate (BS.length raw)
    pure raw
  let sizeMB = fromIntegral (sum (map BS.length raws)) / (1024 * 1024) :: Double
      ks = takeWhile (< kmax) (iterate (* 2) 1) ++ [kmax]
      work k = parMapPool k (pure . compress) raws
  printf "Дані: %d файлів, %.1f МБ; макс. потоків: %d\n"
    (length files) sizeMB kmax
  _ <- work kmax -- warm-up, untimed
  printf "%8s %10s %12s %14s\n"
    ("потоки" :: String) ("час, с" :: String)
    ("МБ/с" :: String) ("прискорення" :: String)
  results <- forM ks $ \k -> do
    setNumCapabilities k
    (dt, _) <- timed (work k)
    pure (k, dt)
  case results of
    [] -> pure ()
    (_, t1) : _ ->
      forM_ results $ \(k, dt) ->
        printf "%8d %10.2f %12.1f %13.2fx\n" k dt (sizeMB / dt) (t1 / dt)
