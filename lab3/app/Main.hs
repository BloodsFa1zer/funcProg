-- | Lab 3, variant 4: in all question sentences of a textbook file,
-- find and print without repetitions the words of a given length.
module Main (main) where

import           Control.Exception (IOException, catch)
import           Control.Monad (forM_, when)
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.IO (BufferMode (NoBuffering), hSetBuffering, isEOF,
                            stdout)
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Parser (parseText)
import           Task (questionSentences, wordsOfLength)
import           TextTypes (renderSentence, wordText)

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout NoBuffering
  args <- getArgs
  readableIOErrors $ case args of
    [path]      -> run path Nothing
    [path, len] -> case parseLen len of
      Just n  -> run path (Just n)
      Nothing -> die "Довжина має бути цілим числом від 1 до 1000."
    _ -> die usage

usage :: String
usage = unlines
  [ "Використання:"
  , "  lab3 <файл-підручника> [довжина слова]"
  , "Без другого аргументу довжина запитується інтерактивно."
  ]

readableIOErrors :: IO () -> IO ()
readableIOErrors act = act `catch` \e ->
  die ("Помилка вводу-виводу: " ++ show (e :: IOException))

-- | Parsed through Integer: Read Int silently wraps out-of-range input.
parseLen :: String -> Maybe Int
parseLen s = do
  n <- readMaybe s :: Maybe Integer
  if n >= 1 && n <= 1000 then Just (fromInteger n) else Nothing

promptLen :: IO Int
promptLen = do
  putStr "Довжина слова: "
  eof <- isEOF
  when eof (die "Довжину слова не вказано.")
  s <- getLine
  case parseLen s of
    Just n  -> pure n
    Nothing -> putStrLn "Потрібне ціле число від 1 до 1000." >> promptLen

run :: FilePath -> Maybe Int -> IO ()
run path mlen = do
  raw <- readFile path
  let sentences = parseText raw
      questions = questionSentences sentences
  printf "Речень у тексті: %d, з них питальних: %d\n"
    (length sentences) (length questions)
  when (null questions) $ die "У тексті немає питальних речень."
  putStrLn "Питальні речення:"
  forM_ questions $ \q -> putStrLn ("  " ++ renderSentence q)
  n <- maybe promptLen pure mlen
  let found = wordsOfLength n questions
  if null found
    then printf "Слів довжини %d у питальних реченнях немає.\n" n
    else do
      printf "Слова довжини %d (без повторень, у порядку появи):\n" n
      forM_ found $ \w -> putStrLn ("  - " ++ wordText w)
