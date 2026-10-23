-- | Turning the raw textbook file into a list of 'Sentence's.
-- Per the assignment, tabs and runs of spaces collapse into one space.
module Parser
  ( normalize
  , parseText
  ) where

import           Data.Char (isAlpha, isDigit)
import           Data.List (dropWhileEnd)
import           TextTypes

-- | Replace all whitespace (tabs, newlines, space runs) with single
-- spaces. 'words'/'unwords' do exactly that.
normalize :: String -> String
normalize = unwords . words

endMarks :: String
endMarks = ".?!"

-- | Split normalized text into sentences by '.', '?' or '!'.
parseText :: String -> [Sentence]
parseText = go . normalize
  where
    go s = case splitSentence s of
      Nothing             -> []   -- trailing fragment without an end mark
      Just (body, run, rest) ->
        let toks = tokenize body
        in if null toks then go rest
                        else Sentence toks (classifyEnd run) : go rest

-- | Find the next sentence boundary: the body before the end-mark run,
-- the run itself (e.g. "?!" or "..."), and the remaining text. A '.'
-- between two digits is a decimal point, not a boundary.
splitSentence :: String -> Maybe (String, String, String)
splitSentence = scan []
  where
    scan _   [] = Nothing
    scan acc (c : cs)
      | c `elem` endMarks && not (decimalDot c acc cs) =
          let (run, rest) = span (`elem` endMarks) (c : cs)
          in Just (reverse acc, run, rest)
      | otherwise = scan (c : acc) cs
    decimalDot '.' (p : _) (n : _) = isDigit p && isDigit n
    decimalDot _   _       _       = False

-- | A sentence is a question when its end-mark run contains '?', so
-- both "?!" and "!?" count; otherwise '!' beats '.'.
classifyEnd :: String -> Char
classifyEnd run
  | '?' `elem` run = '?'
  | '!' `elem` run = '!'
  | otherwise      = '.'

-- | Characters a word may contain: letters, digits, inner apostrophes
-- (all three variants) and hyphens.
wordChar :: Char -> Bool
wordChar c = isAlpha c || isDigit c || c == '-' || c `elem` apostrophes

-- | Apostrophes and hyphens are allowed inside a word but trimmed at its
-- edges, so «-то» / «край-» become plain words.
edge :: Char -> Bool
edge ch = ch == '-' || ch `elem` apostrophes

-- | Split a sentence body into word and punctuation tokens.
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | c == ' '   = tokenize cs
  | wordChar c =
      let (raw, rest) = span wordChar (c : cs)
          core = dropWhileEnd edge (dropWhile edge raw)
      in if null core
           then map TMark raw ++ tokenize rest   -- bare hyphens/apostrophes
           else TWord (Word' (map classify core)) : tokenize rest
  | otherwise  = TMark c : tokenize cs
