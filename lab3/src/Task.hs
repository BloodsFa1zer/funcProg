-- | Variant 4: in all question sentences of the text, find and print
-- without repetitions the words of a given length.
module Task
  ( questionSentences
  , wordsOfLength
  ) where

import           Data.List (nubBy)
import           TextTypes

questionSentences :: [Sentence] -> [Sentence]
questionSentences = filter isQuestion

-- | Unique (case-insensitive) words of the given length, in order of
-- first occurrence.
wordsOfLength :: Int -> [Sentence] -> [Word']
wordsOfLength n =
  nubBy sameWord . filter ((== n) . wordLength) . concatMap sentenceWords
