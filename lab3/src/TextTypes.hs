-- | The domain types required by the assignment:
-- Symbol, Word, Punctuation mark, Sentence.
module TextTypes
  ( Symbol(..)
  , apostrophes
  , classify
  , Word'(..)
  , wordText
  , wordLength
  , sameWord
  , Token(..)
  , Sentence(..)
  , isQuestion
  , sentenceWords
  , renderSentence
  ) where

import           Data.Char (isAlpha, isDigit, toLower)

-- | A classified character of the source text.
data Symbol
  = Letter !Char   -- ^ a letter (any alphabet, including Ukrainian)
  | Digit  !Char   -- ^ a decimal digit
  | Mark   !Char   -- ^ a punctuation mark
  | Gap            -- ^ whitespace (already normalized to one space)
  deriving (Eq, Show)

-- | Apostrophe variants seen in Ukrainian text: straight ('), right
-- single quote (’) and modifier letter apostrophe (ʼ, the one actually
-- produced by Ukrainian keyboards). They belong to a word but are
-- classified as marks, so all three behave the same way.
apostrophes :: String
apostrophes = "'\x2019\x02BC"

classify :: Char -> Symbol
classify c
  | c `elem` apostrophes = Mark c   -- before isAlpha: U+02BC is a letter
  | isAlpha c = Letter c
  | isDigit c = Digit c
  | c == ' '  = Gap
  | otherwise = Mark c

symbolChar :: Symbol -> Char
symbolChar (Letter c) = c
symbolChar (Digit c)  = c
symbolChar (Mark c)   = c
symbolChar Gap        = ' '

-- | A word: a run of letters/digits, possibly with inner apostrophes
-- or hyphens (обʼєкт, будь-який) — those count towards its length.
newtype Word' = Word' [Symbol]
  deriving (Eq, Show)

wordText :: Word' -> String
wordText (Word' ss) = map symbolChar ss

wordLength :: Word' -> Int
wordLength (Word' ss) = length ss

-- | Case-insensitive equality — "without repetitions" should treat
-- «Що» and «що» as the same word.
sameWord :: Word' -> Word' -> Bool
sameWord a b = key a == key b
  where key = map toLower . wordText

-- | One token of a sentence: a word or a standalone punctuation mark.
data Token = TWord Word' | TMark Char
  deriving (Eq, Show)

-- | A sentence: its tokens plus the end mark ('.', '?' or '!').
data Sentence = Sentence
  { sentenceTokens :: [Token]
  , sentenceEnd    :: Char
  } deriving (Eq, Show)

isQuestion :: Sentence -> Bool
isQuestion s = sentenceEnd s == '?'

sentenceWords :: Sentence -> [Word']
sentenceWords s = [w | TWord w <- sentenceTokens s]

-- | Render with normalized spacing: one space between tokens, no space
-- before a punctuation mark, the end mark attached at the close.
renderSentence :: Sentence -> String
renderSentence (Sentence ts end) = go ts ++ [end]
  where
    go []               = ""
    go [t]              = tok t
    go (t : rest@(r:_)) = tok t ++ (if isMark r then "" else " ") ++ go rest
    tok (TWord w) = wordText w
    tok (TMark m) = [m]
    isMark (TMark _) = True
    isMark _         = False
