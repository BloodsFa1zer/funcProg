# Lab 3, variant 4 — words of a given length in question sentences

Processes a programming-textbook text file using the domain types the
assignment requires (Symbol, Word, Sentence, Punctuation mark) and, for
**variant 4**: in all question sentences of the text, finds and prints
without repetitions the words of a given length.

## Structure

- `src/TextTypes.hs` — the domain types: `Symbol` (Letter / Digit /
  Mark / Gap), `Word'` (a list of symbols; inner apostrophes and hyphens
  belong to the word and count towards its length), `Token`, `Sentence`
  (tokens + end mark). Word comparison is case-insensitive, so «Що» and
  «що» are one word.
- `src/Parser.hs` — normalization (tabs and space runs collapse into one
  space, as the assignment demands: `unwords . words`) and parsing into
  sentences by `.`/`?`/`!`; runs like «?!» or «...» classify the
  sentence by their first mark.
- `src/Task.hs` — the variant-4 logic: filter question sentences, pick
  words of length N, deduplicate preserving first-occurrence order.
- `app/Main.hs` — CLI; `textbook.txt` — sample text with questions,
  tabs, space runs, apostrophe and hyphen words.

## Usage

```sh
cabal build
lab3 textbook.txt 7      # words of length 7
lab3 textbook.txt        # length is asked interactively
```

Sample output (length 7):

```
Речень у тексті: 14, з них питальних: 6
...
Слова довжини 7 (без повторень, у порядку появи):
  - Haskell
  - лінивою
  - функція
  ...
```

## Interpretation choices

- **"слова" (words):** any run of letters/digits is a word, so a numeric
  token like «2010» counts as a word of length 4. Inner apostrophes and
  hyphens count towards the length.
- **"без повторень" (without repetitions):** case-insensitive, so «Що»
  and «що» are one word; the first occurrence's casing is printed.
- **Sentence boundaries** are `.`/`?`/`!`; a `.` between two digits is
  treated as a decimal point, not a boundary. A sentence counts as a
  question when its end-mark run contains `?` (so both «?!» and «!?»).

## Known limitations

- **Abbreviations** with an internal period («т.д.») are split into
  separate sentences — naive boundary detection, out of the variant's
  scope. Decimals are handled; dotted abbreviations are not.
- **A `?` inside parentheses or quotes** («(чому?)») is treated as a
  sentence end. Distinguishing it needs bracket/quote tracking, which
  the variant does not require.
- **`renderSentence`** is display-only (the variant needs no text
  reconstruction): paired quotes are re-attached to the preceding token,
  so «термін "рекурсія"» renders as «термін" рекурсія"». Word extraction
  is unaffected.
