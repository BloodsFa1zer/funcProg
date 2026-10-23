# Lab 2, variant 1 — Multithreaded Huffman archiver

Compresses a set of files into one archive using Huffman coding, in
parallel (GHC `-threaded`), and measures the speedup on a multiprocessor
system.

## Structure

- `src/Huffman.hs` — pure Huffman coding: frequency table, deterministic
  tree construction, bit packing/unpacking. Only the frequency table is
  stored in the archive; the decoder rebuilds the identical tree.
- `src/Archive.hs` — archive format (`Data.Binary`): magic + list of
  entries, each with its own frequency table. `readArchive` is the trust
  boundary: it validates entry names and bit lengths, so corrupted or
  crafted archives fail with a readable message instead of a crash.
- `src/Pool.hs` — worker pool on plain `forkIO` threads: at most
  min(k, jobs) workers pop jobs from a shared `MVar` queue; results keep
  input order and are forced inside the worker; exceptions are re-thrown
  in the caller.
- `app/Main.hs` — CLI: `pack` / `unpack` / `bench`.

## Parallelism model

Each file is compressed independently by a worker thread (per-file
Huffman tree), so compression of N files scales across cores without
any shared mutable state. `Entry`/`Compressed` have strict fields and
the pool forces each result to WHNF inside the worker — otherwise lazy
evaluation would move the real work back to the single writer thread
and retain every original file in memory until the archive is written.

## Usage

```sh
cabal build
lab2 pack   archive.huf file1 file2 ... [-j N]   # compress
lab2 unpack archive.huf outdir          [-j N]   # decompress
lab2 bench  file1 file2 ...             [-j N]   # speedup table
```

`N` defaults to the number of cores (`+RTS -N` is baked in, along with
`-A128m` — the encoder allocates heavily and a larger allocation area
keeps parallel GC from dominating at high thread counts).

## Measured results (Apple Silicon, 18 cores, GHC 9.14.1)

16 equal text files, 304 MB total, data preloaded to RAM (CPU-only),
one untimed warm-up pass before measuring:

| threads | time, s | MB/s  | speedup |
|--------:|--------:|------:|--------:|
| 1       | 5.13    | 59.3  | 1.00x   |
| 2       | 2.47    | 123.3 | 2.08x   |
| 4       | 1.55    | 196.8 | 3.32x   |
| 8       | 1.00    | 304.4 | 5.13x   |
| 16      | 0.90    | 336.7 | 5.67x   |
| 18      | 0.86    | 355.5 | 5.99x   |

Correctness: `pack` + `unpack` of 5 mixed files (219 MB text + random
binary) is byte-identical to the originals (`cmp`); English text
compresses to ~54.5%, uniformly random data stays at 100% as the theory
predicts.

Notes for the defense:

- Speedup is bounded by the largest file (per-file granularity): with 5
  files of 20–80 MB the ceiling is total/largest ≈ 2.75x; with 16 equal
  files scaling is monotonic all the way to 18 threads.
- Scaling flattens past 8 threads because Apple Silicon mixes fast
  performance cores with slow efficiency cores and the encoder is
  memory-bandwidth hungry — extra slow cores add little throughput.
- The GC allocation area is raised to `-A128m` in the cabal file — with
  the default 4 MB nursery, parallel GC synchronization dominated and
  capped the speedup near 4.5x.
- The untimed warm-up pass matters: without it the k=1 baseline runs on
  a cold chip and the later rows absorb thermal throttling, making the
  numbers jump between runs.
