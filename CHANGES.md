# Changes

All notable changes to this library. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/); versions follow semantic
versioning once released.

## Unreleased

### Added

- `Period`: calendar months and UTC day ranges of up to twelve months, and validation
  of statement requests with per-field problem codes.
- `Basis` (netted or posting), `Annotation` (caller-supplied descriptions).
- `Statement`: built from ledger records in memory, incrementally (`Builder`) or from a
  journal file; running balances, pending holds, identities checked against the
  ledger; `Stable.V1` that checks again on decoding.
- `Locale` and `Catalogue`: labels, number and date formats for English, German,
  French, Spanish, Russian and Kyrgyz, with checked overrides.
- `Csv`: RFC 4180 with a byte order mark, English headers, a formula guard and an
  optional localised preamble.
- `Pdf`: A4 statements with paging, repeated table headers and a footer mark;
  `Pdf_writer`, `Pdf_font`, `Truetype` (subsetting) and `Win_ansi` underneath; the
  bundled Noto Sans for Cyrillic text.
- `statements` command (`demo`, `journal`) and a quickstart example run in CI.
