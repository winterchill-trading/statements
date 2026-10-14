# AGENTS.md

Guide for AI coding agents working in this repository. Read this first; the subdirectory
`AGENTS.md` files add detail for `lib/`, `bin/`, `test/`, `examples/` and `.github/`.
The workspace-wide guide is `../oxcaml-claude-skills/AGENTS.md`.

## Purpose

`statements` turns the journal of `../ledger` into customer-facing account statements:
one account and currency over a month or up to twelve months of UTC days, netted or
posting basis, running balances and checked identities. It renders them as RFC 4180
CSV and as PDF in six languages, with its own small PDF writer and a bundled font.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The library, wrapped as `Statements` (`Period`, `Statement`, `Catalogue`, `Csv`, `Pdf`, PDF toolkit) |
| `lib/gen/` | `embed.exe`, which turns the bundled font into an OCaml string at build time |
| `fonts/` | Noto Sans Regular (unmodified), its SIL Open Font License, a README with the checksum |
| `bin/` | The `statements` command: `demo` (sample account) and `journal` (a journal file) |
| `examples/` | `quickstart.ml`, built and run in CI |
| `test/` | Expect tests, Quickcheck properties, `cli.t` cram test |
| `scripts/check-pdf.sh` | Renders the demo in every language and checks it with `qpdf` and `pdftotext` |
| `doc/index.mld` | odoc landing page |
| `dune-project` | Package metadata; `dune build` regenerates `statements.opam` |

## Build, test, docs

Inside the workspace (sibling checkouts and the shared `5.2.0+ox` switch):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @statements/all
opam exec --switch 5.2.0+ox -- dune build --root .. @statements/runtest
opam exec --switch 5.2.0+ox -- dune build --root .. @statements/doc
scripts/check-pdf.sh ../_build/default/statements/bin/main.exe
```

Parallel work: prefix with `DUNE_BUILD_DIR=_build-statements` (resolved from the
workspace root because of `--root ..`), and point `check-pdf.sh` at
`../_build-statements/default/statements/bin/main.exe`.

Standalone, once the dependencies are published:

```sh
opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git
opam install --deps-only --with-test .
dune build && dune test
```

## Conventions

- Every module has an `.mli` with odoc on every public value; `open! Core`, labelled
  arguments, ppx_jane derivations.
- Stable types (`Locale`, `Basis`, `Period`, `Annotation`, `Statement.Totals`,
  `Statement`) have `Stable.V1` with digests pinned in `test/test_journal_file.ml`. A
  changed wire shape must fail that test; add a `V2` instead of editing `V1`.
- Times in record fields are `Time_ns.Alternate_sexp.t` (and
  `Time_ns.Stable.Alternate_sexp.V1.t` in stable types) so sexps print in UTC on every
  machine.
- `Statement.create` must keep checking `Statement.check`; a statement that disagrees
  with the ledger is an error, never a value.
- Tests: expect tests and Quickcheck (`Quickcheck.test` inside `let%test_unit`).
  Promote one file at a time with `dune promote --root .. <file>`.
- Formatting: `opam exec --switch 5.2.0+ox -- dune build --root .. @statements/fmt --auto-promote`,
  after tests pass and promotion is done. Never run `dune fmt` at the workspace root.
- Comments for humans at non-obvious choices (font metrics provenance, CLDR deviations).
- `CHANGES.md`: a line under `## Unreleased` for every user-visible change.
- Catalogue texts: one row per label with six columns in `lib/catalogue.ml`; a new
  label needs all six, and templates keep their `{placeholders}`.

## Dependencies and dependents

- Depends on `money` and `ledger` (and through `money` on `fixed-decimal`); external:
  `core`, `zarith`, `ppx_jane`; `core_unix` for the command only.
- CI pins `fixed-decimal`, `money`, `ledger` from GitHub in that order.
- No org repository depends on it yet.

## CI

`.github/workflows/ci.yml`: the infra `ox-setup` action (cached switch, sibling pins),
`dune build`, `dune test`, the quickstart example, `scripts/check-pdf.sh` with `qpdf`
and `poppler-utils` installed from apt, an odoc build in which any warning fails, and
the shared `obj-magic` workflow. To check a run: `gh run list --limit 1`, then at most
one `gh run watch <id> --exit-status`. Do not poll.

## Pitfalls

- `Time_ns.t` sexps print in the machine's zone; use the `Alternate_sexp` types in
  records and `Time_ns.to_string_utc` in tests.
- ledger refuses records whose `recorded_at` goes backwards: test journals must be built
  in time order, including entries for other accounts.
- The statement selects by business date, not effective time; a backdated entry appears
  in the period it was booked in.
- odoc rejects `[\\]` inside a code span ("End of text is not allowed"); describe such
  characters in words. A `{v ... v}` block must be indented at its brace's column.
- The `fonts/` file is compiled into the library by a dune rule; replacing it changes
  every embedded PDF and the checksum in `fonts/README.md`.
- PDF xref entries are exactly 20 bytes (`%010d 00000 n \n`); `test/test_pdf.ml`
  checks offsets and stream lengths, keep it passing after any writer change.
- `Uchar`-level code uses `Stdlib.String.get_utf_8_uchar`; Core's `String` has no UTF-8
  iterator of that shape.

## Where to read more

- `README.md`: semantics, CSV columns, fonts and licence, known gaps.
- `../specs/repos/statements.md` (when written) and `../ledger/README.md`.
- `../oxcaml-claude-skills/skills/banking-standards/` and `janestreet-ocaml-style/`.
- `../oxcaml-claude-skills/GOTCHAS.md`, section "Product libraries".

## Never do

- Install or upgrade opam packages that change the compiler, `core`, `base`, `async` or
  `ppx_jane` on the shared switch; dry-run first and stop if the plan touches them.
- Fetch fonts or other assets at build time, or bundle a font whose licence does not
  allow redistribution.
- Publish, tag or release; commit credentials; print secrets; poll CI in a loop.
