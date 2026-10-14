# test/

Inline expect tests (`statements_test` library) and a cram test of the command.

- `helpers.ml`: a test journal builder (accounts, deposits, holds, settlements,
  reversals) and the September fixture used across files.
- `test_statement.ml`, `test_properties.ml` (Quickcheck: camt.053 agreement, month
  chaining, both bases, tamper detection), `test_journal_file.ml` (streamed journal,
  pinned stable digests), `test_period.ml`, `test_catalogue.ml`, `test_csv.ml`,
  `test_truetype.ml`, `test_pdf.ml` (xref and stream structure), `cli.t/run.t`.

Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @statements/runtest`.
Promote one file at a time: `dune promote --root .. statements/test/<file>`. If a test
raised, delete any `[@@expect.uncaught_exn]` block promotion wrote before rerunning.
