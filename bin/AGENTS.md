# bin/

The `statements` command (`main.ml`, Core `Command`):

- `statements demo [-locale L] [-basis netted|posting] [-format pdf|csv|sexp] [-preamble] [-output FILE]`
  renders September 2026 of the sample account in `demo.ml` (built with ledger's
  `Hold` and `Entry` builders; entries must stay in time order).
- `statements journal FILE -account ID -currency CODE [-month YYYY-MM | -from D -to D]`
  renders a statement from a ledger journal file.

`../test/cli.t/run.t` is the cram test; `../scripts/check-pdf.sh` uses `demo` for every
language. Changing the demo data changes both: promote the cram output and rerun the
script.
