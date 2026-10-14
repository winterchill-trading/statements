# lib/

The `statements` library, wrapped as `Statements`.

- Domain: `period.ml`, `basis.ml`, `annotation.ml`, `statement.ml` (`Builder` folds
  records with `Ledger.Balances.apply` and stops at the first record dated after the
  period).
- Localisation: `locale.ml`, `catalogue.ml` (texts table, number and date formats).
- Rendering: `csv.ml`, `pdf.ml` (layout), `pdf_writer.ml` (objects, xref),
  `pdf_font.ml` (standard or embedded font), `truetype.ml` (parse, subset),
  `win_ansi.ml`.
- Private: `helvetica_widths.ml` (metric table, provenance in its comment), `utf8.ml`,
  `noto_sans_data.ml` (generated from `../fonts/` by `gen/embed.exe`; only its `.mli`
  is in git).

Rules: every module has an `.mli` with odoc; the library never does I/O except
`Statement.of_journal_file` (through `Ledger.Journal.fold`); output must stay
deterministic (no clock reads, no random ids).

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @statements/all`.
