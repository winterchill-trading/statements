(lang dune 3.17)

(name statements)

(generate_opam_files true)

(license MIT)

(authors "WinterChill AI")

(maintainers "hello@winterchill.xyz")

(source
 (github winterchill-trading/statements))

(homepage "https://github.com/winterchill-trading/statements")

(bug_reports "https://github.com/winterchill-trading/statements/issues")

(documentation "https://github.com/winterchill-trading/statements#readme")

(package
 (name statements)
 (synopsis
  "Customer-facing bank account statements over a double-entry ledger, with localised CSV and PDF")
 (description
  "statements turns the journal of a double-entry ledger into the statement a bank sends its customer: one account in one currency over a calendar month or a range of whole UTC days of up to twelve months, on one of two presentation bases (card and payment holds netted, with open holds listed apart from the balances, or every posting including hold placements), with running balances and the identity opening + money in - money out = closing checked against the ledger's own balances. Statements render as CSV (RFC 4180, UTF-8 with a byte order mark, English headers and ISO values) and as PDF 1.4 written by a small built-in writer, with labels, numbers and dates localised from a catalogue (English, German, French, Spanish, Russian and Kyrgyz built in). Latin text uses the standard PDF fonts; Cyrillic text embeds a subset of a bundled Noto Sans (SIL Open Font License 1.1). Pure: the library never persists anything. Requires the OxCaml compiler (through its money dependency).")
 (tags
  (statements banking ledger csv pdf localisation oxcaml))
 (depends
  oxcaml
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.3~)))
  dune
  (core
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (core_unix
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (ppx_jane
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  zarith
  money
  ledger
  (expect_test_helpers_core
   (and
    :with-test
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (odoc :with-doc)))
