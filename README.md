# statements

Customer-facing account statements over the double-entry
[`ledger`](https://github.com/winterchill-trading/ledger): one account in one currency
over a calendar month or up to twelve months of whole UTC days, with running balances,
two ways of presenting card and payment holds, and rendering as CSV and PDF in six
languages.

## Why separate

A ledger knows postings, not what a customer should read. Banks have to decide how a
card authorisation that later settles for a different amount appears, which balance a
statement closes on when funds are still reserved, how a request for "March" or "the
last 90 days" is validated, and how the result looks in the customer's language and
number format. Those choices are presentation, they change more often than booking
rules, and they are the same for every bank that keeps books in `ledger`, so they live
in their own library. It is pure: it reads journal records and returns values and
bytes; it never stores anything.

## Quick start

```ocaml
let statement =
  Statements.Statement.create
    records                      (* Ledger.Record.t list, from a journal *)
    ~account:(Ledger.Account_id.of_string "customer:42")
    ~currency:Money.Currency.eur
    ~period:(Statements.Period.of_month { year = 2026; month = Oct })
    ~basis:Netted
    ~now:(Time_ns.now ())
  |> Or_error.ok_exn
in
let csv = Statements.Csv.render statement in
let pdf =
  Statements.Pdf.render
    ~catalogue:(Statements.Catalogue.builtin Ru)
    ~header:(Statements.Pdf.Header.create ~account_name:"Main account" ())
    statement
```

`examples/quickstart.ml` is a complete program (built and run in CI). The command line:

```sh
statements demo -locale de -output statement.pdf        # a built-in sample account
statements demo -format csv -preamble -locale fr        # CSV with a localised summary
statements journal ledger.journal -account customer:42 -currency EUR -month 2026-09
```

## What a statement is

| Piece | Meaning |
| --- | --- |
| Period | A calendar month or whole UTC days, at most 12 months (the last day must be before the same day twelve months on). Entries are selected by their ledger business date, which never goes backwards in a journal, so consecutive periods split it exactly. |
| Opening, closing | The balance after every entry dated before the period, and after every entry dated in it, on the account's normal side. |
| Lines | One per entry that changes the balance, in journal order, each with its running balance. |
| Netted basis | The customer view. Placing, increasing, releasing or expiring a hold is not a line; a settlement is one line for the amount that left, dated when it settled and linked to its hold (with the authorisation time). Holds still open at the end are listed as pending, outside the balances. |
| Posting basis | The book view for businesses and auditors. Placing a hold is money out when placed, a release is money in, a capture moves nothing further; nothing is pending. |
| Identities | `opening + money in - money out = closing`; every running balance follows from the previous one; on the netted basis `closing + held_for_pending` is the ledger's available balance at the end, on the posting basis `closing` is. `Statement.create` checks them against the ledger's own balances and fails rather than return a statement that disagrees with the books. |

Descriptions come from the caller: the ledger stores ids, not names, so `~annotate`
maps each entry to a description, counterparty and category. A settlement without text
of its own takes the description of the authorisation it settles.

`Period.parse` validates a request (`month=YYYY-MM`, or `from` and `to` as
`YYYY-MM-DD`, not both) and returns every problem with its field and an upper-case code
(`CONFLICT`, `REQUIRED`, `INVALID`, `FUTURE`, `OUT_OF_RANGE`, `BEFORE_FROM`, `TOO_LONG`),
ready to become a validation error in an API.

## Languages

English, German, French, Spanish, Russian and Kyrgyz are built in. A `Catalogue` is
data: a text for every label (titles, column names, transaction types, the footer
mark), a number format (separators, Spanish-style grouping from five digits, currency
code before or after), a date pattern and month names. `Catalogue.with_text` replaces
one text and `Catalogue.create` builds a whole catalogue; both check that every label is
present and that templates such as `Page {page} of {pages}` keep their placeholders.
Only rendering is localised; CSV values stay language-neutral.

## CSV

RFC 4180 (CRLF, quoting, doubled quotes), UTF-8 with a byte order mark, English headers:

`entry_ref, booking_date, value_time, type, status, direction, amount, currency,
balance, reversal, reference, description, counterparty, category, hold, authorised_at`

Dates are ISO 8601, times UTC, amounts signed decimals with a point and no grouping,
types the ledger's codes (`CARD_SETTLEMENT`). Booked lines come first, then pending holds
(`PENDING`, no balance). Text from people or merchants that starts with `=`, `+`, `-`,
`@`, tab or CR gets a leading `'` so a spreadsheet does not run it as a formula. An
optional localised preamble (title, account, period, totals) is padded to the same
number of fields.

## PDF

A4, PDF 1.4, written by the library's own small writer (objects, cross-reference table,
fonts); output is deterministic. Each page has a footer with the page number and, by
default, the catalogue's "not a real statement" mark (`~mark:(Text ...)` or `No_mark`
change it).

Fonts: when every string fits WinAnsiEncoding, the document uses the standard Helvetica
fonts and embeds nothing (about 5 KB). Otherwise (Russian or Kyrgyz labels, a Cyrillic
payee name) it embeds a subset of the bundled **Noto Sans Regular 2.015**, licensed
under the **SIL Open Font License 1.1** (`fonts/OFL.txt`), which permits bundling and
redistribution with software and declares no Reserved Font Name. The font file is
committed unmodified and compiled into the library; nothing is downloaded at build
time. The subset keeps glyph ids, carries a `ToUnicode` map so text can be searched and
copied, and costs about 80 KB per document.

Why no PDF library: `opam install --dry-run camlpdf` on the shared switch plans exactly
one package (camlpdf 2.9) and changes nothing else, so it could be used. It was not
needed: writing a statement takes a few hundred lines (objects, xref, a TrueType subset),
which keeps the dependency list to `money` and `ledger` and the output under our control.

Validation: the tests check the file structure (every cross-reference entry points at
its object, stream lengths are exact, the trailer is right). CI also renders the demo
in every language and on both bases and runs `qpdf --check` and `pdftotext` on each
file (`scripts/check-pdf.sh`).

## Modules

| Module | Purpose |
| --- | --- |
| `Period` | Months and day ranges in UTC; request validation |
| `Basis` | Netted or posting presentation |
| `Annotation` | Caller-supplied description, counterparty, category |
| `Statement` | The statement, `Builder` for streaming, `check`, file names, stable form |
| `Source` | Statements from a journal alone, or from a journal next to transfer records for systems that reserve money by transfer instead of ledger holds |
| `Locale`, `Catalogue` | Languages, labels, number and date formats |
| `Csv` | RFC 4180 rendering |
| `Pdf` | The statement document |
| `Pdf_writer`, `Pdf_font`, `Truetype`, `Win_ansi` | The PDF toolkit underneath, reusable for receipts |

Stable types (`Locale`, `Basis`, `Period`, `Annotation`, `Statement.Totals`,
`Statement`) have `Stable.V1` bin_io with pinned digests; decoding a statement runs
`check` again.

## Verified

- Expect tests for every module, including a month with every kind of line on both
  bases, statements streamed from a real journal file, localised formats in all six
  languages, CSV output and PDF structure.
- Quickcheck properties over random journals (deposits, payments out, authorisations
  settled in part or in full, cancelled or left open, reversals): netted lines equal
  the booked entries of the ledger's own camt.053 statement; consecutive months chain
  (closing of one is the opening of the next) and two months equal their union; both
  bases end on the same available balance; `check` rejects a tampered statement; CSV
  fields round-trip through an RFC 4180 reader.

## Known gaps

- Streams are stored uncompressed; a Latin statement is a few KB, one with the
  embedded font about 80 KB.
- The embedded font has no bold face; bold text is drawn with an outline. Scripts other
  than Latin, Greek and Cyrillic, right-to-left text and kerning are not supported.
- The Kyrgyz and Russian texts (and the others) should be reviewed by native speakers
  before customers see them; `Catalogue.with_text` replaces any of them.
- One account and one currency per statement; a multi-currency or multi-account pack
  is several statements.
- No tagged PDF (accessibility) or PDF/A.

## Docs

- `AGENTS.md`: guide for coding agents working in this repository.
- API reference: `dune build @doc`.

## License

MIT (`LICENSE`). The bundled font is under the SIL Open Font License 1.1
(`fonts/OFL.txt`).
