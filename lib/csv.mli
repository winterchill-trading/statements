(** A statement as CSV, for spreadsheets and accounting tools.

    The file follows RFC 4180: records end with CRLF, a field is quoted when it contains a
    comma, a double quote, CR or LF, and quotes inside are doubled. It starts with a UTF-8
    byte order mark so spreadsheet programs read non-ASCII text correctly.

    Headers and values are language-neutral: English column names, ISO 8601 dates and UTC
    times, amounts as plain signed decimals with a point and no grouping, and the ledger's
    upper-case transaction codes. Booked lines come first in journal order, then pending
    holds (status [PENDING], no balance).

    Text written by people or merchants (descriptions, references, names) is a
    spreadsheet-formula risk: a field starting with [=], [+], [-], [@], tab or CR is
    prefixed with a single quote, the usual defence against CSV injection. Numeric columns
    are never altered. *)

open! Core

(** The header row: [entry_ref], [booking_date], [value_time], [type], [status],
    [direction], [amount], [currency], [balance], [reversal], [reference], [description],
    [counterparty], [category], [hold], [authorised_at]. *)
val columns : string list

(** The whole file. With [~preamble], a localised block of rows comes before the header
    (title, account, currency, period, and the totals), each padded to the number of
    columns so the file stays rectangular; readers that expect the header on the first row
    should not ask for it. *)
val render : ?preamble:Catalogue.t -> Statement.t -> string

(** One field, quoted if needed (no formula guard). *)
val field : string -> string

(** One record from its fields, with the CRLF. *)
val record : string list -> string
