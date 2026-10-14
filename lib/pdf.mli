(** A statement as a PDF document (A4, PDF 1.4), in the language of a {!Catalogue}.

    Page one starts with the title, the account and the period, then a summary box with
    the opening balance, money in, money out and the closing balance (and, on the netted
    basis, the amount held for pending payments and the available balance at the end), a
    sentence explaining the basis, and the transactions: date, description (with the
    transaction type, reference, authorisation date and reversal note on a second, smaller
    line), money in, money out and the running balance. Holds still pending follow in
    their own table. The table header repeats on every page, and every page has a footer
    with the page number and, by default, a mark saying the document is not a real bank
    statement.

    Latin text uses the standard Helvetica fonts. If any text needs other characters
    (Russian and Kyrgyz labels, a Cyrillic payee), the bundled Noto Sans is embedded as a
    subset instead; scripts outside Latin, Greek and Cyrillic are not covered. *)

open! Core

module Header : sig
  (** What the statement says about the account and its holder, beyond the ledger's ids:
      the ledger stores no personal data. *)
  type t =
    { institution : string option (** the issuer's name, top right *)
    ; account_name : string (** e.g. ["Main account"] *)
    ; holder : string option (** the customer's name *)
    ; details : (string * string) list (** label and value: [("IBAN", "DE89 ...")] *)
    }
  [@@deriving sexp, compare, equal]

  val create
    :  ?institution:string
    -> ?holder:string
    -> ?details:(string * string) list
    -> account_name:string
    -> unit
    -> t
end

module Mark : sig
  (** The footer line on every page. *)
  type t =
    | Catalogue (** the catalogue's "not a real statement" text (the default) *)
    | Text of string
    | No_mark
  [@@deriving sexp, compare, equal]
end

module Font_choice : sig
  type t =
    | Auto (** standard fonts when the text allows, else the embedded font *)
    | Embedded (** always embed (larger file, same look in every language) *)
  [@@deriving sexp, compare, equal]
end

(** The PDF file. [catalogue] defaults to English. *)
val render
  :  ?catalogue:Catalogue.t
  -> ?mark:Mark.t
  -> ?font:Font_choice.t
  -> header:Header.t
  -> Statement.t
  -> string
