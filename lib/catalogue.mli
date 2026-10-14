(** The words and formats a rendered statement uses, per {!Locale}.

    A catalogue is data: a text for every {!Label}, a number format, a date format and the
    month names. {!builtin} has the six built-in languages; a bank with its own wording
    replaces texts with {!with_text} or builds a whole catalogue with {!create}, and both
    check that every label is present and every template keeps its placeholders. Templates
    name their parameters in braces, e.g. ["Page {page} of {pages}"].

    Only rendering is localised. Amounts, dates and codes in CSV files stay in ISO and
    English forms so programs can read them (see {!Csv}). *)

open! Core

module Label : sig
  type t =
    | Statement_title
    | Account
    | Currency
    | Period
    | As_of
    | Summary
    | Opening_balance
    | Money_in
    | Money_out
    | Closing_balance
    | Held_for_pending
    | Available_at_end
    | Transactions
    | No_transactions
    | Pending_title
    | Date
    | Description
    | Reference
    | Amount
    | Balance
    | Reversal_note
    | Authorised_on (** template: [{date}] *)
    | Page (** template: [{page}], [{pages}] *)
    | Not_a_real_statement
    | Basis_netted (** explains {!Basis.Netted} *)
    | Basis_posting (** explains {!Basis.Posting} *)
    | Code_deposit
    | Code_withdrawal
    | Code_transfer
    | Code_fx
    | Code_fee
    | Code_card_auth
    | Code_card_settlement
    | Code_refund
    | Code_reversal
    | Code_return
    | Code_recall
    | Code_adjustment
    | Code_interest
    | Code_safeguarding
    | Code_hold
  [@@deriving sexp, compare, equal, hash, enumerate]

  (** The label naming a ledger transaction code. *)
  val of_code : Ledger.Entry.Code.t -> t

  (** The placeholders a template for this label must contain, e.g. [["page"; "pages"]]. *)
  val placeholders : t -> string list

  include Comparator.S with type t := t
end

module Number_format : sig
  type t =
    { decimal_separator : string
    ; group_separator : string (** between groups of three integer digits *)
    ; min_grouping_digits : int
    (** 1: group from four integer digits on (1,234); 2: from five (1234 but 12.345), as
        Spanish does *)
    ; currency_first : bool (** ["EUR 1,234.56"] rather than ["1.234,56 EUR"] *)
    }
  [@@deriving sexp, compare, equal]
end

module Date_format : sig
  type t =
    { pattern : string
    (** [dd] day with two digits, [d] without padding, [MM] month number, [MMM] short
        month name, [yyyy] year; anything else is copied *)
    ; short_months : string list (** twelve names, used by [MMM] *)
    }
  [@@deriving sexp, compare, equal]
end

type t [@@deriving sexp_of]

(** Fails unless every label has a text with exactly its placeholders, there are twelve
    month names (and twelve short ones), and [month_title] contains [{month}] and
    [{year}]. *)
val create
  :  locale:Locale.t
  -> texts:(Label.t * string) list
  -> number:Number_format.t
  -> date:Date_format.t
  -> months:string list (** standalone names, January first *)
  -> month_title:string (** e.g. ["{month} {year}"] *)
  -> t Or_error.t

(** The built-in catalogue of a language. *)
val builtin : Locale.t -> t

val locale : t -> Locale.t

(** Replaces one text, checking its placeholders. *)
val with_text : t -> Label.t -> string -> t Or_error.t

(** The text of a label, placeholders unfilled. *)
val text : t -> Label.t -> string

(** The text with each [{name}] replaced by its value. *)
val render : t -> Label.t -> (string * string) list -> string

(** The label of a ledger transaction code. *)
val code_label : t -> Ledger.Entry.Code.t -> string

(** An amount with grouping and the locale's separators: ["1,234.56"], ["1.234,56"],
    ["-12,00"]. With [~currency:true], the currency code is added on the locale's side. *)
val format_amount : ?currency:bool -> t -> Money.Amount.t -> string

val format_date : t -> Date.t -> string

(** The date and the UTC time: ["14 Oct 2026, 09:30 UTC"]. *)
val format_time : t -> Time_ns.t -> string

(** A month by name (["October 2026"], ["octubre de 2026"]), a day range as two dates
    joined by an en dash. *)
val format_period : t -> Period.t -> string
