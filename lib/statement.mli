(** A customer-facing statement of one account in one currency over a {!Period}.

    Built from the records of a [ledger] journal: in memory with {!create}, or streamed
    from a journal file with {!of_journal_file}. Entries are selected by business date,
    which the ledger keeps in journal order, so the opening balance is the balance after
    every record dated before the period and the closing balance the balance after every
    record dated in it. Lines are in journal order with a running balance on the account's
    normal side (money in raises a customer account's balance).

    The {!Basis} decides whether holds are netted (a settlement is one line, open holds
    are pending and outside the balances) or shown posting by posting.

    {!create} checks the identities of {!check} before returning, against the ledger's own
    balances at the end of the period, so a statement that comes back is consistent with
    the books. *)

open! Core

module Direction : sig
  type t =
    | In
    (** raises the balance: a deposit, a refund, a released hold on the posting basis *)
    | Out
  [@@deriving sexp, compare, equal, hash, enumerate]

  (** ["IN"], ["OUT"]. *)
  val to_string : t -> string
end

module Line : sig
  type t =
    { seq : int (** the journal record *)
    ; index : int (** the entry within the record (several for linked entries) *)
    ; business_date : Date.t (** the booking date *)
    ; effective_at : Time_ns.Alternate_sexp.t (** the value time *)
    ; code : Ledger.Entry.Code.t
    ; direction : Direction.t
    ; amount : Money.Amount.t (** positive; [direction] says which way *)
    ; balance : Money.Amount.t (** running balance after this line *)
    ; reference : string
    ; idempotency_key : Ledger.Idempotency_key.t
    ; reversal : bool (** the entry undoes an earlier one *)
    ; hold : Ledger.Hold_id.t option (** the hold this entry acts on *)
    ; held_since : Time_ns.Alternate_sexp.t option
    (** on the netted basis, when the settled hold was placed (the authorisation time) *)
    ; annotation : Annotation.t
    }
  [@@deriving sexp_of, compare, equal]

  (** [amount] for money in, minus [amount] for money out. *)
  val signed : t -> Money.Amount.t

  (** ["<seq>.<index>"], the same entry reference camt.053 uses. *)
  val entry_ref : t -> string
end

module Pending : sig
  (** A hold still open at the end of the period (netted basis only). *)
  type t =
    { hold : Ledger.Hold_id.t
    ; placed_at : Time_ns.Alternate_sexp.t (** the placing entry's value time *)
    ; business_date : Date.t (** the placing entry's booking date *)
    ; amount : Money.Amount.t (** what remains held; positive *)
    ; expires_at : Time_ns.Alternate_sexp.t option
    ; code : Ledger.Entry.Code.t
    ; reference : string
    ; idempotency_key : Ledger.Idempotency_key.t
    ; annotation : Annotation.t
    }
  [@@deriving sexp_of, compare, equal]
end

module Totals : sig
  type t =
    { opening : Money.Amount.t
    ; money_in : Money.Amount.t (** sum of the [In] lines; never negative *)
    ; money_out : Money.Amount.t (** sum of the [Out] lines; never negative *)
    ; closing : Money.Amount.t
    ; lines : int
    }
  [@@deriving sexp_of, compare, equal]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
    end
  end
end

type t =
  { account : Ledger.Account_id.t
  ; currency : Money.Currency.t
  ; basis : Basis.t
  ; period : Period.t
  ; as_of : Time_ns.Alternate_sexp.t
  (** the end of the period, or [now] when the period has not ended yet *)
  ; totals : Totals.t
  ; lines : Line.t list (** journal order *)
  ; pending : Pending.t list (** by hold id; empty on the posting basis *)
  ; held_for_pending : Money.Amount.t (** minus the sum of [pending]: zero or negative *)
  ; ledger_balance_at_end : Money.Amount.t
  (** the ledger's available balance of the account at the end of the period:
      [closing + held_for_pending] *)
  }
[@@deriving sexp_of, compare, equal]

(** The statement of [account] in [currency] for [period] from a complete journal (in
    sequence order, starting at record 1). [now] bounds {!field-as_of} for a period that
    has not ended. Fails when a record breaks a ledger rule, when [account] is not opened
    before the period ends, when the account is a pending sub-account, or (a bug) when an
    identity of {!check} does not hold. *)
val create
  :  ?annotate:(Ledger.Entry.t -> Annotation.t) (** default {!Annotation.default} *)
  -> Ledger.Record.t list
  -> account:Ledger.Account_id.t
  -> currency:Money.Currency.t
  -> period:Period.t
  -> basis:Basis.t
  -> now:Time_ns.t
  -> t Or_error.t

(** {!create} over a journal file, read one frame at a time with [Ledger.Journal.fold]:
    memory is the ledger state, not the file. Fails like [Ledger.Journal.read] and like
    {!create}. *)
val of_journal_file
  :  ?annotate:(Ledger.Entry.t -> Annotation.t)
  -> string
  -> account:Ledger.Account_id.t
  -> currency:Money.Currency.t
  -> period:Period.t
  -> basis:Basis.t
  -> now:Time_ns.t
  -> t Or_error.t

(** Incremental construction, for callers that already fold over a journal. Feed every
    record in sequence order, then call [finish]. Records dated after the period are
    accepted and ignored. *)
module Builder : sig
  type statement := t
  type t

  val create
    :  ?annotate:(Ledger.Entry.t -> Annotation.t)
    -> account:Ledger.Account_id.t
    -> currency:Money.Currency.t
    -> period:Period.t
    -> basis:Basis.t
    -> unit
    -> t

  val add : t -> Ledger.Record.t -> t Or_error.t
  val finish : t -> now:Time_ns.t -> statement Or_error.t
end

(** Checks, for any statement value:
    - every amount is in the statement's currency, line amounts are positive and money in
      and out are not negative;
    - [opening + money_in - money_out = closing], and the totals match the lines;
    - each line's balance is the previous balance (the opening for the first) plus the
      line, and the last balance is [closing];
    - lines are in journal order and dated inside the period;
    - [held_for_pending] is minus the sum of the pending amounts (zero on the posting
      basis, where nothing is pending), and
      [ledger_balance_at_end = closing + held_for_pending]. *)
val check : t -> unit Or_error.t

(** ["<prefix>-statement-<CUR>-<period slug>.<extension>"], e.g.
    [bank-statement-EUR-2026-10.pdf]. *)
val file_name : t -> prefix:string -> extension:string -> string

(** The newest [limit] lines, newest first, and whether older lines were left out: the
    shape of an on-screen statement. *)
val newest_lines : t -> limit:int -> Line.t list * bool

module Stable : sig
  module V1 : sig
    (** For storing an issued statement. Decoding runs {!check}. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
