(** Where a statement's lines come from: a ledger journal alone, or a journal next to
    transfer records.

    {!Statement.create} reads reserved funds from ledger holds. Some systems reserve money
    differently: an outgoing payment is a transfer record with a status, and the ledger
    gets plain entries for it (a debit when the transfer is created and a credit back if
    it fails, or a single debit when it settles). {!of_transfers} reads those records next
    to the journal and produces the same two bases:

    - [Posting]: the ledger as it is. Every entry on the account is a line, whatever
      transfer it belongs to, with [hold] set to the transfer's id; nothing is pending.
    - [Netted]: a transfer is one money-out line when it settles, dated at its settlement
      time and linked to it by {!Transfer.field-id} ([hold] and [held_since] on the line),
      placed in journal order at the latest ledger entry booked for it. A transfer that
      failed is no line at all. A transfer still pending at the end of the period is
      listed as pending and kept out of the balances. The entries booked for a transfer
      never appear as lines of their own on this basis.

    On the netted basis the balances are the ledger's with the effect of transfers still
    in flight undone: [closing] is the ledger balance plus whatever the ledger has already
    taken for transfers pending at the end, and [ledger_balance_at_end] is
    [closing + held_for_pending], the balance available to spend. Ledger holds on the same
    account are netted as {!Statement.create} nets them. *)

open! Core

(** An outgoing transfer, as the system that runs it records it. *)
module Transfer : sig
  module Status : sig
    type t =
      | Pending
      | Settled of { at : Time_ns.Alternate_sexp.t } (** the money has left for good *)
      | Failed of { at : Time_ns.Alternate_sexp.t }
      (** rejected, returned or cancelled: the money stays with (or came back to) the
          account *)
    [@@deriving sexp, compare, equal]
  end

  type t =
    { id : Ledger.Hold_id.t
    (** the transfer's id; it links the pending item and the settled line the way a hold
        id does *)
    ; account : Ledger.Account_id.t (** the account the money leaves *)
    ; amount : Money.Amount.t (** positive *)
    ; created_at : Time_ns.Alternate_sexp.t
    ; status : Status.t
    (** the latest status; its time decides whether the transfer was still pending at the
        end of a period *)
    ; entries : Ledger.Idempotency_key.t list
    (** the ledger entries booked for the transfer so far, in any order; empty while a
        system books only at settlement *)
    ; idempotency_key : Ledger.Idempotency_key.t
    (** the transfer's own key, shown on its pending item *)
    ; code : Ledger.Entry.Code.t
    (** shown on its line and pending item, e.g. [Withdrawal] *)
    ; reference : string
    ; annotation : Annotation.t option
    (** what the line says; [None] takes the annotation of its latest entry, or of none
        while it has none *)
    }
  [@@deriving sexp, compare, equal]

  (** Whether the transfer was created before [at] and had not settled or failed by then
      (a status time equal to [at] counts as not yet). *)
  val pending_at : t -> Time_ns.t -> bool
end

type t

(** A journal (complete, in sequence order) whose holds are ledger holds: the source of
    {!Statement.create}. *)
val of_ledger : ?annotate:(Ledger.Entry.t -> Annotation.t) -> Ledger.Record.t list -> t

(** A journal and the transfer records of the system that booked it. Transfers of other
    accounts and currencies are ignored when a statement is built. On both bases an entry
    of a transfer whose own annotation has an empty description takes the transfer's
    [annotation]. *)
val of_transfers
  :  ?annotate:(Ledger.Entry.t -> Annotation.t)
  -> Ledger.Record.t list
  -> transfers:Transfer.t list
  -> t

(** The statement of [account] in [currency] for [period]; see {!Statement.create} for
    [now] and the ledger checks.

    With transfers, also fails when an amount is not positive, when two transfers name the
    same entry, when a status time is before the creation time, when a transfer settled in
    the period has no ledger entry dated by its end, and when the transfers do not
    reconcile with the ledger: the netted closing worked out from the lines must equal the
    ledger balance with in-flight transfers undone, which holds only when every transfer's
    entries add up to its amount once it settles and to zero once it fails, and are dated
    no later than its status. *)
val statement
  :  t
  -> account:Ledger.Account_id.t
  -> currency:Money.Currency.t
  -> period:Period.t
  -> basis:Basis.t
  -> now:Time_ns.t
  -> Statement.t Or_error.t
