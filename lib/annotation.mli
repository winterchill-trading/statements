(** What a statement line says about a ledger entry beyond its amount.

    The ledger stores ids, not people, so the words a customer reads (a merchant name, a
    payee, a category) come from the caller: pass [~annotate] to {!Statement.create}. The
    default uses the entry's own remittance information. *)

open! Core

type t =
  { description : string (** shown on the line; empty means "use the code's label" *)
  ; counterparty : string option (** a merchant, payee or payer name *)
  ; category : string option (** a spending category code, e.g. [groceries] *)
  }
[@@deriving sexp, compare, equal, hash]

(** [{ description = entry.remittance_info; counterparty = None; category = None }]. *)
val default : Ledger.Entry.t -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
