(** How a statement presents funds reserved by holds (card authorisations, payments
    waiting for the network).

    - [Netted]: the customer view. Placing, increasing, releasing or expiring a hold is
      not a line; a settlement (capture) is one line for the amount that left, dated when
      it settled and linked to its hold. Holds still open at the end of the period are
      listed apart as pending and are not in the balances. The identity is
      [opening + in - out = closing], and the ledger's available balance at the end is
      [closing + held_for_pending] (held is negative or zero).
    - [Posting]: the book view, as a business or an auditor reads it. Every movement of
      the account itself is a line: placing a hold is money out when it is placed, a
      release is money in, and a capture moves nothing further. Nothing is pending and
      [opening + in - out = closing = ] the ledger's available balance at the end. *)

open! Core

type t =
  | Netted
  | Posting
[@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

(** ["netted"], ["posting"]. *)
val to_string : t -> string

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
