open! Core

module T = struct
  type t =
    { description : string
    ; counterparty : string option
    ; category : string option
    }
  [@@deriving bin_io, sexp, compare, equal, hash]
end

include T

let default (e : Ledger.Entry.t) =
  { description = e.remittance_info; counterparty = None; category = None }
;;

module Stable = struct
  module V1 = T
end
