open! Core

module T = struct
  type t =
    | Netted
    | Posting
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
end

include T

let to_string = function
  | Netted -> "netted"
  | Posting -> "posting"
;;

module Stable = struct
  module V1 = T
end
