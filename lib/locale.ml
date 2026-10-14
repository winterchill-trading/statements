open! Core

module T = struct
  type t =
    | En
    | De
    | Fr
    | Es
    | Ru
    | Ky
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
end

include T

let to_string = function
  | En -> "en"
  | De -> "de"
  | Fr -> "fr"
  | Es -> "es"
  | Ru -> "ru"
  | Ky -> "ky"
;;

let of_string_or_error s =
  let primary =
    String.lowercase s
    |> String.split_on_chars ~on:[ '-'; '_' ]
    |> List.hd
    |> Option.value ~default:""
  in
  match List.find all ~f:(fun l -> String.equal (to_string l) primary) with
  | Some l -> Ok l
  | None -> Or_error.error_s [%message "unsupported locale" ~_:(s : string)]
;;

let resolve candidates =
  List.find_map candidates ~f:(fun c ->
    Option.bind c ~f:(fun s -> Or_error.ok (of_string_or_error s)))
  |> Option.value ~default:En
;;

module Stable = struct
  module V1 = T
end
