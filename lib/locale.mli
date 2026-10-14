(** The languages a statement can be rendered in.

    A locale picks the labels, the number format and the date format from a
    {!Catalogue}. The data in a statement (amounts, dates, codes) does not depend on it;
    only the rendering does. *)

open! Core

type t =
  | En (** English *)
  | De (** German *)
  | Fr (** French *)
  | Es (** Spanish *)
  | Ru (** Russian *)
  | Ky (** Kyrgyz *)
[@@deriving sexp, compare, equal, hash, enumerate, quickcheck]

(** The two-letter ISO 639-1 code: ["en"], ["de"], ["fr"], ["es"], ["ru"], ["ky"]. *)
val to_string : t -> string

(** Parses a language tag case-insensitively. A region or script subtag is ignored, so
    ["de-AT"] and ["ru_KG"] parse as [De] and [Ru]. Fails on any other language. *)
val of_string_or_error : string -> t Or_error.t

(** [resolve candidates] is the first candidate that parses, else [En]: the usual order
    is an explicit request parameter, then the customer's stored preference. *)
val resolve : string option list -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
