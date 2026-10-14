(** The period a statement covers: a calendar month or a range of whole days, in UTC.

    Days are UTC calendar days and a period runs from 00:00 UTC on its first day up to,
    not including, 00:00 UTC on the day after its last day. A statement selects ledger
    entries by their business date, which the ledger assigns in UTC and never lets go
    backwards, so consecutive periods split a journal without gaps or overlaps.

    A period is at most {!max_months} long. {!parse} applies the usual rules for a
    statement request (a month, or a first and last day, not both) and reports every
    problem with the field it belongs to, so an HTTP layer can map them one to one onto
    validation errors. *)

open! Core

(** A calendar month, written [YYYY-MM]. *)
module Year_month : sig
  type t =
    { year : int
    ; month : Month.t
    }
  [@@deriving sexp, compare, equal, hash]

  (** ["2026-10"]. *)
  val to_string : t -> string

  (** Parses exactly [YYYY-MM] with a four-digit year from 1900 to 9999. *)
  val of_string_or_error : string -> t Or_error.t

  val of_date : Date.t -> t
  val first_day : t -> Date.t
  val last_day : t -> Date.t

  (** The month after. *)
  val succ : t -> t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

type t = private
  { from_date : Date.t (** first day, inclusive *)
  ; to_date : Date.t (** last day, inclusive *)
  ; month : Year_month.t option
  (** [Some] when the period is exactly one calendar month *)
  }
[@@deriving sexp_of, compare, equal, hash]

(** 12. *)
val max_months : int

(** One calendar month. *)
val of_month : Year_month.t -> t

(** [from_date] to [to_date] inclusive. Fails when [to_date] is before [from_date] or the
    range is longer than {!max_months} (it must end before the same day twelve months
    later). A range that is exactly a calendar month is the same value as {!of_month}. *)
val of_days : from_date:Date.t -> to_date:Date.t -> t Or_error.t

(** 00:00 UTC on [from_date]. *)
val start_time : t -> Time_ns.t

(** 00:00 UTC on the day after [to_date]: the first instant not covered. *)
val end_time : t -> Time_ns.t

(** Whether a business date falls in the period. *)
val contains : t -> Date.t -> bool

(** Number of days covered. *)
val days : t -> int

(** ["2026-10"] for a month, ["2026-10-01_2026-12-31"] otherwise: safe in a file name. *)
val slug : t -> string

(** {2 Parsing a request} *)

module Field : sig
  type t =
    | Month
    | From
    | To
  [@@deriving sexp, compare, equal, enumerate]

  (** ["month"], ["from"], ["to"]. *)
  val to_string : t -> string
end

module Problem : sig
  type t =
    | Conflict (** both a month and a day range were given *)
    | Required (** one end of a day range is missing *)
    | Invalid (** not a valid [YYYY-MM] or [YYYY-MM-DD] *)
    | Future (** starts after today *)
    | Out_of_range (** starts before the earliest date statements exist for *)
    | Before_from (** the last day is before the first *)
    | Too_long (** longer than {!max_months} *)
  [@@deriving sexp, compare, equal, enumerate]

  (** ["CONFLICT"], ["REQUIRED"], ...: upper-case codes for an API error body. *)
  val to_string : t -> string
end

module Issue : sig
  type t =
    { field : Field.t
    ; problem : Problem.t
    }
  [@@deriving sexp, compare, equal]
end

(** Reads a statement request. [month] is [YYYY-MM]; [from] and [to_] are [YYYY-MM-DD] and
    inclusive. With neither form, the period is the month of [today]. [today] is the
    current UTC date; [earliest] is the first day statements may cover (an account's
    opening date, or a retention limit).

    A month after the current one is [Future] and a month ending before [earliest] is
    [Out_of_range]. In a day range a first day after [today] is [Future] and one before
    [earliest] is [Out_of_range]; a last day in the future is allowed (the statement then
    ends now). All problems found are returned, in field order. *)
val parse
  :  ?month:string
  -> ?from:string
  -> ?to_:string
  -> today:Date.t
  -> earliest:Date.t
  -> unit
  -> (t, Issue.t list) Result.t

module Stable : sig
  module V1 : sig
    (** Decoding checks the range again. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
