open! Core

module Year_month = struct
  module T = struct
    type t =
      { year : int
      ; month : Month.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end

  include T

  let to_string { year; month } = sprintf "%04d-%02d" year (Month.to_int month)

  let of_string_or_error s =
    let invalid () = Or_error.error_s [%message "expected YYYY-MM" ~_:(s : string)] in
    if String.length s <> 7
       || (not (Char.equal s.[4] '-'))
       || not (String.for_alli s ~f:(fun i c -> i = 4 || Char.is_digit c))
    then invalid ()
    else (
      let year = Int.of_string (String.sub s ~pos:0 ~len:4) in
      let m = Int.of_string (String.sub s ~pos:5 ~len:2) in
      if year < 1900 || m < 1 || m > 12
      then invalid ()
      else Ok { year; month = Month.of_int_exn m })
  ;;

  let of_date d = { year = Date.year d; month = Date.month d }
  let first_day { year; month } = Date.create_exn ~y:year ~m:month ~d:1

  let last_day { year; month } =
    Date.create_exn ~y:year ~m:month ~d:(Date.days_in_month ~year ~month)
  ;;

  let succ t = of_date (Date.add_months (first_day t) 1)

  module Stable = struct
    module V1 = T
  end
end

module T = struct
  type t =
    { from_date : Date.Stable.V1.t
    ; to_date : Date.Stable.V1.t
    ; month : Year_month.Stable.V1.t option
    }
  [@@deriving bin_io, sexp, compare, equal, hash]
end

include T

let max_months = 12

let of_month ym =
  { from_date = Year_month.first_day ym
  ; to_date = Year_month.last_day ym
  ; month = Some ym
  }
;;

(* Twelve months from the 31st of a month land on the last day of the target month
   ([Date.add_months] clamps), so the bound is "before the same day twelve months on",
   which for 31 Jan is 31 Jan of the next year. *)
let too_long ~from_date ~to_date =
  Date.( >= ) to_date (Date.add_months from_date max_months)
;;

let of_days ~from_date ~to_date =
  if Date.( < ) to_date from_date
  then
    Or_error.error_s
      [%message "period ends before it starts" (from_date : Date.t) (to_date : Date.t)]
  else if too_long ~from_date ~to_date
  then
    Or_error.error_s
      [%message
        "period longer than twelve months" (from_date : Date.t) (to_date : Date.t)]
  else (
    let ym = Year_month.of_date from_date in
    if Date.equal from_date (Year_month.first_day ym)
       && Date.equal to_date (Year_month.last_day ym)
    then Ok (of_month ym)
    else Ok { from_date; to_date; month = None })
;;

let midnight d =
  Time_ns.of_date_ofday ~zone:Time_float.Zone.utc d Time_ns.Ofday.start_of_day
;;

let start_time t = midnight t.from_date
let end_time t = midnight (Date.add_days t.to_date 1)
let contains t d = Date.( >= ) d t.from_date && Date.( <= ) d t.to_date
let days t = Date.diff t.to_date t.from_date + 1

let slug t =
  match t.month with
  | Some ym -> Year_month.to_string ym
  | None -> sprintf "%s_%s" (Date.to_string t.from_date) (Date.to_string t.to_date)
;;

module Field = struct
  type t =
    | Month
    | From
    | To
  [@@deriving sexp, compare, equal, enumerate]

  let to_string = function
    | Month -> "month"
    | From -> "from"
    | To -> "to"
  ;;
end

module Problem = struct
  type t =
    | Conflict
    | Required
    | Invalid
    | Future
    | Out_of_range
    | Before_from
    | Too_long
  [@@deriving sexp, compare, equal, enumerate]

  let to_string t = Sexp.to_string [%sexp (t : t)] |> String.uppercase
end

module Issue = struct
  type t =
    { field : Field.t
    ; problem : Problem.t
    }
  [@@deriving sexp, compare, equal]
end

(* Strict [YYYY-MM-DD]: [Date.of_string] also accepts other layouts. *)
let parse_day s =
  if String.length s = 10
     && Char.equal s.[4] '-'
     && Char.equal s.[7] '-'
     && String.for_alli s ~f:(fun i c -> i = 4 || i = 7 || Char.is_digit c)
  then Option.try_with (fun () -> Date.of_string s)
  else None
;;

let parse ?month ?from ?to_ ~today ~earliest () =
  let issue field problem = { Issue.field; problem } in
  let blank = Option.filter ~f:(fun s -> not (String.is_empty (String.strip s))) in
  let month = blank month
  and from = blank from
  and to_ = blank to_ in
  match month, from, to_ with
  | Some _, Some _, _ | Some _, _, Some _ -> Error [ issue Month Conflict ]
  | None, None, None -> Ok (of_month (Year_month.of_date today))
  | Some m, None, None ->
    (match Year_month.of_string_or_error m with
     | Error _ -> Error [ issue Month Invalid ]
     | Ok ym ->
       if Date.( > ) (Year_month.first_day ym) today
       then Error [ issue Month Future ]
       else if Date.( < ) (Year_month.last_day ym) earliest
       then Error [ issue Month Out_of_range ]
       else Ok (of_month ym))
  | None, from, to_ ->
    let day field = function
      | None -> Error [ issue field Required ]
      | Some s ->
        (match parse_day s with
         | None -> Error [ issue field Invalid ]
         | Some d -> Ok d)
    in
    (match day From from, day To to_ with
     | Error a, Error b -> Error (a @ b)
     | Error a, Ok _ | Ok _, Error a -> Error a
     | Ok from_date, Ok to_date ->
       let from_issues =
         if Date.( > ) from_date today
         then [ issue From Future ]
         else if Date.( < ) from_date earliest
         then [ issue From Out_of_range ]
         else []
       in
       let to_issues =
         if Date.( < ) to_date from_date
         then [ issue To Before_from ]
         else if too_long ~from_date ~to_date
         then [ issue To Too_long ]
         else []
       in
       (match from_issues @ to_issues with
        | [] -> Ok (of_days ~from_date ~to_date |> ok_exn)
        | issues -> Error issues))
;;

module Stable = struct
  module V1 = struct
    include T

    let check t =
      let%bind.Or_error p = of_days ~from_date:t.from_date ~to_date:t.to_date in
      if equal p t
      then Ok t
      else Or_error.error_s [%message "inconsistent period" ~_:(t : t)]
    ;;

    let t_of_sexp s = t_of_sexp s |> check |> ok_exn

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (T)
        (struct
          type nonrec t = t

          let to_binable = Fn.id
          let of_binable t = check t |> ok_exn
        end)
  end
end
