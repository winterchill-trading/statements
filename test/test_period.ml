open! Core
open Statements

let d = Date.of_string
let today = d "2026-10-14"
let earliest = d "2025-03-10"

let show ?month ?from ?to_ () =
  print_s
    [%sexp
      (Period.parse ?month ?from ?to_ ~today ~earliest ()
       : (Period.t, Period.Issue.t list) Result.t)]
;;

let%expect_test "default is the current month" =
  show ();
  [%expect
    {|
    (Ok
     ((from_date 2026-10-01) (to_date 2026-10-31)
      (month (((year 2026) (month Oct))))))
    |}]
;;

let%expect_test "month form" =
  show ~month:"2026-02" ();
  show ~month:"2026-11" ();
  show ~month:"2025-03" ();
  show ~month:"2025-02" ();
  show ~month:"2026-13" ();
  show ~month:"26-01" ();
  show ~month:"2026-1" ();
  [%expect
    {|
    (Ok
     ((from_date 2026-02-01) (to_date 2026-02-28)
      (month (((year 2026) (month Feb))))))
    (Error (((field Month) (problem Future))))
    (Ok
     ((from_date 2025-03-01) (to_date 2025-03-31)
      (month (((year 2025) (month Mar))))))
    (Error (((field Month) (problem Out_of_range))))
    (Error (((field Month) (problem Invalid))))
    (Error (((field Month) (problem Invalid))))
    (Error (((field Month) (problem Invalid))))
    |}]
;;

let%expect_test "day range form" =
  show ~from:"2026-01-15" ~to_:"2026-02-14" ();
  show ~from:"2026-09-01" ~to_:"2026-09-30" ();
  show ~from:"2025-10-15" ~to_:"2026-10-14" ();
  show ~from:"2025-10-15" ~to_:"2026-10-15" ();
  show ~from:"2026-10-15" ~to_:"2026-10-20" ();
  show ~from:"2026-10-01" ~to_:"2026-12-31" ();
  show ~from:"2025-03-09" ~to_:"2025-03-31" ();
  show ~from:"2026-05-02" ~to_:"2026-05-01" ();
  show ~from:"2026-05-02" ();
  show ~to_:"2026-05-02" ();
  show ~from:"2026-5-2" ~to_:"x" ();
  show ~month:"2026-05" ~from:"2026-05-02" ();
  [%expect
    {|
    (Ok ((from_date 2026-01-15) (to_date 2026-02-14) (month ())))
    (Ok
     ((from_date 2026-09-01) (to_date 2026-09-30)
      (month (((year 2026) (month Sep))))))
    (Ok ((from_date 2025-10-15) (to_date 2026-10-14) (month ())))
    (Error (((field To) (problem Too_long))))
    (Error (((field From) (problem Future))))
    (Ok ((from_date 2026-10-01) (to_date 2026-12-31) (month ())))
    (Error (((field From) (problem Out_of_range))))
    (Error (((field To) (problem Before_from))))
    (Error (((field To) (problem Required))))
    (Error (((field From) (problem Required))))
    (Error (((field From) (problem Invalid)) ((field To) (problem Invalid))))
    (Error (((field Month) (problem Conflict))))
    |}]
;;

let%expect_test "times, slugs and problem codes" =
  let p =
    Period.of_days ~from_date:(d "2026-01-31") ~to_date:(d "2027-01-30") |> ok_exn
  in
  print_s
    [%sexp
      (Period.start_time p : Time_ns.Alternate_sexp.t)
      , (Period.end_time p : Time_ns.Alternate_sexp.t)
      , (Period.days p : int)
      , (Period.slug p : string)];
  print_s
    [%sexp
      (Period.of_days ~from_date:(d "2026-01-31") ~to_date:(d "2027-01-31")
       : Period.t Or_error.t)];
  print_s [%sexp (List.map Period.Problem.all ~f:Period.Problem.to_string : string list)];
  [%expect
    {|
    ("2026-01-31 00:00:00Z" "2027-01-31 00:00:00Z" 365 2026-01-31_2027-01-30)
    (Error
     ("period longer than twelve months" (from_date 2026-01-31)
      (to_date 2027-01-31)))
    (CONFLICT REQUIRED INVALID FUTURE OUT_OF_RANGE BEFORE_FROM TOO_LONG)
    |}]
;;

let%expect_test "stable round trip checks the range" =
  let p = Period.of_month { year = 2026; month = Feb } in
  let bin = Binable.to_string (module Period.Stable.V1) p in
  assert (Period.equal p (Binable.of_string (module Period.Stable.V1) bin));
  print_endline (Bin_prot.Shape.eval_to_digest_string Period.Stable.V1.bin_shape_t);
  let bad = Sexp.of_string "((from_date 2026-02-01) (to_date 2026-02-28) (month ()))" in
  print_s
    [%sexp
      (Or_error.try_with (fun () -> Period.Stable.V1.t_of_sexp bad) : Period.t Or_error.t)];
  [%expect
    {|
    82533a4564f2d369d6ab388fd25691f1
    (Error
     ("inconsistent period"
      ((from_date 2026-02-01) (to_date 2026-02-28) (month ()))))
    |}]
;;
