open! Core
open Statements
open Helpers

(* Writes the September fixture into a real journal file (the journal stamps entries with
   its own clock, so a synthetic clock is moved to each entry's time first) and checks
   that the streamed statement equals the one built from the records in memory. *)
let%expect_test "of_journal_file equals create over the same records" =
  let dir = Filename_unix.temp_dir "statements" "" in
  let path = dir ^/ "test.journal" in
  let time_source =
    Async_kernel.Time_source.create
      ~now:(Time_ns.of_string_with_utc_offset "2026-08-01 00:00:00Z")
      ()
  in
  let journal = Ledger.Journal.create ~time_source ~fsync:false path |> ok_exn in
  List.iter
    (records (september ()))
    ~f:(fun r ->
      Async_kernel.Time_source.advance_directly
        time_source
        ~to_:
          (Time_ns.max
             (Ledger.Record.Event.recorded_at r.event)
             (Async_kernel.Time_source.now time_source));
      let result =
        match r.event with
        | Account_opened a -> Ledger.Journal.open_account journal a
        | Posted e -> Ledger.Journal.append_entry journal e
        | _ -> assert false
      in
      ignore (ok_exn result : Ledger.Journal.Append_result.t));
  Ledger.Journal.close journal;
  List.iter Basis.all ~f:(fun basis ->
    let streamed =
      Statement.of_journal_file
        path
        ~account:alice
        ~currency:Money.Currency.eur
        ~period:september_period
        ~basis
        ~now
      |> ok_exn
    in
    let in_memory =
      Statement.create
        (Ledger.Journal.read path |> ok_exn |> fst)
        ~account:alice
        ~currency:Money.Currency.eur
        ~period:september_period
        ~basis
        ~now
      |> ok_exn
    in
    [%test_result: Statement.t] streamed ~expect:in_memory;
    print_s
      [%sexp
        (basis : Basis.t)
        , (List.length streamed.lines : int)
        , (Money.Amount.to_string streamed.totals.closing : string)]);
  Core_unix.unlink path;
  Core_unix.rmdir dir;
  [%expect {|
    (Netted 4 "704.50 EUR")
    (Posting 8 "684.50 EUR")
    |}]
;;

let%expect_test "stable digests" =
  List.iter
    [ "Locale", Locale.Stable.V1.bin_shape_t
    ; "Basis", Basis.Stable.V1.bin_shape_t
    ; "Period", Period.Stable.V1.bin_shape_t
    ; "Annotation", Annotation.Stable.V1.bin_shape_t
    ; "Totals", Statement.Totals.Stable.V1.bin_shape_t
    ; "Statement", Statement.Stable.V1.bin_shape_t
    ]
    ~f:(fun (name, shape) ->
      printf "%-10s %s\n" name (Bin_prot.Shape.eval_to_digest_string shape));
  [%expect
    {|
    Locale     70db471bb180d3cc471b7da195c61aa3
    Basis      5a39832c9e71abf90b9746a2ff461e06
    Period     82533a4564f2d369d6ab388fd25691f1
    Annotation a5a3038375301bdff7dc6e8399da78da
    Totals     32ec91e2759de9c5b2299406d8d55a7b
    Statement  92339c7c120e8f35676944ef9a30e7ab
    |}]
;;

let%expect_test "a statement round-trips through its stable form" =
  let s =
    Statement.create
      (records (september ()))
      ~account:alice
      ~currency:Money.Currency.eur
      ~period:september_period
      ~basis:Netted
      ~now
    |> ok_exn
  in
  let bin = Binable.to_string (module Statement.Stable.V1) s in
  [%test_result: Statement.t]
    (Binable.of_string (module Statement.Stable.V1) bin)
    ~expect:s;
  let sexp = Statement.Stable.V1.sexp_of_t s in
  [%test_result: Statement.t] (Statement.Stable.V1.t_of_sexp sexp) ~expect:s;
  let broken = { s with held_for_pending = Money.Amount.zero Money.Currency.eur } in
  let bin = Binable.to_string (module Statement.Stable.V1) broken in
  print_s
    [%sexp
      (Or_error.try_with (fun () -> Binable.of_string (module Statement.Stable.V1) bin)
       |> Or_error.map ~f:(fun _ -> ())
       : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("held_for_pending does not match the pending holds"
      (account customer:alice)))
    |}]
;;
