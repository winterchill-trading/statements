open! Core
open Statements

let render_output ~format ~locale ~header ~preamble statement =
  let catalogue = Catalogue.builtin locale in
  match format with
  | `Csv -> Csv.render ?preamble:(Option.some_if preamble catalogue) statement
  | `Pdf -> Pdf.render ~catalogue ~header statement
  | `Sexp -> Sexp.to_string_hum [%sexp (statement : Statement.t)] ^ "\n"
;;

let format_flag =
  let open Command.Param in
  flag
    "format"
    (optional_with_default
       `Pdf
       (Arg_type.of_alist_exn [ "pdf", `Pdf; "csv", `Csv; "sexp", `Sexp ]))
    ~doc:"FORMAT pdf (default), csv or sexp"
;;

let locale_flag =
  let open Command.Param in
  flag
    "locale"
    (optional_with_default
       Locale.En
       (Arg_type.create (fun s -> Locale.of_string_or_error s |> ok_exn)))
    ~doc:"LANG en (default), de, fr, es, ru or ky"
;;

let basis_flag =
  let open Command.Param in
  flag
    "basis"
    (optional_with_default
       Basis.Netted
       (Arg_type.of_alist_exn (List.map Basis.all ~f:(fun b -> Basis.to_string b, b))))
    ~doc:"BASIS netted (default) or posting"
;;

let preamble_flag =
  Command.Param.flag
    "preamble"
    Command.Param.no_arg
    ~doc:" CSV: add the localised summary rows"
;;

let write output data =
  match output with
  | None -> Out_channel.output_string Out_channel.stdout data
  | Some path -> Out_channel.write_all path ~data
;;

let demo =
  Command.basic
    ~summary:"Render the statement of a built-in sample account (September 2026)"
    (let%map_open.Command format = format_flag
     and locale = locale_flag
     and basis = basis_flag
     and preamble = preamble_flag
     and output =
       flag "output" (optional string) ~doc:"FILE write here instead of stdout"
     in
     fun () ->
       let statement =
         Statement.create
           ~annotate:Demo.annotate
           (Demo.records ())
           ~account:Demo.account
           ~currency:Money.Currency.eur
           ~period:(Period.of_month { year = 2026; month = Sep })
           ~basis
           ~now:(Time_ns.of_string_with_utc_offset "2026-10-14 12:00:00Z")
         |> ok_exn
       in
       let header =
         Pdf.Header.create
           ~institution:"Example Bank"
           ~holder:"Alex Example"
           ~details:[ "IBAN", "DE00 0000 0000 0000 0000 00" ]
           ~account_name:"Main account"
           ()
       in
       write output (render_output ~format ~locale ~header ~preamble statement))
;;

let journal =
  Command.basic
    ~summary:"Render a statement from a ledger journal file"
    (let%map_open.Command path = anon ("JOURNAL" %: string)
     and account = flag "account" (required string) ~doc:"ID the account"
     and currency = flag "currency" (required string) ~doc:"CODE e.g. EUR"
     and month = flag "month" (optional string) ~doc:"YYYY-MM the month"
     and from = flag "from" (optional string) ~doc:"YYYY-MM-DD first day"
     and to_ = flag "to" (optional string) ~doc:"YYYY-MM-DD last day"
     and name =
       flag "name" (optional_with_default "Account" string) ~doc:"NAME account name"
     and format = format_flag
     and locale = locale_flag
     and basis = basis_flag
     and preamble = preamble_flag
     and output =
       flag "output" (optional string) ~doc:"FILE write here instead of stdout"
     in
     fun () ->
       let now = Time_ns.now () in
       let today = Time_ns.to_date now ~zone:Time_float.Zone.utc in
       let period =
         match
           Period.parse
             ?month
             ?from
             ?to_
             ~today
             ~earliest:(Date.of_string "1970-01-01")
             ()
         with
         | Ok p -> p
         | Error issues -> raise_s [%message "bad period" (issues : Period.Issue.t list)]
       in
       let statement =
         Statement.of_journal_file
           path
           ~account:(Ledger.Account_id.of_string account)
           ~currency:(Money.Currency.of_string currency)
           ~period
           ~basis
           ~now
         |> ok_exn
       in
       let header = Pdf.Header.create ~account_name:name () in
       write output (render_output ~format ~locale ~header ~preamble statement))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Customer statements from a double-entry ledger journal"
       [ "demo", demo; "journal", journal ])
;;
