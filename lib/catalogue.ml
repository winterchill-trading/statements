open! Core

module Label = struct
  module T = struct
    type t =
      | Statement_title
      | Account
      | Currency
      | Period
      | As_of
      | Summary
      | Opening_balance
      | Money_in
      | Money_out
      | Closing_balance
      | Held_for_pending
      | Available_at_end
      | Transactions
      | No_transactions
      | Pending_title
      | Date
      | Description
      | Reference
      | Amount
      | Balance
      | Reversal_note
      | Authorised_on
      | Page
      | Not_a_real_statement
      | Basis_netted
      | Basis_posting
      | Code_deposit
      | Code_withdrawal
      | Code_transfer
      | Code_fx
      | Code_fee
      | Code_card_auth
      | Code_card_settlement
      | Code_refund
      | Code_reversal
      | Code_return
      | Code_recall
      | Code_adjustment
      | Code_interest
      | Code_safeguarding
      | Code_hold
    [@@deriving sexp, compare, equal, hash, enumerate]
  end

  include T
  include Comparator.Make (T)

  let of_code : Ledger.Entry.Code.t -> t = function
    | Deposit -> Code_deposit
    | Withdrawal -> Code_withdrawal
    | Transfer_intraledger -> Code_transfer
    | Fx_leg -> Code_fx
    | Fee -> Code_fee
    | Card_auth -> Code_card_auth
    | Card_settlement -> Code_card_settlement
    | Refund -> Code_refund
    | Reversal -> Code_reversal
    | Return -> Code_return
    | Recall -> Code_recall
    | Adjustment _ -> Code_adjustment
    | Interest -> Code_interest
    | Safeguarding_sweep -> Code_safeguarding
    | Hold -> Code_hold
  ;;

  let placeholders = function
    | Authorised_on -> [ "date" ]
    | Page -> [ "page"; "pages" ]
    | _ -> []
  ;;
end

module Number_format = struct
  type t =
    { decimal_separator : string
    ; group_separator : string
    ; min_grouping_digits : int
    ; currency_first : bool
    }
  [@@deriving sexp, compare, equal]
end

module Date_format = struct
  type t =
    { pattern : string
    ; short_months : string list
    }
  [@@deriving sexp, compare, equal]
end

type t =
  { locale : Locale.t
  ; texts : string Map.M(Label).t
  ; number : Number_format.t
  ; date : Date_format.t
  ; months : string list
  ; month_title : string
  }
[@@deriving sexp_of]

(* The names inside braces, in order of appearance. *)
let placeholders_of s =
  let rec go acc i =
    match String.index_from s i '{' with
    | None -> List.rev acc
    | Some j ->
      (match String.index_from s j '}' with
       | None -> List.rev acc
       | Some k -> go (String.sub s ~pos:(j + 1) ~len:(k - j - 1) :: acc) (k + 1))
  in
  go [] 0
;;

let check_text label text =
  let expected = List.sort (Label.placeholders label) ~compare:String.compare in
  let found = List.sort (placeholders_of text) ~compare:String.compare in
  if [%equal: string list] expected found && not (String.is_empty (String.strip text))
  then Ok ()
  else
    Or_error.error_s
      [%message
        "text does not fit its label"
          (label : Label.t)
          (text : string)
          (expected : string list)]
;;

let create ~locale ~texts ~number ~date ~months ~month_title =
  let open Or_error.Let_syntax in
  let%bind texts =
    match Map.of_alist (module Label) texts with
    | `Ok m -> Ok m
    | `Duplicate_key l -> Or_error.error_s [%message "duplicate label" (l : Label.t)]
  in
  let%bind () =
    List.map Label.all ~f:(fun l ->
      match Map.find texts l with
      | None -> Or_error.error_s [%message "missing label" (l : Label.t)]
      | Some s -> check_text l s)
    |> Or_error.all_unit
  in
  let%bind () =
    if List.length months = 12 && List.length date.Date_format.short_months = 12
    then Ok ()
    else Or_error.error_s [%message "twelve month names are needed"]
  in
  let%map () =
    if [%equal: string list]
         (List.sort (placeholders_of month_title) ~compare:String.compare)
         [ "month"; "year" ]
    then Ok ()
    else Or_error.error_s [%message "month_title needs {month} and {year}" month_title]
  in
  { locale; texts; number; date; months; month_title }
;;

let locale t = t.locale
let text t label = Map.find_exn t.texts label

let substitute template params =
  List.fold params ~init:template ~f:(fun acc (name, value) ->
    String.substr_replace_all acc ~pattern:("{" ^ name ^ "}") ~with_:value)
;;

let render t label params = substitute (text t label) params

let with_text t label s =
  let%map.Or_error () = check_text label s in
  { t with texts = Map.set t.texts ~key:label ~data:s }
;;

let code_label t code = text t (Label.of_code code)

let group_digits (n : Number_format.t) digits =
  let len = String.length digits in
  if len < 3 + n.min_grouping_digits
  then digits
  else (
    let first =
      match len mod 3 with
      | 0 -> 3
      | r -> r
    in
    let groups =
      String.sub digits ~pos:0 ~len:first
      :: List.init
           ((len - first) / 3)
           ~f:(fun i -> String.sub digits ~pos:(first + (3 * i)) ~len:3)
    in
    String.concat ~sep:n.group_separator groups)
;;

let format_amount ?(currency = false) t amount =
  let s = Money.Amount.to_decimal_string amount in
  let negative, s =
    match String.chop_prefix s ~prefix:"-" with
    | Some rest -> true, rest
    | None -> false, s
  in
  let int_part, frac =
    match String.lsplit2 s ~on:'.' with
    | Some (i, f) -> i, Some f
    | None -> s, None
  in
  let number =
    (if negative then "-" else "")
    ^ group_digits t.number int_part
    ^ Option.value_map frac ~default:"" ~f:(fun f -> t.number.decimal_separator ^ f)
  in
  if not currency
  then number
  else (
    let code = Money.Currency.to_string (Money.Amount.currency amount) in
    if t.number.currency_first then code ^ " " ^ number else number ^ " " ^ code)
;;

let format_date t d =
  let month = Month.to_int (Date.month d) in
  (* Longest tokens first so [MMM] is not read as [MM] followed by [M]. *)
  let tokens =
    [ "yyyy", sprintf "%04d" (Date.year d)
    ; "MMM", List.nth_exn t.date.short_months (month - 1)
    ; "MM", sprintf "%02d" month
    ; "dd", sprintf "%02d" (Date.day d)
    ; "d", Int.to_string (Date.day d)
    ]
  in
  let p = t.date.pattern in
  let buf = Buffer.create 16 in
  let rec go i =
    if i < String.length p
    then (
      match
        List.find tokens ~f:(fun (tok, _) ->
          String.is_substring_at p ~pos:i ~substring:tok)
      with
      | Some (tok, value) ->
        Buffer.add_string buf value;
        go (i + String.length tok)
      | None ->
        Buffer.add_char buf p.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents buf
;;

let format_time t time =
  let date, ofday = Time_ns.to_date_ofday time ~zone:Time_float.Zone.utc in
  let parts = Time_ns.Ofday.to_parts ofday in
  sprintf "%s, %02d:%02d UTC" (format_date t date) parts.hr parts.min
;;

let format_period t (p : Period.t) =
  match p.month with
  | Some { year; month } ->
    substitute
      t.month_title
      [ "month", List.nth_exn t.months (Month.to_int month - 1)
      ; "year", Int.to_string year
      ]
  | None -> format_date t p.from_date ^ " \u{2013} " ^ format_date t p.to_date
;;

(* Built-in texts. Columns: English, German, French, Spanish, Russian, Kyrgyz. *)
let rows : (Label.t * string list) list =
  [ ( Statement_title
    , [ "Account statement"
      ; "Kontoauszug"
      ; "Relevé de compte"
      ; "Extracto de cuenta"
      ; "Выписка по счёту"
      ; "Эсептен көчүрмө"
      ] )
  ; Account, [ "Account"; "Konto"; "Compte"; "Cuenta"; "Счёт"; "Эсеп" ]
  ; Currency, [ "Currency"; "Währung"; "Devise"; "Divisa"; "Валюта"; "Валюта" ]
  ; Period, [ "Period"; "Zeitraum"; "Période"; "Periodo"; "Период"; "Мезгил" ]
  ; ( As_of
    , [ "As of"; "Stand"; "Arrêté au"; "A fecha de"; "По состоянию на"; "Абалы боюнча" ] )
  ; Summary, [ "Summary"; "Übersicht"; "Récapitulatif"; "Resumen"; "Сводка"; "Жыйынтык" ]
  ; ( Opening_balance
    , [ "Opening balance"
      ; "Anfangssaldo"
      ; "Solde d'ouverture"
      ; "Saldo inicial"
      ; "Входящий остаток"
      ; "Баштапкы калдык"
      ] )
  ; ( Money_in
    , [ "Money in"; "Eingänge"; "Crédits"; "Entradas"; "Поступления"; "Кирген каражат" ] )
  ; ( Money_out
    , [ "Money out"; "Ausgänge"; "Débits"; "Salidas"; "Списания"; "Чыккан каражат" ] )
  ; ( Closing_balance
    , [ "Closing balance"
      ; "Endsaldo"
      ; "Solde de clôture"
      ; "Saldo final"
      ; "Исходящий остаток"
      ; "Акыркы калдык"
      ] )
  ; ( Held_for_pending
    , [ "Held for pending payments"
      ; "Für ausstehende Zahlungen reserviert"
      ; "Réservé pour opérations en attente"
      ; "Retenido para pagos pendientes"
      ; "Заблокировано по незавершённым операциям"
      ; "Аякталбаган операциялар үчүн бөгөттөлгөн"
      ] )
  ; ( Available_at_end
    , [ "Available balance at end"
      ; "Verfügbarer Saldo am Ende"
      ; "Solde disponible en fin de période"
      ; "Saldo disponible al final"
      ; "Доступный остаток на конец периода"
      ; "Мезгилдин аягындагы жеткиликтүү калдык"
      ] )
  ; ( Transactions
    , [ "Transactions"
      ; "Umsätze"
      ; "Opérations"
      ; "Movimientos"
      ; "Операции"
      ; "Операциялар"
      ] )
  ; ( No_transactions
    , [ "No transactions in this period."
      ; "Keine Umsätze in diesem Zeitraum."
      ; "Aucune opération sur cette période."
      ; "No hay movimientos en este periodo."
      ; "За период операций нет."
      ; "Бул мезгилде операциялар жок."
      ] )
  ; ( Pending_title
    , [ "Pending, not included in the balances"
      ; "Vorgemerkt, nicht im Saldo enthalten"
      ; "En attente, non incluses dans les soldes"
      ; "Pendientes, no incluidos en los saldos"
      ; "В обработке, не учтены в остатках"
      ; "Иштетилүүдө, калдыктарга кошулган эмес"
      ] )
  ; Date, [ "Date"; "Datum"; "Date"; "Fecha"; "Дата"; "Күнү" ]
  ; ( Description
    , [ "Description"; "Beschreibung"; "Libellé"; "Concepto"; "Описание"; "Сүрөттөмө" ] )
  ; ( Reference
    , [ "Reference"; "Referenz"; "Référence"; "Referencia"; "Референс"; "Референс" ] )
  ; Amount, [ "Amount"; "Betrag"; "Montant"; "Importe"; "Сумма"; "Сумма" ]
  ; Balance, [ "Balance"; "Saldo"; "Solde"; "Saldo"; "Остаток"; "Калдык" ]
  ; Reversal_note, [ "reversal"; "Storno"; "annulation"; "anulación"; "сторно"; "сторно" ]
  ; ( Authorised_on
    , [ "authorised {date}"
      ; "autorisiert am {date}"
      ; "autorisé le {date}"
      ; "autorizado el {date}"
      ; "авторизовано {date}"
      ; "{date} уруксат берилген"
      ] )
  ; ( Page
    , [ "Page {page} of {pages}"
      ; "Seite {page} von {pages}"
      ; "Page {page} sur {pages}"
      ; "Página {page} de {pages}"
      ; "Страница {page} из {pages}"
      ; "Барак {page} / {pages}"
      ] )
  ; ( Not_a_real_statement
    , [ "Simulated statement, not a real bank document"
      ; "Simulierter Auszug, kein echtes Bankdokument"
      ; "Relevé simulé, pas un document bancaire réel"
      ; "Extracto simulado, no es un documento bancario real"
      ; "Имитация выписки, не является банковским документом"
      ; "Симуляцияланган көчүрмө, чыныгы банк документи эмес"
      ] )
  ; ( Basis_netted
    , [ "Card payments are shown when they settle; amounts still held are listed under \
         Pending."
      ; "Kartenzahlungen erscheinen bei der Abrechnung; noch reservierte Beträge stehen \
         unter Vorgemerkt."
      ; "Les paiements par carte apparaissent à leur règlement ; les montants encore \
         réservés figurent sous En attente."
      ; "Los pagos con tarjeta aparecen al liquidarse; los importes aún retenidos \
         figuran en Pendientes."
      ; "Операции по карте отражаются при списании; заблокированные суммы указаны в \
         разделе «В обработке»."
      ; "Карта менен төлөмдөр эсептешкенде көрсөтүлөт; бөгөттөлгөн суммалар \
         «Иштетилүүдө» бөлүмүндө."
      ] )
  ; ( Basis_posting
    , [ "Every posting is listed, including funds reserved and released."
      ; "Alle Buchungen sind aufgeführt, auch Reservierungen und Freigaben."
      ; "Toutes les écritures figurent, y compris les réservations et leurs levées."
      ; "Se incluyen todos los asientos, también las retenciones y sus liberaciones."
      ; "Указаны все проводки, включая блокировки и их снятие."
      ; "Бардык проводкалар, анын ичинде бөгөттөөлөр жана алардын алынышы көрсөтүлгөн."
      ] )
  ; ( Code_deposit
    , [ "Deposit"; "Einzahlung"; "Dépôt"; "Ingreso"; "Пополнение"; "Толуктоо" ] )
  ; ( Code_withdrawal
    , [ "Payment out"
      ; "Auszahlung"
      ; "Paiement sortant"
      ; "Pago saliente"
      ; "Исходящий платёж"
      ; "Чыгуучу төлөм"
      ] )
  ; ( Code_transfer
    , [ "Transfer"; "Umbuchung"; "Virement interne"; "Traspaso"; "Перевод"; "Которуу" ] )
  ; ( Code_fx
    , [ "Currency exchange"
      ; "Währungstausch"
      ; "Change"
      ; "Cambio de divisa"
      ; "Обмен валюты"
      ; "Валюта алмаштыруу"
      ] )
  ; Code_fee, [ "Fee"; "Gebühr"; "Frais"; "Comisión"; "Комиссия"; "Комиссия" ]
  ; ( Code_card_auth
    , [ "Card authorisation"
      ; "Kartenautorisierung"
      ; "Autorisation carte"
      ; "Autorización de tarjeta"
      ; "Авторизация по карте"
      ; "Карта авторизациясы"
      ] )
  ; ( Code_card_settlement
    , [ "Card payment"
      ; "Kartenzahlung"
      ; "Paiement par carte"
      ; "Pago con tarjeta"
      ; "Оплата картой"
      ; "Карта менен төлөм"
      ] )
  ; ( Code_refund
    , [ "Refund"; "Erstattung"; "Remboursement"; "Reembolso"; "Возврат"; "Кайтарым" ] )
  ; Code_reversal, [ "Reversal"; "Storno"; "Annulation"; "Anulación"; "Сторно"; "Сторно" ]
  ; ( Code_return
    , [ "Returned payment"
      ; "Rücküberweisung"
      ; "Paiement retourné"
      ; "Pago devuelto"
      ; "Возврат платежа"
      ; "Кайтарылган төлөм"
      ] )
  ; ( Code_recall
    , [ "Recalled payment"
      ; "Rückruf"
      ; "Paiement rappelé"
      ; "Pago retirado"
      ; "Отзыв платежа"
      ; "Кайра чакырылган төлөм"
      ] )
  ; ( Code_adjustment
    , [ "Correction"; "Korrektur"; "Correction"; "Corrección"; "Корректировка"; "Оңдоо" ]
    )
  ; ( Code_interest
    , [ "Interest"; "Zinsen"; "Intérêts"; "Intereses"; "Проценты"; "Пайыздар" ] )
  ; ( Code_safeguarding
    , [ "Safeguarding transfer"
      ; "Sicherungsübertrag"
      ; "Transfert de cantonnement"
      ; "Traspaso de salvaguarda"
      ; "Перевод на защищённый счёт"
      ; "Коргоо эсебине которуу"
      ] )
  ; ( Code_hold
    , [ "Reserved funds"
      ; "Reservierung"
      ; "Fonds réservés"
      ; "Fondos retenidos"
      ; "Блокировка средств"
      ; "Бөгөттөлгөн каражат"
      ] )
  ]
;;

let column : Locale.t -> int = function
  | En -> 0
  | De -> 1
  | Fr -> 2
  | Es -> 3
  | Ru -> 4
  | Ky -> 5
;;

let english_short_months =
  [ "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" ]
;;

(* French and the Cyrillic locales group digits with a space. CLDR uses a narrow no-break
   space (U+202F) for French; U+00A0 is used for all three because the standard PDF fonts
   have no glyph for U+202F. *)
let nbsp = "\u{00A0}"

let formats : Locale.t -> Number_format.t * Date_format.t * string list * string
  = function
  | En ->
    ( { decimal_separator = "."
      ; group_separator = ","
      ; min_grouping_digits = 1
      ; currency_first = true
      }
    , { pattern = "d MMM yyyy"; short_months = english_short_months }
    , [ "January"
      ; "February"
      ; "March"
      ; "April"
      ; "May"
      ; "June"
      ; "July"
      ; "August"
      ; "September"
      ; "October"
      ; "November"
      ; "December"
      ]
    , "{month} {year}" )
  | De ->
    ( { decimal_separator = ","
      ; group_separator = "."
      ; min_grouping_digits = 1
      ; currency_first = false
      }
    , { pattern = "dd.MM.yyyy"; short_months = english_short_months }
    , [ "Januar"
      ; "Februar"
      ; "März"
      ; "April"
      ; "Mai"
      ; "Juni"
      ; "Juli"
      ; "August"
      ; "September"
      ; "Oktober"
      ; "November"
      ; "Dezember"
      ]
    , "{month} {year}" )
  | Fr ->
    ( { decimal_separator = ","
      ; group_separator = nbsp
      ; min_grouping_digits = 1
      ; currency_first = false
      }
    , { pattern = "dd/MM/yyyy"; short_months = english_short_months }
    , [ "janvier"
      ; "février"
      ; "mars"
      ; "avril"
      ; "mai"
      ; "juin"
      ; "juillet"
      ; "août"
      ; "septembre"
      ; "octobre"
      ; "novembre"
      ; "décembre"
      ]
    , "{month} {year}" )
  | Es ->
    (* Spanish groups only from five integer digits (CLDR minimumGroupingDigits = 2). *)
    ( { decimal_separator = ","
      ; group_separator = "."
      ; min_grouping_digits = 2
      ; currency_first = false
      }
    , { pattern = "dd/MM/yyyy"; short_months = english_short_months }
    , [ "enero"
      ; "febrero"
      ; "marzo"
      ; "abril"
      ; "mayo"
      ; "junio"
      ; "julio"
      ; "agosto"
      ; "septiembre"
      ; "octubre"
      ; "noviembre"
      ; "diciembre"
      ]
    , "{month} de {year}" )
  | Ru ->
    ( { decimal_separator = ","
      ; group_separator = nbsp
      ; min_grouping_digits = 1
      ; currency_first = false
      }
    , { pattern = "dd.MM.yyyy"; short_months = english_short_months }
    , [ "январь"
      ; "февраль"
      ; "март"
      ; "апрель"
      ; "май"
      ; "июнь"
      ; "июль"
      ; "август"
      ; "сентябрь"
      ; "октябрь"
      ; "ноябрь"
      ; "декабрь"
      ]
    , "{month} {year}" )
  | Ky ->
    ( { decimal_separator = ","
      ; group_separator = nbsp
      ; min_grouping_digits = 1
      ; currency_first = false
      }
    , { pattern = "dd.MM.yyyy"; short_months = english_short_months }
    , [ "январь"
      ; "февраль"
      ; "март"
      ; "апрель"
      ; "май"
      ; "июнь"
      ; "июль"
      ; "август"
      ; "сентябрь"
      ; "октябрь"
      ; "ноябрь"
      ; "декабрь"
      ]
    , "{year}-ж., {month}" )
;;

let builtins =
  Locale.all
  |> List.map ~f:(fun locale ->
    let number, date, months, month_title = formats locale in
    let texts =
      List.map rows ~f:(fun (label, texts) -> label, List.nth_exn texts (column locale))
    in
    locale, create ~locale ~texts ~number ~date ~months ~month_title |> ok_exn)
;;

let builtin locale = List.Assoc.find_exn builtins locale ~equal:Locale.equal
