open! Core
module Amount = Money.Amount

module Header = struct
  type t =
    { institution : string option
    ; account_name : string
    ; holder : string option
    ; details : (string * string) list
    }
  [@@deriving sexp, compare, equal]

  let create ?institution ?holder ?(details = []) ~account_name () =
    { institution; account_name; holder; details }
  ;;
end

module Mark = struct
  type t =
    | Catalogue
    | Text of string
    | No_mark
  [@@deriving sexp, compare, equal]
end

module Font_choice = struct
  type t =
    | Auto
    | Embedded
  [@@deriving sexp, compare, equal]
end

(* A4 in points, and the layout grid. *)
let page_width = 595.276
let page_height = 841.89
let margin = 42.5
let content_width = page_width -. (2. *. margin)
let content_bottom = 62.

(* Table columns: date, description, money in, money out, balance. The amount columns are
   right-aligned at their right edge. *)
let date_width = 62.
let amount_width = 74.
let balance_width = 82.

let description_width =
  content_width -. date_width -. (2. *. amount_width) -. balance_width
;;

let x_date = margin
let x_description = margin +. date_width
let right_in = x_description +. description_width +. amount_width
let right_out = right_in +. amount_width
let right_balance = right_out +. balance_width

type color =
  | Gray of float
  | Rgb of float * float * float

let ink = Gray 0.
let muted = Gray 0.4
let mark_color = Rgb (0.65, 0.1, 0.1)

(* One page's content stream, built up as drawing proceeds. *)
type page = { ops : Buffer.t }

type layout =
  { font : Pdf_font.t
  ; catalogue : Catalogue.t
  ; mutable pages : page list (** newest first *)
  ; mutable y : float (** baseline of the next line on the current page *)
  }

let current l = List.hd_exn l.pages
let num = Pdf_writer.number

let color_ops = function
  | Gray g -> sprintf "%s g %s G" (num g) (num g)
  | Rgb (r, g, b) ->
    sprintf "%s %s %s rg %s %s %s RG" (num r) (num g) (num b) (num r) (num g) (num b)
;;

let width l ~bold ~size s = Pdf_font.width l.font ~bold ~size s

let text l ?(bold = false) ?(size = 9.) ?(color = ink) ~x ~y s =
  if not (String.is_empty s)
  then
    Buffer.add_string
      (current l).ops
      (sprintf
         "BT %s %s 1 0 0 1 %s %s Tm %s Tj ET\n"
         (Pdf_font.select l.font ~bold ~size)
         (color_ops color)
         (num x)
         (num y)
         (Pdf_font.show l.font s))
;;

let text_right l ?bold ?(size = 9.) ?color ~right ~y s =
  let w = width l ~bold:(Option.value bold ~default:false) ~size s in
  text l ?bold ~size ?color ~x:(right -. w) ~y s
;;

let rule l ?(gray = 0.8) ?(line = 0.5) ~x1 ~x2 ~y () =
  Buffer.add_string
    (current l).ops
    (sprintf
       "%s G %s w %s %s m %s %s l S\n"
       (num gray)
       (num line)
       (num x1)
       (num y)
       (num x2)
       (num y))
;;

let box l ~x ~y ~w ~h ~gray =
  Buffer.add_string
    (current l).ops
    (sprintf "%s g %s %s %s %s re f\n" (num gray) (num x) (num y) (num w) (num h))
;;

(* Cuts [s] to fit [max] points, ending in an ellipsis when cut. *)
let fit l ?(bold = false) ~size ~max s =
  if Float.( <= ) (width l ~bold ~size s) max
  then s
  else (
    let ellipsis = "\u{2026}" in
    let rec go chars =
      match chars with
      | [] -> ellipsis
      | _ :: rest ->
        let chars = rest in
        let candidate = Utf8.of_uchars (List.rev chars) ^ ellipsis in
        if Float.( <= ) (width l ~bold ~size candidate) max then candidate else go rest
    in
    go (List.rev (Utf8.uchars s)))
;;

let new_page l =
  l.pages <- { ops = Buffer.create 8192 } :: l.pages;
  l.y <- page_height -. margin -. 10.
;;

let label l lbl = Catalogue.text l.catalogue lbl
let amount l a = Catalogue.format_amount l.catalogue a
let amount_cur l a = Catalogue.format_amount ~currency:true l.catalogue a

let transactions_header l =
  let y = l.y in
  let t ~x s = text l ~bold:true ~size:8. ~color:muted ~x ~y s in
  let r ~right s =
    text_right
      l
      ~bold:true
      ~size:8.
      ~color:muted
      ~right
      ~y
      (fit l ~bold:true ~size:8. ~max:(amount_width -. 6.) s)
  in
  t ~x:x_date (label l Date);
  t ~x:x_description (label l Description);
  r ~right:right_in (label l Money_in);
  r ~right:right_out (label l Money_out);
  r ~right:right_balance (label l Balance);
  rule l ~gray:0.5 ~x1:margin ~x2:(margin +. content_width) ~y:(y -. 5.) ();
  l.y <- y -. 17.
;;

let pending_header l =
  let y = l.y in
  text l ~bold:true ~size:8. ~color:muted ~x:x_date ~y (label l Date);
  text l ~bold:true ~size:8. ~color:muted ~x:x_description ~y (label l Description);
  text_right l ~bold:true ~size:8. ~color:muted ~right:right_balance ~y (label l Amount);
  rule l ~gray:0.5 ~x1:margin ~x2:(margin +. content_width) ~y:(y -. 5.) ();
  l.y <- y -. 17.
;;

(* Starts a new page with the table header when [height] more points do not fit. *)
let ensure l ~height ~header =
  if Float.( < ) (l.y -. height) content_bottom
  then (
    new_page l;
    header l)
;;

let row l ~header ~date ~description ~secondary ~cells =
  let height = if String.is_empty secondary then 15. else 24. in
  ensure l ~height ~header;
  let y = l.y in
  text l ~x:x_date ~y date;
  text l ~x:x_description ~y (fit l ~size:9. ~max:(description_width -. 8.) description);
  if not (String.is_empty secondary)
  then
    text
      l
      ~size:7.
      ~color:muted
      ~x:x_description
      ~y:(y -. 9.5)
      (fit l ~size:7. ~max:(description_width -. 8.) secondary);
  List.iter cells ~f:(fun (right, s) -> text_right l ~right ~y s);
  rule l ~gray:0.88 ~x1:margin ~x2:(margin +. content_width) ~y:(y -. height +. 9.) ();
  l.y <- y -. height
;;

let description l (a : Annotation.t) code =
  if String.is_empty a.description
  then Option.value a.counterparty ~default:(Catalogue.code_label l.catalogue code)
  else a.description
;;

let secondary parts =
  String.concat ~sep:" \u{00B7} " (List.filter parts ~f:(Fn.non String.is_empty))
;;

let header_block l (h : Header.t) (s : Statement.t) =
  let top = l.y in
  text l ~bold:true ~size:18. ~x:margin ~y:top (label l Statement_title);
  Option.iter h.institution ~f:(fun i ->
    text_right l ~bold:true ~size:10. ~right:(margin +. content_width) ~y:top i);
  (* Left: the account; right: period, currency, as of. *)
  let left = ref (top -. 30.) in
  let line ?(bold = false) ?(size = 9.) ?(color = ink) s =
    text
      l
      ~bold
      ~size
      ~color
      ~x:margin
      ~y:!left
      (fit l ~bold ~size ~max:(content_width /. 2.) s);
    left := !left -. (size +. 4.)
  in
  line ~bold:true ~size:11. h.account_name;
  Option.iter h.holder ~f:(fun n -> line n);
  List.iter h.details ~f:(fun (k, v) -> line (k ^ ": " ^ v));
  line
    ~color:muted
    ~size:8.
    (label l Account ^ ": " ^ Ledger.Account_id.to_string s.account);
  let x = margin +. (content_width /. 2.) +. 20. in
  let right = ref (top -. 30.) in
  let pair k v =
    text l ~size:8. ~color:muted ~x ~y:!right k;
    text
      l
      ~size:9.
      ~x:(x +. 90.)
      ~y:!right
      (fit l ~size:9. ~max:((content_width /. 2.) -. 110.) v);
    right := !right -. 13.
  in
  pair (label l Period) (Catalogue.format_period l.catalogue s.period);
  pair (label l Currency) (Money.Currency.to_string s.currency);
  pair (label l As_of) (Catalogue.format_time l.catalogue s.as_of);
  l.y <- Float.min !left !right -. 10.
;;

let summary_block l (s : Statement.t) =
  let rows_left =
    [ label l Opening_balance, amount_cur l s.totals.opening, false
    ; label l Money_in, amount_cur l s.totals.money_in, false
    ; label l Money_out, amount_cur l (Amount.neg s.totals.money_out), false
    ; label l Closing_balance, amount_cur l s.totals.closing, true
    ]
  in
  let rows_right =
    match s.basis with
    | Netted ->
      [ label l Held_for_pending, amount_cur l s.held_for_pending, false
      ; label l Available_at_end, amount_cur l s.ledger_balance_at_end, true
      ]
    | Posting -> []
  in
  let half = content_width /. 2. in
  let fits rows ~room =
    List.for_all rows ~f:(fun (k, v, bold) ->
      Float.( <= ) (width l ~bold ~size:9. k +. width l ~bold ~size:9. v +. 10.) room)
  in
  (* Two columns side by side when every row fits in half the width; otherwise (long
     labels, as in Kyrgyz) one column with the netted rows after the totals. *)
  let columns =
    if fits rows_left ~room:(half -. 20.) && fits rows_right ~room:(half -. 20.)
    then
      [ rows_left, margin +. 8., margin +. half -. 12.
      ; rows_right, margin +. half +. 12., margin +. content_width -. 8.
      ]
    else [ rows_left @ rows_right, margin +. 8., margin +. content_width -. 8. ]
  in
  let rows =
    List.fold columns ~init:0 ~f:(fun acc (r, _, _) -> Int.max acc (List.length r))
  in
  let height = 14. +. (14. *. Float.of_int rows) in
  let top = l.y in
  box l ~x:margin ~y:(top -. height) ~w:content_width ~h:height ~gray:0.95;
  text
    l
    ~bold:true
    ~size:8.
    ~color:muted
    ~x:(margin +. 8.)
    ~y:(top -. 12.)
    (label l Summary);
  List.iter columns ~f:(fun (rows, x, right) ->
    List.iteri rows ~f:(fun i (k, v, bold) ->
      let y = top -. 26. -. (14. *. Float.of_int i) in
      let room = right -. x -. width l ~bold ~size:9. v -. 10. in
      text l ~bold ~size:9. ~x ~y (fit l ~bold ~size:9. ~max:room k);
      text_right l ~bold ~size:9. ~right ~y v));
  l.y <- top -. height -. 14.;
  let note =
    match s.basis with
    | Netted -> label l Basis_netted
    | Posting -> label l Basis_posting
  in
  text l ~size:7.5 ~color:muted ~x:margin ~y:l.y (fit l ~size:7.5 ~max:content_width note);
  l.y <- l.y -. 24.
;;

let heading l s =
  ensure l ~height:50. ~header:(fun _ -> ());
  text l ~bold:true ~size:11. ~x:margin ~y:l.y s;
  l.y <- l.y -. 18.
;;

let transactions l (s : Statement.t) =
  heading l (label l Transactions);
  transactions_header l;
  if List.is_empty s.lines
  then (
    text l ~size:9. ~color:muted ~x:x_description ~y:l.y (label l No_transactions);
    l.y <- l.y -. 18.);
  List.iter s.lines ~f:(fun line ->
    let in_, out =
      match line.direction with
      | In -> amount l line.amount, ""
      | Out -> "", amount l line.amount
    in
    row
      l
      ~header:transactions_header
      ~date:(Catalogue.format_date l.catalogue line.business_date)
      ~description:(description l line.annotation line.code)
      ~secondary:
        (secondary
           [ Catalogue.code_label l.catalogue line.code
           ; line.reference
           ; Option.value_map line.held_since ~default:"" ~f:(fun t ->
               Catalogue.render
                 l.catalogue
                 Authorised_on
                 [ ( "date"
                   , Catalogue.format_date
                       l.catalogue
                       (Time_ns.to_date t ~zone:Time_float.Zone.utc) )
                 ])
           ; (if line.reversal then label l Reversal_note else "")
           ])
      ~cells:[ right_in, in_; right_out, out; right_balance, amount l line.balance ])
;;

let pending l (s : Statement.t) =
  if not (List.is_empty s.pending)
  then (
    l.y <- l.y -. 10.;
    heading l (label l Pending_title);
    pending_header l;
    List.iter s.pending ~f:(fun p ->
      row
        l
        ~header:pending_header
        ~date:(Catalogue.format_date l.catalogue p.business_date)
        ~description:(description l p.annotation p.code)
        ~secondary:(secondary [ Catalogue.code_label l.catalogue p.code; p.reference ])
        ~cells:[ right_balance, amount l (Amount.neg p.amount) ]))
;;

let footer l ~mark ~page ~pages buffer =
  let saved = l.pages in
  l.pages <- [ { ops = buffer } ];
  rule l ~gray:0.7 ~x1:margin ~x2:(margin +. content_width) ~y:44. ();
  Option.iter mark ~f:(fun m ->
    text
      l
      ~bold:true
      ~size:8.
      ~color:mark_color
      ~x:margin
      ~y:30.
      (fit l ~bold:true ~size:8. ~max:(content_width -. 90.) m));
  text_right
    l
    ~size:8.
    ~color:muted
    ~right:(margin +. content_width)
    ~y:30.
    (Catalogue.render
       l.catalogue
       Page
       [ "page", Int.to_string page; "pages", Int.to_string pages ]);
  l.pages <- saved
;;

(* Every string the document may show, to decide whether the standard fonts suffice. *)
let all_text catalogue ~mark (h : Header.t) (s : Statement.t) =
  List.map Catalogue.Label.all ~f:(Catalogue.text catalogue)
  @ [ Catalogue.format_period catalogue s.period
    ; Catalogue.format_time catalogue s.as_of
    ; Catalogue.format_amount catalogue s.totals.closing
    ; h.account_name
    ]
  @ Option.to_list h.institution
  @ Option.to_list h.holder
  @ List.concat_map h.details ~f:(fun (k, v) -> [ k; v ])
  @ Option.to_list mark
  @ List.concat_map s.lines ~f:(fun l ->
    l.reference :: l.annotation.description :: Option.to_list l.annotation.counterparty)
  @ List.concat_map s.pending ~f:(fun p ->
    p.reference :: p.annotation.description :: Option.to_list p.annotation.counterparty)
;;

let render
  ?(catalogue = Catalogue.builtin En)
  ?(mark = Mark.Catalogue)
  ?(font = Font_choice.Auto)
  ~header
  (s : Statement.t)
  =
  let mark =
    match mark with
    | Catalogue -> Some (Catalogue.text catalogue Not_a_real_statement)
    | Text t -> Some t
    | No_mark -> None
  in
  let font =
    match font with
    | Auto -> Pdf_font.choose (all_text catalogue ~mark header s)
    | Embedded -> Pdf_font.embedded ()
  in
  let l = { font; catalogue; pages = []; y = 0. } in
  new_page l;
  header_block l header s;
  summary_block l s;
  transactions l s;
  pending l s;
  let pages = List.rev l.pages in
  let count = List.length pages in
  List.iteri pages ~f:(fun i p -> footer l ~mark ~page:(i + 1) ~pages:count p.ops);
  let w = Pdf_writer.create () in
  let catalog = Pdf_writer.reserve w in
  let pages_obj = Pdf_writer.reserve w in
  let resources = Pdf_writer.reserve w in
  let kids =
    List.map pages ~f:(fun p ->
      let contents = Pdf_writer.add_stream w (Buffer.contents p.ops) in
      Pdf_writer.add
        w
        (sprintf
           "<< /Type /Page /Parent %s /MediaBox [0 0 %s %s] /Resources %s /Contents %s >>"
           (Pdf_writer.ref_ pages_obj)
           (num page_width)
           (num page_height)
           (Pdf_writer.ref_ resources)
           (Pdf_writer.ref_ contents)))
  in
  let fonts = Pdf_font.write font w in
  Pdf_writer.define
    w
    resources
    (sprintf "<< /Font << %s >> /ProcSet [/PDF /Text] >>" (String.concat ~sep:" " fonts));
  Pdf_writer.define
    w
    pages_obj
    (sprintf
       "<< /Type /Pages /Kids [%s] /Count %d >>"
       (String.concat ~sep:" " (List.map kids ~f:Pdf_writer.ref_))
       count);
  Pdf_writer.define
    w
    catalog
    (sprintf "<< /Type /Catalog /Pages %s >>" (Pdf_writer.ref_ pages_obj));
  let info =
    Pdf_writer.add
      w
      (sprintf
         "<< /Title %s /Producer (statements) >>"
         (Pdf_writer.text_string
            (sprintf
               "%s %s %s"
               (Catalogue.text catalogue Statement_title)
               (Money.Currency.to_string s.currency)
               (Catalogue.format_period catalogue s.period))))
  in
  Pdf_writer.to_string w ~root:catalog ~info ()
;;
