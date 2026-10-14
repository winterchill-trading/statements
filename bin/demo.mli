(** A sample journal for trying the renderers: one customer account over a month with
    deposits, card payments (settled, cancelled and still pending), a payment out that was
    returned, and a Cyrillic payee name. *)

open! Core

val account : Ledger.Account_id.t
val records : unit -> Ledger.Record.t list

(** Descriptions and counterparties for the sample entries. *)
val annotate : Ledger.Entry.t -> Statements.Annotation.t
