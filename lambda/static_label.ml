[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
include Numbers.Int

type t = int

let to_string = Int.to_string

let format fmt t = Format.fprintf fmt "%d" t

let fail = 0

type sequence = { mutable next : t }

let make_sequence () = { next = 1 }

let reset seq = seq.next <- 1

let get_and_incr seq =
  let res = seq.next in
  seq.next <- succ seq.next;
  res

let of_int_unsafe i = i