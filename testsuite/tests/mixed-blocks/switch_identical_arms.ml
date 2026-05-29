(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 {
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }
*)

(* Regression test for the [to_cmm] switch-arm collapsing optimisation.

   When the match compiler produces or-patterns over constructors with
   different layouts, the arms may emit syntactically distinct Lambda
   primitives (e.g. [Pmixedfield]s on constructors with different total field
   counts) that nevertheless resolve to byte-for-byte identical Cmm. [to_cmm]
   merges such equivalent switch arms, collapsing a switch all of whose arms
   compute the same value into just the scrutinee followed by that value. This
   test checks that the collapsing does not change the computed result. *)

type unboxed = #{ foo : int; bar : int }

(* Binary case: lowered to an if-then-else. Field 0 is at the same physical
   offset in both constructors, so the two arms produce identical Cmm loads. *)
module Binary = struct
  type t =
    | Short of { length : int; tail : unboxed }
    | Long of { length : int; tail : unboxed; shift : int }

  let length (Short { length; _ } | Long { length; _ }) = length
end

(* N-ary case: lowered to a general switch. All three arms read field 0, so the
   switch collapses to a single case after deduplication. *)
module Nary = struct
  type t =
    | A of { x : int; a : unboxed }
    | B of { x : int; a : unboxed; y : int }
    | C of { x : int; a : unboxed; y : int; z : int }

  let get_x (A { x; _ } | B { x; _ } | C { x; _ }) = x
end

(* Product result: the bound value is an unboxed pair, so each arm performs two
   loads. Both arms read it from the same offsets, so the pair of loads is
   identical between arms and the switch still collapses correctly. *)
module Product = struct
  type t =
    | Short of { length : int; tail : unboxed }
    | Long of { length : int; tail : unboxed; shift : int }

  let tail_foo (Short { tail; _ } | Long { tail; _ }) =
    let #{ foo; bar = _ } = tail in
    foo
end

let () =
  let zero = #{ foo = 7; bar = 8 } in
  assert (Binary.length (Binary.Short { length = 1; tail = zero }) = 1);
  assert (
    Binary.length (Binary.Long { length = 2; tail = zero; shift = 9 }) = 2);
  assert (Nary.get_x (Nary.A { x = 10; a = zero }) = 10);
  assert (Nary.get_x (Nary.B { x = 11; a = zero; y = 0 }) = 11);
  assert (Nary.get_x (Nary.C { x = 12; a = zero; y = 0; z = 0 }) = 12);
  assert (Product.tail_foo (Product.Short { length = 3; tail = zero }) = 7);
  assert (
    Product.tail_foo (Product.Long { length = 4; tail = zero; shift = 5 }) = 7);
  print_string "ok\n"
