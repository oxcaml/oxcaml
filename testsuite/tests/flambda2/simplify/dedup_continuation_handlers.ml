(* TEST
   flambda2;
   expect.opt with dump-simplify;
 *)

(* These have different lambda primitives but the same flambda primitives *)

type t0 =
  | B of { mutable a : string }
  | C of { mutable a : string; mutable d : #(int * int) }

let get0 (x : t0) =
  match x with
  | B r -> r.a
  | C r -> r.a
;;
[%%expect_fexpr Simplify{|
let code get0_0 deleted in
let code loopify(never) size(2) newer_version_of(get0_0)
      get0_0_1 (x : val) my_closure &my_alloc_region my_depth -> k * k1 : val =
  let Pfield = %block_load.mut.[`0`] (x) in
  cont k (Pfield)
in
let $camlTOP2__get0_1 = closure get0_0_1 @get0 &toplevel.alloc_region in
let $camlTOP2 = Block 0 ($camlTOP2__get0_1) in
cont done ($camlTOP2)
|}]

let set0 (x : t0) a =
  match x with
  | B r -> r.a <- a
  | C r -> r.a <- a
[%%expect_fexpr Simplify{|
let code set0_2 deleted in
let code loopify(never) size(5) newer_version_of(set0_2)
      set0_2_1 (x : val, a : val)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : imm tagged =
  let Psetfield = %block_set.[`0`] (x, a) in
  cont k (0)
in
let $camlTOP3__set0_3 = closure set0_2_1 @set0 &toplevel.alloc_region in
let $camlTOP3 = Block 0 ($camlTOP3__set0_3) in
cont done ($camlTOP3)
|}]
;;

(* These have the same lambda primitives but the sharing is obscured by a
   function call *)

type _ t1 =
  | A : int -> [ `A ] t1
  | B : int -> [ `B ] t1

let get1 =
  let[@inline] get_a (A n : [ `A ] t1) = n in
  let[@inline] get_b (B n : [ `B ] t1) = n in
  fun (type a) (x : a t1) ->
    match x with
    | A _ as x -> get_a x
    | B _ as x -> get_b x
;;
[%%expect_fexpr Simplify{|
let code `fn[:8,2--97]_6` deleted in
let code loopify(never) size(2) newer_version_of(`fn[:8,2--97]_6`)
      `fn[:8,2--97]_6_1` (x : [ 0 of imm tagged |1 of imm tagged ])
        my_closure &my_alloc_region my_depth
        -> k * k1
        : imm tagged =
  let Pfield = %block_load.[`0`] (x) in
  cont k (Pfield)
in
let $`camlTOP5__fn[:8,2--97]_9` =
  closure `fn[:8,2--97]_6_1` @`fn[:8,2--97]` &toplevel.alloc_region
in
let $camlTOP5 = Block 0 ($`camlTOP5__fn[:8,2--97]_9`) in
cont done ($camlTOP5)
|}]
