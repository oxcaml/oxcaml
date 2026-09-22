(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Ryan Tjoa, Jane Street, New York                   *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Cmm_peephole_engine
open Rule

let var name : _ term = Var (create_var Expr name)

let x = var "x"

let y = var "y"

let z = var "z"

let a = var "a"

let b = var "b"

(* A constant parameter, as matched on the left-hand side and as used in
   conditions and on the right-hand side. *)
let const name =
  let v = create_var Natint name in
  Const v, I_var v

let c, c' = const "c"

let c1, c1' = const "c1"

let c2, c2' = const "c2"

let m, m' = const "m"

let k i = Const i

let il n = I_lit n

let ( +: ) l r = Apply (Add, l, r)

let ( -: ) l r = Apply (Sub, l, r)

let ( *: ) l r = Apply (Mul, l, r)

let ( &: ) l r = Apply (And, l, r)

let ( |: ) l r = Apply (Or, l, r)

let ( ^: ) l r = Apply (Xor, l, r)

let ( <<: ) l r = Apply (Lsl, l, r)

let ( >>: ) l r = Apply (Lsr, l, r)

let asr_ l r = Apply (Asr, l, r)

let not_ t = t ^: Lit (-1n)

let neg t = Lit 0n -: t

let ( +! ) l r = I_op (Add, l, r)

let ( -! ) l r = I_op (Sub, l, r)

let ( *! ) l r = I_op (Mul, l, r)

let ( &! ) l r = I_op (And, l, r)

let ( |! ) l r = I_op (Or, l, r)

let ( <<! ) l r = I_op (Lsl, l, r)

let ( >>! ) l r = I_op (Lsr, l, r)

let i_neg i = il 0n -! i

let word_bits = il (Nativeint.of_int word_bits)

let and_rules =
  [ create And (x, x) x;
    create And (x, not_ x) (Lit 0n);
    create And (x |: y, x) x;
    create And (a, not_ (a ^: b)) (a &: b);
    create And (a |: b, not_ (a &: b)) (a ^: b);
    create And (a |: not_ b, not_ a |: b) (not_ (a ^: b));
    create And (not_ x, not_ y) (not_ (x |: y));
    create And ~cond:(Eq (c1' &! c2', il 0n)) (x ^: c1, c2) (x &: k c2');
    create And ~cond:(Eq (c1' &! c2', il 0n)) (x |: c1, c2) (x &: k c2');
    create And
      ~cond:(Eq (m' |! (il 1n <<! c') -! il 1n, il (-1n)))
      (x <<: c, m)
      (x <<: k c');
    create And
      ~cond:(Eq (m' |! (il (-1n) <<! word_bits -! c'), il (-1n)))
      (x >>: c, m)
      (x >>: k c');
    (* Masking off the bits an arithmetic shift copied from the sign makes it a
       logical shift. *)
    create And
      ~cond:(All [Slt (il 0n, c'); Eq (m', il (-1n) >>! c')])
      (asr_ x c, m)
      (x >>: k c') ]

let or_rules =
  [ create Or (x, x) x;
    create Or (x, not_ x) (Lit (-1n));
    create Or (x &: y, x) x;
    create Or (x ^: y, y) (x |: y);
    create Or (a &: b, a ^: b) (a |: b);
    create Or (a &: not_ b, not_ a &: b) (a ^: b);
    create Or (not_ x, not_ y) (not_ (x &: y));
    create Or (not_ a, a ^: b) (not_ (a &: b));
    create Or (a &: b, not_ (a |: b)) (not_ (a ^: b));
    create Or (a ^: b, not_ (a |: b)) (not_ (a &: b)) ]

let xor_rules =
  [ create Xor (x, x) (Lit 0n);
    create Xor (x, not_ x) (Lit (-1n));
    create Xor (not_ a &: b, a) (a |: b);
    create Xor (not_ x +: y, Lit (-1n)) (x -: y);
    create Xor (not_ x -: y, Lit (-1n)) (x +: y);
    create Xor (neg x, Lit (-1n)) (x -: Lit 1n);
    create Xor (not_ x ^: y, Lit (-1n)) (x ^: y);
    create Xor (not_ x &: y, Lit (-1n)) (x |: not_ y);
    create Xor (not_ x |: y, Lit (-1n)) (x &: not_ y);
    create Xor (a, not_ a |: b) (not_ (a &: b)) ]

let add_rules =
  [ create Add (x, neg y) (x -: y);
    create Add (a -: b, b) a;
    create Add (x &: y, x |: y) (x +: y);
    create Add (x &: y, x ^: y) (x |: y);
    create Add (not_ x, not_ y) (Lit (-2n) -: (x +: y));
    create Add (x *: c, x) (x *: k (c' +! il 1n));
    create Add (x *: z, y *: z) ((x +: y) *: z) ]

let sub_rules =
  [ create Sub (x +: y, x) y;
    create Sub (x, x +: y) (neg y);
    create Sub (x, x -: y) y;
    create Sub (x -: y, x) (neg y);
    create Sub (x, neg y) (x +: y);
    create Sub (Lit 0n, x -: y) (y -: x);
    create Sub (Lit 0n, neg x) x;
    create Sub (Lit 0n, not_ x) (x +: Lit 1n);
    create Sub (x |: y, x &: y) (x ^: y);
    create Sub (x |: y, x ^: y) (x &: y);
    create Sub (x +: y, x &: y) (x |: y);
    create Sub (x +: y, x |: y) (x &: y);
    create Sub (not_ x, not_ y) (y -: x);
    create Sub (x *: c, x) (x *: k (c' -! il 1n));
    create Sub (x, x *: c) (x *: k (il 1n -! c'));
    create Sub (x *: z, y *: z) ((x -: y) *: z) ]

let lsl_rules =
  [ create Lsl ~cond:(Eq (m' <<! c', il (-1n) <<! c')) (x &: m, c) (x <<: k c');
    create Lsl (x *: c1, c2) (x *: k (c1' <<! c2'));
    (* Clearing the low bits with a pair of shifts is a mask. The mask [-1 << c]
       only fits a sign-extended 32-bit immediate for shifts below 32; for
       larger shifts the pair of shifts is cheaper. For [c = 1], [(x asr 1) lsl
       1] followed by [+ 1] re-tags an integer; [Cmm_helpers.ignore_low_bit_int]
       knows the rewritten form so that a subsequent untagging still cancels. *)
    create Lsl
      ~cond:(All [Slt (il 0n, c'); Slt (c', il 32n)])
      (x >>: c, c)
      (x &: k (il (-1n) <<! c'));
    create Lsl
      ~cond:(All [Slt (il 0n, c'); Slt (c', il 32n)])
      (asr_ x c, c)
      (x &: k (il (-1n) <<! c')) ]

(* For smaller shifts the mask does not fit an immediate operand, and the pair
   of shifts is cheaper. *)
let lsr_rules =
  [create Lsr ~cond:(Slt (il 32n, c')) (x <<: c, c) (x &: k (il (-1n) >>! c'))]

let mul_rules =
  [ create Mul (x *: c1, c2) (x *: k (c1' *! c2'));
    create Mul (x <<: c1, c2) (x *: k (c2' <<! c1'));
    create Mul (neg x, neg y) (x *: y);
    create Mul (neg x, c) (x *: k (i_neg c')) ]

let all =
  List.concat
    [ and_rules;
      or_rules;
      xor_rules;
      add_rules;
      sub_rules;
      lsl_rules;
      lsr_rules;
      mul_rules ]

let for_op op = List.filter (fun rule -> root_op rule = op) all
