(* TEST
   flambda2;
   expect.opt with dump-simplify;
 *)

[@@@flambda_o3]

type t = A | B | C | D

let all_boolean_simplifications x = function
  (* identity, not, invariant *)
  | true -> #(true, false, x)
  | false -> #(false, true, x)
[%%expect_fexpr Simplify{|
let code all_boolean_simplifications_0 deleted in
let code loopify(never) size(2) newer_version_of(all_boolean_simplifications_0)
      all_boolean_simplifications_0_1 (x, param : imm tagged)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : val * val * val =
  let not_scrutinee = %boolean_not (param) in
  cont k (param, not_scrutinee, x)
in
let $camlTOP3__all_boolean_simplifications_1 =
  closure all_boolean_simplifications_0_1 @all_boolean_simplifications
    &toplevel.alloc_region
in
let $camlTOP3 = Block 0 ($camlTOP3__all_boolean_simplifications_1) in
cont done ($camlTOP3)
|}]

let all_large_simplifications x = function
  (* identity, invariant, affine, lookup table *)
  | A -> #(0, x, 1, None)
  | B -> #(1, x, 2, None)
  | C -> #(2, x, 3, Some 0)
  | D -> #(3, x, 4, Some 0)
[%%expect_fexpr Simplify{|
let $camlTOP4__const_block85 = Block 0 (0) in
let code all_large_simplifications_2 deleted in
let $camlTOP4__switch_block118 =
  Value_array [|0;
  0;
  $camlTOP4__const_block85;
  $camlTOP4__const_block85|]
in
let code loopify(never) size(4) newer_version_of(all_large_simplifications_2)
      all_large_simplifications_2_1 (x, param : imm tagged)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : val * val * val * val =
  let final_arg = %int_barith.add (1, param) in
  let arg = %array_load ($camlTOP4__switch_block118, param) in
  cont k (param, x, final_arg, arg)
in
let $camlTOP4__all_large_simplifications_3 =
  closure all_large_simplifications_2_1 @all_large_simplifications
    &toplevel.alloc_region
in
let $camlTOP4 = Block 0 ($camlTOP4__all_large_simplifications_3) in
cont done ($camlTOP4)
|}]

let shared_affine_functions = function
  (* x -> 2x, 2x + 1 *)
  | A -> #(0, 1)
  | B -> #(2, 3)
  | C -> #(4, 5)
  | D -> #(6, 7)
[%%expect_fexpr Simplify{|
let code shared_affine_functions_4 deleted in
let code loopify(never) size(7) newer_version_of(shared_affine_functions_4)
      shared_affine_functions_4_1 (param : imm tagged)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : val * val =
  let scaled_arg = %int_barith.mul (param, 2) in
  let final_arg = %int_barith.add (1, scaled_arg) in
  cont k (scaled_arg, final_arg)
in
let $camlTOP5__shared_affine_functions_5 =
  closure shared_affine_functions_4_1 @shared_affine_functions
    &toplevel.alloc_region
in
let $camlTOP5 = Block 0 ($camlTOP5__shared_affine_functions_5) in
cont done ($camlTOP5)
|}]

let shared_lookup_table = function
  (* First arg to prevent simplif from messing with us. *)
  | A -> #(0, None, None)
  | B -> #(1, None, None)
  | C -> #(2, Some 0, Some 0)
  | D -> #(3, Some 0, Some 0)
[%%expect_fexpr Simplify{|
let $camlTOP6__const_block210 = Block 0 (0) in
let code shared_lookup_table_6 deleted in
let $camlTOP6__switch_block236 =
  Value_array [|0;
  0;
  $camlTOP6__const_block210;
  $camlTOP6__const_block210|]
in
let code loopify(never) size(2) newer_version_of(shared_lookup_table_6)
      shared_lookup_table_6_1 (param : imm tagged)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : val * val * val =
  let arg = %array_load ($camlTOP6__switch_block236, param) in
  cont k (param, arg, arg)
in
let $camlTOP6__shared_lookup_table_7 =
  closure shared_lookup_table_6_1 @shared_lookup_table &toplevel.alloc_region
in
let $camlTOP6 = Block 0 ($camlTOP6__shared_lookup_table_7) in
cont done ($camlTOP6)
|}]

(* CR-someday bclement: this should be a single unboxed product array *)
let multiple_lookup_tables = function
  | A -> #(0, 1)
  | B -> #(1, 1)
  | C -> #(0, 0)
  | D -> #(1, 0)
[%%expect_fexpr Simplify{|
let code multiple_lookup_tables_8 deleted in
let $camlTOP7__switch_block289 = Value_array [|1; 1; 0; 0|] in
let $camlTOP7__switch_block287 = Value_array [|0; 1; 0; 1|] in
let code loopify(never) size(3) newer_version_of(multiple_lookup_tables_8)
      multiple_lookup_tables_8_1 (param : imm tagged)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : val * val =
  let arg = %array_load.`imm` ($camlTOP7__switch_block287, param) in
  let arg_1 = %array_load.`imm` ($camlTOP7__switch_block289, param) in
  cont k (arg, arg_1)
in
let $camlTOP7__multiple_lookup_tables_9 =
  closure multiple_lookup_tables_8_1 @multiple_lookup_tables
    &toplevel.alloc_region
in
let $camlTOP7 = Block 0 ($camlTOP7__multiple_lookup_tables_9) in
cont done ($camlTOP7)
|}]

let offset_lookup_table i =
  let t =
    (* Never [A], and simplify knows it. *)
    match i with
    | 0 -> B
    | 1 -> C
    | _ -> D
  in
  match t with
  | A -> assert false
  | B -> 0
  | C -> 2
  | D -> 1
[%%expect_fexpr Simplify{|
let code offset_lookup_table_10 deleted in
let $camlTOP8__switch_block359 = Value_array [|0; 2; 1|] in
let code loopify(never) size(30) newer_version_of(offset_lookup_table_10)
      offset_lookup_table_10_1 (i : imm tagged)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : imm tagged =
  (let prim = %int_comp.ne (i, 0) in
   switch prim
     | 0 -> k2 (1i)
     | 1 -> k3
     where k3 =
       let prim_1 = %int_comp.ne (i, 1) in
       switch prim_1
         | 0 -> k2 (2i)
         | 1 -> k2 (3i))
    where k2 (naked_immediate : imm) =
      let tagged_scrutinee = %tag_imm (naked_immediate) in
      let offset_scrutinee = %int_barith.add (tagged_scrutinee, -1) in
      let arg =
        %array_load.`imm` ($camlTOP8__switch_block359, offset_scrutinee)
      in
      cont k (arg)
in
let $camlTOP8__offset_lookup_table_11 =
  closure offset_lookup_table_10_1 @offset_lookup_table
    &toplevel.alloc_region
in
let $camlTOP8 = Block 0 ($camlTOP8__offset_lookup_table_11) in
cont done ($camlTOP8)
|}]

(* The test below ensures that we don't introduce a %boolean_not primitive that
   could legitimately be evaluated with an argument not in the {0, 1} set. *)

type x = X0 | X1 | X2

type u = Absent | Box_A of t

let poison_prevents_boolean_not b =
  let t =
    (* After variant unboxing, this becomes:

       switch b with
       | 0 -> goto k (0i, 1)
       | 1 -> goto k (1i, 0)
       | 2 -> goto k (1i, <poison>)

       The first arg is a lookup table, and the second is a boolean-not
       operation but only when restricted to its non-poison arms.

       It would be incorrect to transform the whole switch into:

       let arg0 = %array_load (lookup_table, b) in
       let arg1 = %boolean_not b in
       goto k (arg0, arg1)

     *)
    match b with
    | X0 -> Box_A B
    | X1 -> Box_A A
    | X2 -> Absent
  in
  match t with
  | Absent -> A
  | Box_A t -> t
[%%expect_fexpr Simplify{|
let code poison_prevents_boolean_not_12 deleted in
let $camlTOP11__switch_block455 = Int_array [|0; 0; 1|] in
let code loopify(never) size(14) newer_version_of(poison_prevents_boolean_not_12)
      poison_prevents_boolean_not_12_1 (b : imm tagged)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : imm tagged =
  (let arg = %array_load.`int` ($camlTOP11__switch_block455, b) in
   let final_arg = %int_barith.sub (1, b) in
   cont k2 (arg, final_arg))
    where k2 (is_int : imm, unboxed_field_0_0) =
      switch is_int
        | 0 -> k (unboxed_field_0_0)
        | 1 -> k (0)
in
let $camlTOP11__poison_prevents_boolean_not_13 =
  closure poison_prevents_boolean_not_12_1 @poison_prevents_boolean_not
    &toplevel.alloc_region
in
let $camlTOP11 = Block 0 ($camlTOP11__poison_prevents_boolean_not_13) in
cont done ($camlTOP11)
|}]
