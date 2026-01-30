(* TEST
   include stdlib_upstream_compatible;
   expect;
*)

(* Test 1: box_ on unboxed types - the manifest is stored as Tbox but
   expands to the boxed type during unification *)

type t1 = float# box_;;
[%%expect{|
type t1 = float# box_
|}]

type t2 = int32# box_;;
[%%expect{|
type t2 = int32# box_
|}]

type t3 = int64# box_;;
[%%expect{|
type t3 = int64# box_
|}]

type t4 = nativeint# box_;;
[%%expect{|
type t4 = nativeint# box_
|}]

(* Test 2: box_ through type aliases - the key test showing that
   u box_ where u = float# is equal to float *)

type u = float#
type t = u box_;;
[%%expect{|
type u = float#
type t = u box_
|}]

(* This is the key test: t unifies with float because u box_ expands to float *)
let f (x : t) : float = x;;
[%%expect{|
val f : t -> float = <fun>
|}]

let g (x : float) : t = x;;
[%%expect{|
val g : float -> t = <fun>
|}]

(* Test 3: Direct float# box_ also unifies with float *)

let h (x : float# box_) : float = x;;
[%%expect{|
val h : float# box_ -> float = <fun>
|}]

let i (x : float) : float# box_ = x;;
[%%expect{|
val i : float -> float# box_ = <fun>
|}]

(* Test 4: box_ types unify with themselves in function types *)

let eq_box (x : int box_) (y : int box_) = x = y;;
[%%expect{|
val eq_box : int box_ -> int box_ -> bool = <fun>
|}]

let eq_float_box (x : float# box_) (y : float# box_) = x = y;;
[%%expect{|
val eq_float_box : float# box_ -> float# box_ -> bool = <fun>
|}]

(* Test 5: box_ in module signatures *)

module type S = sig
  type t = float# box_
  val x : t
end;;
[%%expect{|
module type S = sig type t = float# box_ val x : t end
|}]

module M : S = struct
  type t = float# box_
  let x = 1.0
end;;
[%%expect{|
module M : S
|}]

(* Test 6: Using the module's type *)

let use_m : float = M.x;;
[%%expect{|
val use_m : float = 1.
|}]

(* Test 7: int32# box_ = int32 *)

type t32 = int32# box_;;
[%%expect{|
type t32 = int32# box_
|}]

let f32 (x : t32) : int32 = x;;
[%%expect{|
val f32 : t32 -> int32 = <fun>
|}]

(* Test 8: int64# box_ = int64 *)

type t64 = int64# box_;;
[%%expect{|
type t64 = int64# box_
|}]

let f64 (x : t64) : int64 = x;;
[%%expect{|
val f64 : t64 -> int64 = <fun>
|}]

(* Test 9: nativeint# box_ = nativeint *)

type tnat = nativeint# box_;;
[%%expect{|
type tnat = nativeint# box_
|}]

let fnat (x : tnat) : nativeint = x;;
[%%expect{|
val fnat : tnat -> nativeint = <fun>
|}]

(* Test 10: Polymorphic box_ with explicit jkind annotation *)

let check_boxed_by : type (a : float64). a -> a box_ -> unit =
  fun _ _ -> ();;
[%%expect{|
val check_boxed_by : ('a : float64). 'a -> 'a box_ -> unit = <fun>
|}]

type uf = float#
let test_check (u : uf) (f : float) = check_boxed_by u f;;
[%%expect{|
type uf = float#
val test_check : uf -> float -> unit = <fun>
|}]

(* Test 11: Multiple levels of type aliasing - the inner type of box_
   must be fully expanded to find the unboxed type *)

type f = float#
type g = f
let test_multi_alias (x : g box_) : float = x;;
[%%expect{|
type f = float#
type g = f
val test_multi_alias : g box_ -> float = <fun>
|}]

type h = g
let test_three_levels (x : h box_) : float = x;;
[%%expect{|
type h = g
val test_three_levels : h box_ -> float = <fun>
|}]

(* Test 12: box_ on types without unboxed versions - the boxed type
   should be equal to itself *)

type abstract_type
type boxed_abstract = abstract_type box_;;
[%%expect{|
type abstract_type
type boxed_abstract = abstract_type box_
|}]

(* Test 13: float32# box_ = float32 *)

type t_f32 = float32# box_;;
[%%expect{|
type t_f32 = float32# box_
|}]

let f_f32 (x : t_f32) : float32 = x;;
[%%expect{|
val f_f32 : t_f32 -> float32 = <fun>
|}]

let g_f32 (x : float32) : float32# box_ = x;;
[%%expect{|
val g_f32 : float32 -> float32# box_ = <fun>
|}]

(* Test 14: Implicit unboxed records - t# box_ = t
   Mixed records (not float-only) have implicit unboxed versions *)

type mixed_record = { i : int; s : string };;
[%%expect{|
type mixed_record = { i : int; s : string; }
|}]

let mixed_of_unboxed (p : mixed_record# box_) : mixed_record = p;;
[%%expect{|
val mixed_of_unboxed : mixed_record# box_ -> mixed_record = <fun>
|}]

let unboxed_of_mixed (p : mixed_record) : mixed_record# box_ = p;;
[%%expect{|
val unboxed_of_mixed : mixed_record -> mixed_record# box_ = <fun>
|}]

type umixed = mixed_record#
type boxed_umixed = umixed box_;;
[%%expect{|
type umixed = mixed_record#
type boxed_umixed = umixed box_
|}]

let convert_alias (x : boxed_umixed) : mixed_record = x;;
[%%expect{|
val convert_alias : boxed_umixed -> mixed_record = <fun>
|}]

(* Float-only records don't have unboxed versions *)
type float_point = { fx : float#; fy : float# };;
[%%expect{|
type float_point = { fx : float#; fy : float#; }
|}]

let float_point_no_unboxed (p : float_point# box_) : float_point = p;;
[%%expect{|
Line 1, characters 32-44:
1 | let float_point_no_unboxed (p : float_point# box_) : float_point = p;;
                                    ^^^^^^^^^^^^
Error: The type "float_point" has no unboxed version.
Hint: Float records don't get unboxed versions.
|}]

(* Test 15: Unboxed tuples - no corresponding boxed tuple, so box_ stays as-is *)

type ut = #(int * string);;
[%%expect{|
type ut = #(int * string)
|}]

type boxed_ut = ut box_;;
[%%expect{|
type boxed_ut = ut box_
|}]

let eq_ut (x : #(int * string) box_) (y : ut box_) = x = y;;
[%%expect{|
val eq_ut : #(int * string) box_ -> ut box_ -> bool = <fun>
|}]

(* Test 16: Additional jkinds - bits32, bits64, word *)

let check_bits32 : type (a : bits32). a -> a box_ -> unit =
  fun _ _ -> ();;
[%%expect{|
val check_bits32 : ('a : bits32). 'a -> 'a box_ -> unit = <fun>
|}]

type ui32 = int32#
let test_bits32 (u : ui32) (b : int32) = check_bits32 u b;;
[%%expect{|
type ui32 = int32#
val test_bits32 : ui32 -> int32 -> unit = <fun>
|}]

let check_bits64 : type (a : bits64). a -> a box_ -> unit =
  fun _ _ -> ();;
[%%expect{|
val check_bits64 : ('a : bits64). 'a -> 'a box_ -> unit = <fun>
|}]

type ui64 = int64#
let test_bits64 (u : ui64) (b : int64) = check_bits64 u b;;
[%%expect{|
type ui64 = int64#
val test_bits64 : ui64 -> int64 -> unit = <fun>
|}]

let check_word : type (a : word). a -> a box_ -> unit =
  fun _ _ -> ();;
[%%expect{|
val check_word : ('a : word). 'a -> 'a box_ -> unit = <fun>
|}]

type unat = nativeint#
let test_word (u : unat) (b : nativeint) = check_word u b;;
[%%expect{|
type unat = nativeint#
val test_word : unat -> nativeint -> unit = <fun>
|}]

(* Test 17: Parameterized box_ type alias *)

type ('a : float64) boxed = 'a box_;;
[%%expect{|
type ('a : float64) boxed = 'a box_
|}]

let use_boxed (x : float# boxed) : float = x;;
[%%expect{|
val use_boxed : float# boxed -> float = <fun>
|}]

type alias_float = float#
let use_boxed_alias (x : alias_float boxed) : float = x;;
[%%expect{|
type alias_float = float#
val use_boxed_alias : alias_float boxed -> float = <fun>
|}]

(* Test 18: Nested box_ - float# box_ expands to float, but float is not an
   unboxed version, so float box_ does NOT expand further to float.
   This tests that box_ only expands for actual unboxed versions. *)

type nested = float# box_ box_;;
[%%expect{|
type nested = float# box_ box_
|}]

(* nested = float# box_ box_ = float box_, which does NOT equal float *)
let nested_to_float (x : nested) : float = x;;
[%%expect{|
Line 1, characters 43-44:
1 | let nested_to_float (x : nested) : float = x;;
                                               ^
Error: This expression has type "nested" = "float box_"
       but an expression was expected of type "float"
|}]

(* But nested types still unify with each other *)
let nested_eq (x : nested) (y : float# box_ box_) = x = y;;
[%%expect{|
val nested_eq : nested -> float# box_ box_ -> bool = <fun>
|}]

(* Test 19: Type error cases *)

let mismatch1 (x : float# box_) : int = x;;
[%%expect{|
Line 1, characters 40-41:
1 | let mismatch1 (x : float# box_) : int = x;;
                                            ^
Error: This expression has type "float# box_" = "float"
       but an expression was expected of type "int"
|}]

let mismatch2 (x : int32# box_) : int64 = x;;
[%%expect{|
Line 1, characters 42-43:
1 | let mismatch2 (x : int32# box_) : int64 = x;;
                                              ^
Error: This expression has type "int32# box_" = "int32"
       but an expression was expected of type "int64"
|}]

type uf1 = float#
type uf2 = int64#
let mismatch3 (x : uf1 box_) : uf2 box_ = x;;
[%%expect{|
type uf1 = float#
type uf2 = int64#
Line 3, characters 42-43:
3 | let mismatch3 (x : uf1 box_) : uf2 box_ = x;;
                                              ^
Error: This expression has type "uf1 box_" = "float"
       but an expression was expected of type "uf2 box_" = "int64"
|}]

(* Test 20: Type inference *)

let infer1 x = (x : float# box_);;
[%%expect{|
val infer1 : float# box_ -> float# box_ = <fun>
|}]

let infer2 () =
  let f (x : float# box_) = x in
  f 1.0;;
[%%expect{|
val infer2 : unit -> float# box_ = <fun>
|}]

type ufl = float#
let infer3 (x : ufl box_) = x +. 1.0;;
[%%expect{|
type ufl = float#
val infer3 : ufl box_ -> float = <fun>
|}]

(* Test 21: Distinct vs same underlying box_ types *)

type t_float_box = float# box_
type t_int32_box = int32# box_;;
[%%expect{|
type t_float_box = float# box_
type t_int32_box = int32# box_
|}]

let distinct1 (x : t_float_box) (y : t_int32_box) = x = y;;
[%%expect{|
Line 1, characters 56-57:
1 | let distinct1 (x : t_float_box) (y : t_int32_box) = x = y;;
                                                            ^
Error: This expression has type "t_int32_box" = "int32"
       but an expression was expected of type "t_float_box" = "float"
|}]

type ua = float#
type ub = float#
type ta = ua box_
type tb = ub box_;;
[%%expect{|
type ua = float#
type ub = float#
type ta = ua box_
type tb = ub box_
|}]

let same_underlying (x : ta) (y : tb) = x = y;;
[%%expect{|
val same_underlying : ta -> tb -> bool = <fun>
|}]

(* Test 22: With constraints in module types *)

module type S_box = sig
  type t
  val x : t
end;;
[%%expect{|
module type S_box = sig type t val x : t end
|}]

module type S_float_box = S_box with type t = float# box_;;
[%%expect{|
module type S_float_box = sig type t = float# box_ val x : t end
|}]

module M_float_box : S_float_box = struct
  type t = float# box_
  let x = 1.0
end;;
[%%expect{|
module M_float_box : S_float_box
|}]

let use_m_float_box : float = M_float_box.x;;
[%%expect{|
val use_m_float_box : float = 1.
|}]

(* Test 23: Functors with box_

   Note: A functor cannot generically convert U.t to U.t box_ because the
   type system doesn't know U.t is an unboxed version of something.
   However, we can write functors that use box_ in their signatures when
   we know the concrete types. *)

module type UNBOXED = sig
  type t : float64
  val zero : t
end;;
[%%expect{|
module type UNBOXED = sig type t : float64 val zero : t end
|}]

(* This demonstrates the limitation: can't coerce U.t to U.t box_ generically *)
module type BOXED = sig
  type unboxed : float64
  type t = unboxed box_
  val zero : t
end;;
[%%expect{|
module type BOXED =
  sig type unboxed : float64 type t = unboxed box_ val zero : t end
|}]

module MakeBoxed_fails (U : UNBOXED) : BOXED with type unboxed = U.t = struct
  type unboxed = U.t
  type t = unboxed box_
  let zero = U.zero
end;;
[%%expect{|
Lines 1-5, characters 71-3:
1 | .......................................................................struct
2 |   type unboxed = U.t
3 |   type t = unboxed box_
4 |   let zero = U.zero
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type unboxed = U.t type t = unboxed box_ val zero : U.t end
       is not included in
         sig type unboxed = U.t type t = unboxed box_ val zero : t end
       Values do not match: val zero : U.t is not included in val zero : t
       The type "U.t" is not compatible with the type "t" = "U.t box_"
|}]

(* But we can use box_ with concrete types in module signatures *)
module Float_boxed_module : sig
  type t = float# box_
  val zero : t
end = struct
  type t = float# box_
  let zero = 0.0
end;;
[%%expect{|
module Float_boxed_module : sig type t = float# box_ val zero : t end
|}]

let use_module_result : float = Float_boxed_module.zero;;
[%%expect{|
val use_module_result : float = 0.
|}]

(* Test 24: First-class modules *)

module type T_FCM = sig
  type t = float# box_
  val value : t
end;;
[%%expect{|
module type T_FCM = sig type t = float# box_ val value : t end
|}]

let fcm : (module T_FCM) =
  (module struct
    type t = float# box_
    let value = 3.14
  end);;
[%%expect{|
val fcm : (module T_FCM) = <module>
|}]

let extract_fcm () =
  let module M = (val fcm) in
  M.value;;
[%%expect{|
val extract_fcm : unit -> float# box_ = <fun>
|}]

let fcm_as_float : float = extract_fcm ();;
[%%expect{|
val fcm_as_float : float = 3.14
|}]

(* Test 25: box_ on value types (int, string)

   Value types like int and string are NOT "unboxed versions" of anything,
   so int box_ does NOT equal int. The box_ operator only expands when applied
   to actual unboxed versions (like float#, int32#, or record#). *)

type boxed_int = int box_;;
[%%expect{|
type boxed_int = int box_
|}]

(* int box_ is NOT equal to int *)
let int_box_is_int (x : int box_) : int = x;;
[%%expect{|
Line 1, characters 42-43:
1 | let int_box_is_int (x : int box_) : int = x;;
                                              ^
Error: This expression has type "int box_"
       but an expression was expected of type "int"
|}]

let int_is_int_box (x : int) : int box_ = x;;
[%%expect{|
Line 1, characters 42-43:
1 | let int_is_int_box (x : int) : int box_ = x;;
                                              ^
Error: This expression has type "int" but an expression was expected of type
         "int box_"
|}]

type boxed_string = string box_;;
[%%expect{|
type boxed_string = string box_
|}]

(* string box_ is NOT equal to string *)
let string_box_roundtrip (x : string) : string box_ = x;;
[%%expect{|
Line 1, characters 54-55:
1 | let string_box_roundtrip (x : string) : string box_ = x;;
                                                          ^
Error: This expression has type "string" but an expression was expected of type
         "string box_"
|}]

(* But box_ types still unify with themselves *)
let int_box_eq (x : int box_) (y : int box_) = x = y;;
[%%expect{|
val int_box_eq : int box_ -> int box_ -> bool = <fun>
|}]

(* Test 26: Recursive types with box_ *)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

type float_tree = float# box_ tree;;
[%%expect{|
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
type float_tree = float# box_ tree
|}]

let make_float_tree () : float_tree =
  Node (1.0, Node (2.0, Leaf, Leaf), Leaf);;
[%%expect{|
val make_float_tree : unit -> float_tree = <fun>
|}]

let sum_tree t =
  let rec go acc = function
    | Leaf -> acc
    | Node (x, l, r) -> go (go (acc +. x) l) r
  in go 0.0 t;;
[%%expect{|
val sum_tree : float tree -> float = <fun>
|}]

let sum_float_tree (t : float_tree) = sum_tree t;;
[%%expect{|
val sum_float_tree : float_tree -> float = <fun>
|}]

(* Test 27: Polymorphic box_ unification with concrete boxed types *)

let f_box : 'a box_ -> 'a box_ = fun x -> x;;
[%%expect{|
val f_box : ('a : any). 'a box_ -> 'a box_ = <fun>
|}]

let _ = f_box 5.;;
[%%expect{|
- : float# box_ = 5.
|}]

let _ = f_box 42l;;
[%%expect{|
- : int32# box_ = 42l
|}]

let _ = f_box 42L;;
[%%expect{|
- : int64# box_ = 42L
|}]

(* int also has an unboxed version (int#) *)
let _ = f_box 42;;
[%%expect{|
- : int# box_ = 42
|}]

(* Test that this does NOT work for types without unboxed versions *)
let _ = f_box "hello";;
[%%expect{|
Line 1, characters 14-21:
1 | let _ = f_box "hello";;
                  ^^^^^^^
Error: This expression has type "string" but an expression was expected of type
         "'a box_"
|}]

(* Test 28: Subtyping with polymorphic variants and box_ *)

type ab = [ `A | `B ]
type a  = [ `A ];;
[%%expect{|
type ab = [ `A | `B ]
type a = [ `A ]
|}]

let coerce_box (x : a box_) : ab box_ = (x :> ab box_);;
[%%expect{|
val coerce_box : a box_ -> ab box_ = <fun>
|}]

(* Also test the other direction fails *)
let coerce_box_fail (x : ab box_) : a box_ = (x :> a box_);;
[%%expect{|
Line 1, characters 45-58:
1 | let coerce_box_fail (x : ab box_) : a box_ = (x :> a box_);;
                                                 ^^^^^^^^^^^^^
Error: Type "ab box_" = "[ `A | `B ] box_" is not a subtype of
         "a box_" = "[ `A ] box_"
       Type "ab" = "[ `A | `B ]" is not a subtype of "a" = "[ `A ]"
       The second variant type does not allow tag(s) "`B"
|}]

(* Test 29: Recursive type declarations with box_
   These test well-foundedness and infinite size checks *)

type a_rec = { a_rec : a_rec box_ } [@@unboxed];;
[%%expect{|
type a_rec = { a_rec : a_rec box_; } [@@unboxed]
|}]

type t1_rec = { t2_rec : t2_rec box_ } [@@unboxed]
and t2_rec = t1_rec box_;;
[%%expect{|
type t1_rec = { t2_rec : t2_rec box_; } [@@unboxed]
and t2_rec = t1_rec box_
|}]

(* Recursive types with box_ that are NOT unboxed also work *)
type b_rec = { b_rec_field : b_rec box_ };;
[%%expect{|
type b_rec = { b_rec_field : b_rec box_; }
|}]

(* Mutually recursive with box_ *)
type c1 = { c2_field : c2 box_ }
and c2 = { c1_field : c1 box_ };;
[%%expect{|
type c1 = { c2_field : c2 box_; }
and c2 = { c1_field : c1 box_; }
|}]
