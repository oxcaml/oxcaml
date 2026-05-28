(* TEST
   include stdlib_upstream_compatible;
   expect;
*)

(* Test 1: box on unboxed types - the manifest is stored as Tbox but
   expands to the boxed type during unification *)

type t1 = float# box
type t2 = int32# box
type t3 = int64# box
type t4 = nativeint# box
type t5 = int# box
type t6 = #(int64# * string) box
type 'a t7 = 'a ref# box
[%%expect{|
type t1 = float
type t2 = int32
type t3 = int64
type t4 = nativeint
type t5 = int
type t6 = int64# * string
type 'a t7 = 'a ref
|}]

let g (x : float# box) : float# box = x;;
[%%expect{|
val g : float -> float = <fun>
|}]

(* Test 2: box through type aliases *)

type u = float#
type t = u box;;
let f (x : t) : float = x;;
[%%expect{|
type u = float#
type t = float
val f : t -> float = <fun>
|}]

type ('a : any) b = 'a box
type t1' = float# b
[%%expect{|
type ('a : any) b = 'a box
type t1' = float
|}]

let g' (x : float# b) : float# b = x;;
[%%expect{|
val g' : float -> float = <fun>
|}]

(* Test 3: [float# box] unifies with [float] *)

let h (x : float# box) : float = x;;
[%%expect{|
val h : float -> float = <fun>
|}]

let i (x : float) : float# box = x;;
[%%expect{|
val i : float -> float = <fun>
|}]

(* Test 4: records too *)

type r = { x : int }
let r_id (rub : r# box) : r = rub;;
[%%expect{|
type r = { x : int; }
val r_id : r -> r = <fun>
|}]

(* but records are still nominally typed *)
type u = #{ x : int }
let bad (ub : u box) : r = ub;;
[%%expect{|
type u = #{ x : int; }
Line 2, characters 27-29:
2 | let bad (ub : u box) : r = ub;;
                               ^^
Error: This expression has type "u box" but an expression was expected of type
         "r"
       Type "u" is not compatible with type "r#"
|}]

(* and float records don't get unboxed versions *)
type r = { x : float }
let r_id (rub : _ box) : r = rub;;
[%%expect{|
type r = { x : float; }
Line 2, characters 29-32:
2 | let r_id (rub : _ box) : r = rub;;
                                 ^^^
Error: This expression has type "'a box" but an expression was expected of type
         "r"
|}]

(* Test 4: box types unify with themselves in function types *)

let eq_box (x : int box) (y : int box) = x = y;;
[%%expect{|
val eq_box : int box -> int box -> bool = <fun>
|}]

let eq_float_box (x : float# box) (y : float# box) = x = y;;
[%%expect{|
val eq_float_box : float -> float -> bool = <fun>
|}]

(* Test 5: box in module signatures *)

module type S = sig
  type t = float# box
  val x : t
end;;
[%%expect{|
module type S = sig type t = float val x : t end
|}]

module M : S = struct
  type t = float# box
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


(* Test 10: Polymorphic box with explicit jkind annotation *)

let check_boxed_by : type (a : float64). a -> a box -> unit =
  fun _ _ -> ();;
[%%expect{|
val check_boxed_by : ('a : float64). 'a -> 'a box -> unit = <fun>
|}]

type uf = float#
let test_check (u : uf) (f : float) = check_boxed_by u f;;
[%%expect{|
type uf = float#
val test_check : uf -> float -> unit = <fun>
|}]

(* Test 11: Multiple levels of type aliasing - the inner type of box
   must be fully expanded to find the unboxed type *)

type f = float#
type g = f
let test_multi_alias (x : g box) : float = x;;
[%%expect{|
type f = float#
type g = f
val test_multi_alias : float -> float = <fun>
|}]

type h = g
let test_three_levels (x : h box) : float = x;;
[%%expect{|
type h = g
val test_three_levels : float -> float = <fun>
|}]

(* Test 12: box on types without unboxed versions *)

type abstract_type
type boxed_abstract = abstract_type box;;
[%%expect{|
type abstract_type
type boxed_abstract = abstract_type box
|}]

(* Test 13: float32# box = float32 *)

type t_f32 = float32# box;;
[%%expect{|
type t_f32 = float32
|}]

let f_f32 (x : t_f32) : float32 = x;;
[%%expect{|
val f_f32 : t_f32 -> float32 = <fun>
|}]

let g_f32 (x : float32) : float32# box = x;;
[%%expect{|
val g_f32 : float32 -> float32 = <fun>
|}]

(* Test 14: Implicit unboxed records - t# box = t
   Mixed records (not float-only) have implicit unboxed versions *)

type mixed_record = { i : int; s : string };;
[%%expect{|
type mixed_record = { i : int; s : string; }
|}]

let mixed_of_unboxed (p : mixed_record# box) : mixed_record = p;;
[%%expect{|
val mixed_of_unboxed : mixed_record -> mixed_record = <fun>
|}]

let unboxed_of_mixed (p : mixed_record) : mixed_record# box = p;;
[%%expect{|
val unboxed_of_mixed : mixed_record -> mixed_record = <fun>
|}]

type umixed = mixed_record#
type boxed_umixed = umixed box;;
[%%expect{|
type umixed = mixed_record#
type boxed_umixed = mixed_record
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

let float_point_no_unboxed (p : float_point# box) : float_point = p;;
[%%expect{|
val float_point_no_unboxed : float_point -> float_point = <fun>
|}]

(* Test 15: Boxing unboxed tuples *)

type ut = #(int * string);;
[%%expect{|
type ut = #(int * string)
|}]

type boxed_ut = ut box;;
[%%expect{|
type boxed_ut = int * string
|}]

let eq_ut (x : #(int * string) box) (y : ut box) = x = y;;
[%%expect{|
val eq_ut : int * string -> int * string -> bool = <fun>
|}]

(* Test 16: Additional jkinds - bits32, bits64, word *)

let check_bits32 : type (a : bits32). a -> a box -> unit =
  fun _ _ -> ();;
[%%expect{|
val check_bits32 : ('a : bits32). 'a -> 'a box -> unit = <fun>
|}]

type ui32 = int32#
let test_bits32 (u : ui32) (b : int32) = check_bits32 u b;;
[%%expect{|
type ui32 = int32#
val test_bits32 : ui32 -> int32 -> unit = <fun>
|}]

let check_bits64 : type (a : bits64). a -> a box -> unit =
  fun _ _ -> ();;
[%%expect{|
val check_bits64 : ('a : bits64). 'a -> 'a box -> unit = <fun>
|}]

type ui64 = int64#
let test_bits64 (u : ui64) (b : int64) = check_bits64 u b;;
[%%expect{|
type ui64 = int64#
val test_bits64 : ui64 -> int64 -> unit = <fun>
|}]

let check_word : type (a : word). a -> a box -> unit =
  fun _ _ -> ();;
[%%expect{|
val check_word : ('a : word). 'a -> 'a box -> unit = <fun>
|}]

type unat = nativeint#
let test_word (u : unat) (b : nativeint) = check_word u b;;
[%%expect{|
type unat = nativeint#
val test_word : unat -> nativeint -> unit = <fun>
|}]

(* Test 17: Parameterized box type alias *)

type ('a : float64) boxed = 'a box;;
[%%expect{|
type ('a : float64) boxed = 'a box
|}]

let use_boxed (x : float# boxed) : float = x;;
[%%expect{|
val use_boxed : float -> float = <fun>
|}]

type alias_float = float#
let use_boxed_alias (x : alias_float boxed) : float = x;;
[%%expect{|
type alias_float = float#
val use_boxed_alias : float -> float = <fun>
|}]

(* Test 18: Nested box - float# box expands to float, but float is not an
   unboxed version, so float box does NOT expand further to float.
   This tests that box only expands for actual unboxed versions. *)

type nested = float# box box;;
[%%expect{|
type nested = float box
|}]

(* nested = float# box box = float box, which does NOT equal float *)
let nested_to_float (x : nested) : float = x;;
[%%expect{|
Line 1, characters 43-44:
1 | let nested_to_float (x : nested) : float = x;;
                                               ^
Error: This expression has type "float box"
       but an expression was expected of type "float"
       Type "float" is not compatible with type "float#"
|}]

(* But nested types still unify with each other *)
let nested_eq (x : nested) (y : float# box box) = x = y;;
[%%expect{|
val nested_eq : float box -> float box -> bool = <fun>
|}]

(* Test 19: Type error cases *)

let mismatch1 (x : float# box) : int = x;;
[%%expect{|
Line 1, characters 39-40:
1 | let mismatch1 (x : float# box) : int = x;;
                                           ^
Error: This expression has type "float" but an expression was expected of type
         "int"
|}]

let mismatch2 (x : int32# box) : int64 = x;;
[%%expect{|
Line 1, characters 41-42:
1 | let mismatch2 (x : int32# box) : int64 = x;;
                                             ^
Error: This expression has type "int32" but an expression was expected of type
         "int64"
|}]

type uf1 = float#
type uf2 = int64#
let mismatch3 (x : uf1 box) : uf2 box = x;;
[%%expect{|
type uf1 = float#
type uf2 = int64#
Line 3, characters 40-41:
3 | let mismatch3 (x : uf1 box) : uf2 box = x;;
                                            ^
Error: This expression has type "float" but an expression was expected of type
         "int64"
|}]

(* Test 20: Type inference *)

let infer1 x = (x : float# box);;
[%%expect{|
val infer1 : float -> float = <fun>
|}]

let infer2 () =
  let f (x : float# box) = x in
  f 1.0;;
[%%expect{|
val infer2 : unit -> float = <fun>
|}]

type ufl = float#
let infer3 (x : ufl box) = x +. 1.0;;
[%%expect{|
type ufl = float#
val infer3 : float -> float = <fun>
|}]

(* Test 21: Distinct vs same underlying box types *)

type t_float_box = float# box
type t_int32_box = int32# box;;
[%%expect{|
type t_float_box = float
type t_int32_box = int32
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
type ta = ua box
type tb = ub box;;
[%%expect{|
type ua = float#
type ub = float#
type ta = float
type tb = float
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

module type S_float_box = S_box with type t = float# box;;
[%%expect{|
module type S_float_box = sig type t = float val x : t end
|}]

module M_float_box : S_float_box = struct
  type t = float# box
  let x = 1.0
end;;
[%%expect{|
module M_float_box : S_float_box
|}]

let use_m_float_box : float = M_float_box.x;;
[%%expect{|
val use_m_float_box : float = 1.
|}]

(* Test 23: Functors with box *)

module type UNBOXED = sig
  type t : float64
  val zero : t
end;;
[%%expect{|
module type UNBOXED = sig type t : float64 val zero : t end
|}]

(* This demonstrates the limitation: can't coerce U.t to U.t box generically *)
module type BOXED = sig
  type unboxed : float64
  type t = unboxed box
  (* CR box: once we have the box operator, have [zero] be boxed *)
  val zero : unboxed
end;;
[%%expect{|
module type BOXED =
  sig type unboxed : float64 type t = unboxed box val zero : unboxed end
|}]

module MakeBoxed (U : UNBOXED) : BOXED with type unboxed = U.t = struct
  type unboxed = U.t
  type t = unboxed box
  let zero = U.zero
end;;
[%%expect{|
module MakeBoxed :
  functor (U : UNBOXED) ->
    sig type unboxed = U.t type t = unboxed box val zero : unboxed end
|}]


(* Test 24: First-class modules *)

module type T_FCM = sig
  type t = float# box
  val value : t
end;;
[%%expect{|
module type T_FCM = sig type t = float val value : t end
|}]

let fcm : (module T_FCM) =
  (module struct
    type t = float# box
    let value = 3.14
  end);;
[%%expect{|
val fcm : (module T_FCM) = <module>
|}]

let extract_fcm () =
  let module M = (val fcm) in
  M.value;;
[%%expect{|
val extract_fcm : unit -> float = <fun>
|}]

let fcm_as_float : float = extract_fcm ();;
[%%expect{|
val fcm_as_float : float = 3.14
|}]

(* Test 25: box on value types (int, string)

   Values like int and string are NOT "unboxed versions" of anything,
   but they can still be extra-boxed. *)

type boxed_int = int box;;
[%%expect{|
type boxed_int = int box
|}]

(* int box is not equal to int *)
let int_box_is_int (x : int box) : int = x;;
[%%expect{|
Line 1, characters 41-42:
1 | let int_box_is_int (x : int box) : int = x;;
                                             ^
Error: This expression has type "int box"
       but an expression was expected of type "int"
       Type "int" is not compatible with type "int#"
|}]

let int_is_int_box (x : int) : int box = x;;
[%%expect{|
Line 1, characters 41-42:
1 | let int_is_int_box (x : int) : int box = x;;
                                             ^
Error: This expression has type "int" but an expression was expected of type
         "int box"
       Type "int#" is not compatible with type "int"
|}]

type boxed_string = string box;;
[%%expect{|
type boxed_string = string box
|}]

(* string box is NOT equal to string *)
let string_box_roundtrip (x : string) : string box = x;;
[%%expect{|
Line 1, characters 53-54:
1 | let string_box_roundtrip (x : string) : string box = x;;
                                                         ^
Error: This expression has type "string" but an expression was expected of type
         "string box"
|}]

(* But box types still unify with themselves *)
let int_box_eq (x : int box) (y : int box) = x = y;;
[%%expect{|
val int_box_eq : int box -> int box -> bool = <fun>
|}]

(* Test 26: Recursive types with box *)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

type float_tree = float# box tree;;
[%%expect{|
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
type float_tree = float tree
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

(* Test 27: Polymorphic box unification with concrete boxed types *)

let f_box : 'a box -> 'a box = fun x -> x;;
[%%expect{|
val f_box : ('a : any). 'a box -> 'a box = <fun>
|}]

let _ = f_box 5.;;
[%%expect{|
- : float = 5.
|}]

let _ = f_box 42l;;
[%%expect{|
- : int32 = 42l
|}]

let _ = f_box 42L;;
[%%expect{|
- : int64 = 42L
|}]

(* int also has an unboxed version (int#) *)
let _ = f_box 42;;
[%%expect{|
- : int = 42
|}]

(* Test that this does NOT work for types without unboxed versions *)
let _ = f_box "hello";;
[%%expect{|
Line 1, characters 14-21:
1 | let _ = f_box "hello";;
                  ^^^^^^^
Error: This expression has type "string" but an expression was expected of type
         "'a box"
|}]

(* Test 28: Subtyping with polymorphic variants and box *)

type ab = [ `A | `B ]
type a  = [ `A ];;
[%%expect{|
type ab = [ `A | `B ]
type a = [ `A ]
|}]

let coerce_box (x : a box) : ab box = (x :> ab box);;
[%%expect{|
val coerce_box : [ `A ] box -> [ `A | `B ] box = <fun>
|}]

(* Also test the other direction fails *)
let coerce_box_fail (x : ab box) : a box = (x :> a box);;
[%%expect{|
Line 1, characters 43-55:
1 | let coerce_box_fail (x : ab box) : a box = (x :> a box);;
                                               ^^^^^^^^^^^^
Error: Type "[ `A | `B ] box" is not a subtype of "[ `A ] box"
       Type "[ `A | `B ]" is not a subtype of "[ `A ]"
       The second variant type does not allow tag(s) "`B"
|}]

(* [int32# box] unifies with [int32] across a subtype coercion *)
let coerce_unbox (x : [ `A of int32# box ]) =
  (x :> [ `A of int32 | `B ]);;
[%%expect{|
val coerce_unbox : [ `A of int32 ] -> [ `A of int32 | `B ] = <fun>
|}]

(* Test 29: Recursive type declarations with box
   These test well-foundedness and infinite size checks *)

type a_rec = { a_rec : a_rec box } [@@unboxed];;
[%%expect{|
type a_rec = { a_rec : a_rec box; } [@@unboxed]
|}]

type t1_rec = { t2_rec : t2_rec box } [@@unboxed]
and t2_rec = t1_rec box;;
[%%expect{|
type t1_rec = { t2_rec : t2_rec box; } [@@unboxed]
and t2_rec = t1_rec box
|}]

(* Recursive types with box that are NOT unboxed also work *)
type b_rec = { b_rec_field : b_rec box };;
[%%expect{|
type b_rec = { b_rec_field : b_rec box; }
|}]

(* Mutually recursive with box *)
type c1 = { c2_field : c2 box }
and c2 = { c1_field : c1 box };;
[%%expect{|
type c1 = { c2_field : c2 box; }
and c2 = { c1_field : c1 box; }
|}]


(* Test 30: [any] in [box] is representable *)

type t : any
type foo = t box
let f (foo : foo) = foo

[%%expect{|
type t : any
type foo = t box
val f : t box -> t box = <fun>
|}]


(* Test 31: GADTs *)

let require_boxed (_ : _ box) = ()
type 'a st = T : string st
type 'a ft = T : float ft
type 'a bt = T : _ box bt
[%%expect{|
val require_boxed : ('a : any). 'a box -> unit = <fun>
type 'a st = T : string st
type 'a ft = T : float ft
type 'a bt = T : 'b box bt
|}]

(* We can match on a GADT to learn that something is boxed *)
let ok_bt (type a) (a : a) (x : a bt) =
  match x with
  | T -> require_boxed a
[%%expect{|
val ok_bt : 'a -> 'a bt -> unit = <fun>
|}]

let ok_ft (type a) (a : a) (x : a ft) =
  match x with
  | T -> require_boxed a
[%%expect{|
val ok_ft : 'a -> 'a ft -> unit = <fun>
|}]

let bad_st (type a) (a : a) (x : a st) =
  match x with
  | T -> require_boxed a
[%%expect{|
Line 3, characters 23-24:
3 |   | T -> require_boxed a
                           ^
Error: This expression has type "a" = "string"
       but an expression was expected of type "'a box"
|}]

(* We can't discover something is boxed, then lower the GADT type (this would be
   unsound) *)

let bad (x : 'a st) (a : 'a) =
  require_boxed a;
  match x with
  | T ->
    ()
[%%expect{|
Line 4, characters 4-5:
4 |   | T ->
        ^
Error: This pattern matches values of type "string st"
       but a pattern was expected which matches values of type "$0 box st"
       The type constructor "$0" would escape its scope
|}]

(* Test 32: [(T box)#] = [T] -- the unboxed version of a [_ box] alias is
   the inner type. *)

type t = int box
type u = t#
let u_is_int (x : u) : int = x
let int_is_u (x : int) : u = x
[%%expect{|
type t = int box
type u = t#
val u_is_int : u -> int = <fun>
val int_is_u : int -> u = <fun>
|}]

(* The same property holds parametrically. *)

type ('a : any) my_box = 'a box
type 'a uu = 'a my_box#
let probe (x : 'a uu) : 'a = x
let probe_inv (x : 'a) : 'a uu = x
[%%expect{|
type ('a : any) my_box = 'a box
type 'a uu = 'a my_box#
val probe : 'a uu -> 'a = <fun>
val probe_inv : 'a -> 'a uu = <fun>
|}]

(* Test: [box] on object types *)

type obj = < m : int >
type t_obj_box = obj box;;
[%%expect{|
type obj = < m : int >
type t_obj_box = < m : int > box
|}]

let obj_box_eq (x : obj box) (y : obj box) = x = y;;
[%%expect{|
val obj_box_eq : < m : int > box -> < m : int > box -> bool = <fun>
|}]

(* Object subtyping is preserved through [box] *)
type obj_more = < m : int; n : int >
let widen (x : obj_more box) = (x :> obj box);;
[%%expect{|
type obj_more = < m : int; n : int >
val widen : < m : int; n : int > box -> < m : int > box = <fun>
|}]

(* Test 33: shadowing the predef [box] disambiguates with [box/2] *)

let id_box (x : 'a box) : 'a box = x
type 'a box = Mine of 'a
let still_id = id_box
[%%expect{|
val id_box : ('a : any). 'a box -> 'a box = <fun>
type 'a box = Mine of 'a
val still_id : ('a : any). 'a box/2 -> 'a box/2 = <fun>
|}]
