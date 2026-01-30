(* TEST
   include stdlib_upstream_compatible;
   expect;
*)

(* Test 1: Basic box_kind syntax for abstract types *)

type t : bits32 box_kind;;
[%%expect{|
type t : bits32 box_kind mod non_float
|}]

type t2 : bits64 box_kind;;
[%%expect{|
type t2 : bits64 box_kind mod non_float
|}]

type t3 : float64 box_kind;;
[%%expect{|
type t3 : float64 box_kind mod non_float
|}]

(* Test 2: box_kind types are subtypes of value - can be used as function args *)

type t4 : bits32 box_kind
let f (x : t4) = x;;
[%%expect{|
type t4 : bits32 box_kind mod non_float
val f : t4 -> t4 = <fun>
|}]

(* Test 3: box_kind of product kinds *)

type t5 : (bits32 & bits64) box_kind;;
[%%expect{|
type t5 : (bits32 & bits64) box_kind mod non_float
|}]

(* Test 3b: product box_kind types are subtypes of value *)
type t5b : (bits32 & bits64) box_kind
let store_product (x : t5b) = [x];;
[%%expect{|
type t5b : (bits32 & bits64) box_kind mod non_float
val store_product : t5b -> t5b list = <fun>
|}]

(* Test 3c: different product box_kinds don't unify *)
type t5c : (bits32 & bits64) box_kind
type t5d : (bits64 & bits32) box_kind
let f (x : t5c) : t5d = x;;
[%%expect{|
type t5c : (bits32 & bits64) box_kind mod non_float
type t5d : (bits64 & bits32) box_kind mod non_float
Line 3, characters 24-25:
3 | let f (x : t5c) : t5d = x;;
                            ^
Error: This expression has type "t5c" but an expression was expected of type
         "t5d"
|}]

(* Test 4: Nested box_kind is allowed syntactically *)

type t6 : bits32 box_kind box_kind;;
[%%expect{|
type t6 : bits32 box_kind box_kind mod non_float
|}]

(* Test 5: box_kind with mode modifiers *)

type t7 : bits32 box_kind mod global;;
[%%expect{|
type t7 : bits32 box_kind mod global non_float
|}]

(* Test 6: box_kind is a subkind of value - can be stored in a list *)

type t8 : bits32 box_kind
let store (x : t8) = [x];;
[%%expect{|
type t8 : bits32 box_kind mod non_float
val store : t8 -> t8 list = <fun>
|}]

(* Test 7: box_kind can satisfy value constraint *)

type t9 : bits32 box_kind
type ('a : value) box_wrapper = { contents : 'a }
let wrap (x : t9) : t9 box_wrapper = { contents = x };;
[%%expect{|
type t9 : bits32 box_kind mod non_float
type 'a box_wrapper = { contents : 'a; }
val wrap : t9 -> t9 box_wrapper = <fun>
|}]

(* Test 8: Different box_kind types don't unify *)

type t10 : bits32 box_kind
type t11 : bits64 box_kind
let f (x : t10) : t11 = x;;
[%%expect{|
type t10 : bits32 box_kind mod non_float
type t11 : bits64 box_kind mod non_float
Line 3, characters 24-25:
3 | let f (x : t10) : t11 = x;;
                            ^
Error: This expression has type "t10" but an expression was expected of type
         "t11"
|}]

(*******************************************)
(* Test 9: Additional base sorts with box_kind *)

(* Float sorts *)
type t_float32_box : float32 box_kind;;
[%%expect{|
type t_float32_box : float32 box_kind mod non_float
|}]

(* Word (native-sized integers) *)
type t_word_box : word box_kind;;
[%%expect{|
type t_word_box : word box_kind mod non_float
|}]

(* Small integer sorts *)
type t_bits8_box : bits8 box_kind;;
[%%expect{|
type t_bits8_box : bits8 box_kind mod non_float
|}]

type t_bits16_box : bits16 box_kind;;
[%%expect{|
type t_bits16_box : bits16 box_kind mod non_float
|}]

(* Vector sorts *)
type t_vec128_box : vec128 box_kind;;
[%%expect{|
type t_vec128_box : vec128 box_kind mod non_float
|}]

type t_vec256_box : vec256 box_kind;;
[%%expect{|
type t_vec256_box : vec256 box_kind mod non_float
|}]

type t_vec512_box : vec512 box_kind;;
[%%expect{|
type t_vec512_box : vec512 box_kind mod non_float
|}]

(* void box_kind - should work syntactically *)
type t_void_box : void box_kind;;
[%%expect{|
type t_void_box : void box_kind mod non_float
|}]

(* value box_kind - should be allowed syntactically (boxed boxable value) *)
type t_value_box : value box_kind;;
[%%expect{|
type t_value_box : value box_kind
|}]

(*******************************************)
(* Test 10: Sort variable unification with box_kind *)

(* box_kind type used where value is expected - should work *)
type t_boxed : bits32 box_kind
type 'a wrapper = { contents : 'a }

let use_boxed (x : t_boxed) : t_boxed wrapper = { contents = x };;
[%%expect{|
type t_boxed : bits32 box_kind mod non_float
type 'a wrapper = { contents : 'a; }
val use_boxed : t_boxed -> t_boxed wrapper = <fun>
|}]

(* Polymorphic function with type variable that gets constrained by box_kind *)
type t_box64 : bits64 box_kind

let poly_id (x : 'a) : 'a = x
let use_poly (x : t_box64) = poly_id x;;
[%%expect{|
type t_box64 : bits64 box_kind mod non_float
val poly_id : 'a -> 'a = <fun>
val use_poly : t_box64 -> t_box64 = <fun>
|}]

(* Verify box_kind types can satisfy the value layout constraint *)
type ('a : value) value_constrained = V of 'a
type t_for_v : bits32 box_kind

let mk_v (x : t_for_v) : t_for_v value_constrained = V x;;
[%%expect{|
type 'a value_constrained = V of 'a
type t_for_v : bits32 box_kind mod non_float
val mk_v : t_for_v -> t_for_v value_constrained = <fun>
|}]

(*******************************************)
(* Test 11: Mutually recursive types with box_kind *)

(* Simple mutual recursion through list *)
type t_rec : bits32 box_kind
and t_rec_list = t_rec list;;
[%%expect{|
type t_rec : bits32 box_kind mod non_float
and t_rec_list = t_rec list
|}]

(* Mutual recursion where both types have box_kind *)
type t_rec_a : bits32 box_kind
and t_rec_b : bits64 box_kind
and t_rec_wrapper = { a : t_rec_a; b : t_rec_b };;
[%%expect{|
type t_rec_a : bits32 box_kind mod non_float
and t_rec_b : bits64 box_kind mod non_float
and t_rec_wrapper = { a : t_rec_a; b : t_rec_b; }
|}]

(* Recursive type referencing itself through boxing *)
type t_self_rec : bits32 box_kind
and t_self_list = Nil | Cons of t_self_rec * t_self_list;;
[%%expect{|
type t_self_rec : bits32 box_kind mod non_float
and t_self_list = Nil | Cons of t_self_rec * t_self_list
|}]

(*******************************************)
(* Test 12: Negative tests - error messages *)

(* box_kind cannot satisfy non-value layout requirement *)
type t_box : bits32 box_kind
type ('a : float64) needs_float64 = 'a

type bad = t_box needs_float64;;
[%%expect{|
type t_box : bits32 box_kind mod non_float
type ('a : float64) needs_float64 = 'a
Line 4, characters 11-16:
4 | type bad = t_box needs_float64;;
               ^^^^^
Error: This type "t_box" should be an instance of type "('a : float64)"
       The layout of t_box is bits32 box_kind
         because of the definition of t_box at line 1, characters 0-28.
       But the layout of t_box must be a sublayout of float64
         because of the definition of needs_float64 at line 2, characters 0-38.
|}]

(* Cannot assign box_kind type to unboxed sort constraint *)
type t_box_attempt : bits32 box_kind
type ('a : bits32) needs_bits32 = 'a

type bad2 = t_box_attempt needs_bits32;;
[%%expect{|
type t_box_attempt : bits32 box_kind mod non_float
type ('a : bits32) needs_bits32 = 'a
Line 4, characters 12-25:
4 | type bad2 = t_box_attempt needs_bits32;;
                ^^^^^^^^^^^^^
Error: This type "t_box_attempt" should be an instance of type "('a : bits32)"
       The layout of t_box_attempt is bits32 box_kind
         because of the definition of t_box_attempt at line 1, characters 0-36.
       But the layout of t_box_attempt must be a sublayout of bits32
         because of the definition of needs_bits32 at line 2, characters 0-36.
|}]

(* any box_kind should fail - any is a layout, not a sort *)
type t_bad : any box_kind;;
[%%expect{|
Line 1, characters 13-16:
1 | type t_bad : any box_kind;;
                 ^^^
Error: The layout any is not representable.
       box_kind must contain a representable layout, not any.
|}]

(* any nested in a product also fails *)
type t_bad2 : (any & bits32) box_kind;;
[%%expect{|
Line 1, characters 14-28:
1 | type t_bad2 : (any & bits32) box_kind;;
                  ^^^^^^^^^^^^^^
Error: The layout any & bits32 is not representable.
       box_kind must contain a representable layout, not any.
|}]

(*******************************************)
(* Test 13: box_kind in module signatures *)

module type S_BOX = sig
  type t : bits32 box_kind
  val create : unit -> t
  val to_list : t -> t list
end;;
[%%expect{|
module type S_BOX =
  sig
    type t : bits32 box_kind mod non_float
    val create : unit -> t
    val to_list : t -> t list
  end
|}]

module M_Box : S_BOX = struct
  type t : bits32 box_kind
  let create () = (assert false : t)
  let to_list x = [x]
end;;
[%%expect{|
module M_Box : S_BOX
|}]

(* Module signature with product box_kind *)
module type S_BOX_PROD = sig
  type t : (bits32 & bits64) box_kind
  val pair : t -> t -> t * t
end;;
[%%expect{|
module type S_BOX_PROD =
  sig
    type t : (bits32 & bits64) box_kind mod non_float
    val pair : t -> t -> t * t
  end
|}]

(*******************************************)
(* Test 14: box_kind in functors *)

module type INPUT = sig
  type t : bits32 box_kind
end

module Make (X : INPUT) = struct
  type t = X.t list
  let wrap (x : X.t) = [x]
end;;
[%%expect{|
module type INPUT = sig type t : bits32 box_kind mod non_float end
module Make :
  functor (X : INPUT) -> sig type t = X.t list val wrap : X.t -> X.t list end
|}]

(* Functor with box_kind type parameter constraint *)
module type BOXABLE = sig
  type ('a : bits64 box_kind) t
end

module MakeBoxable (X : BOXABLE) = struct
  type u : bits64 box_kind
  type wrapped = u X.t
end;;
[%%expect{|
module type BOXABLE = sig type ('a : bits64 box_kind mod non_float) t end
module MakeBoxable :
  functor (X : BOXABLE) ->
    sig type u : bits64 box_kind mod non_float type wrapped = u X.t end
|}]

(*******************************************)
(* Test 15: Local abstract types with box_kind *)

let locally_abstract (type a : bits32 box_kind) (x : a) : a = x;;
[%%expect{|
val locally_abstract : ('a : bits32 box_kind mod non_float). 'a -> 'a = <fun>
|}]

(* Using local abstract type in a more complex scenario *)
let with_local_box (type a : bits64 box_kind) (x : a) : a list * int =
  ([x], 42);;
[%%expect{|
val with_local_box :
  ('a : bits64 box_kind mod non_float). 'a -> 'a list * int = <fun>
|}]

(* Local abstract type with box_kind in module *)
let f () =
  let module M = struct
    type t : bits32 box_kind
    let wrap (x : t) = [x]
  end in
  ();;
[%%expect{|
val f : unit -> unit = <fun>
|}]

(*******************************************)
(* Test 16: Polymorphic functions with box_kind constraints *)

(* Explicitly polymorphic over box_kind types *)
let box_id : ('a : bits32 box_kind). 'a -> 'a = fun x -> x;;
[%%expect{|
val box_id : ('a : bits32 box_kind mod non_float). 'a -> 'a = <fun>
|}]

(* Using the polymorphic function *)
type my_box : bits32 box_kind
let use_box_id (x : my_box) = box_id x;;
[%%expect{|
type my_box : bits32 box_kind mod non_float
val use_box_id : my_box -> my_box = <fun>
|}]

(* Polymorphic over product box_kind *)
let prod_box_id : ('a : (bits32 & bits64) box_kind). 'a -> 'a = fun x -> x;;
[%%expect{|
val prod_box_id : ('a : (bits32 & bits64) box_kind mod non_float). 'a -> 'a =
  <fun>
|}]

(*******************************************)
(* Test 17: Intersection/meet behavior *)

(* box_kind intersect with value should give box_kind *)
type t_meet : bits32 box_kind
type 'a val_param = 'a

(* This should work - box_kind meet value = box_kind *)
type t_meet_result = t_meet val_param;;
[%%expect{|
type t_meet : bits32 box_kind mod non_float
type 'a val_param = 'a
type t_meet_result = t_meet val_param
|}]

(* Different box_kinds should not meet successfully *)
type ('a : bits32 box_kind) box32_param = 'a
type t_other : bits64 box_kind

type t_bad_meet = t_other box32_param;;
[%%expect{|
type ('a : bits32 box_kind mod non_float) box32_param = 'a
type t_other : bits64 box_kind mod non_float
Line 4, characters 18-25:
4 | type t_bad_meet = t_other box32_param;;
                      ^^^^^^^
Error: This type "t_other" should be an instance of type
         "('a : bits32 box_kind mod non_float)"
       The layout of t_other is bits64 box_kind
         because of the definition of t_other at line 2, characters 0-30.
       But the layout of t_other must be a sublayout of bits32 box_kind
         because of the definition of box32_param at line 1, characters 0-44.
|}]

(*******************************************)
(* Test 18: Complex product box_kinds *)

(* Triple product *)
type t_triple : (bits32 & bits64 & float64) box_kind;;
[%%expect{|
type t_triple : (bits32 & bits64 & float64) box_kind mod non_float
|}]

(* Products with same sort *)
type t_same : (bits32 & bits32) box_kind;;
[%%expect{|
type t_same : (bits32 & bits32) box_kind mod non_float
|}]

(* Deep nesting of box_kind *)
type t_deep : bits32 box_kind box_kind box_kind;;
[%%expect{|
type t_deep : bits32 box_kind box_kind box_kind mod non_float
|}]

(* Product of different box_kinds stored in a record *)
type t_prod_a : bits32 box_kind
type t_prod_b : bits64 box_kind
type prod_record = { pa : t_prod_a; pb : t_prod_b };;
[%%expect{|
type t_prod_a : bits32 box_kind mod non_float
type t_prod_b : bits64 box_kind mod non_float
type prod_record = { pa : t_prod_a; pb : t_prod_b; }
|}]

(*******************************************)
(* Test 19: box_kind with separability *)

(* All box_kind types should have mod non_float - verify with float64 box_kind *)
type t_float_box : float64 box_kind;;
[%%expect{|
type t_float_box : float64 box_kind mod non_float
|}]

(* Verify box_kind types can be used in contexts requiring separability *)
type t_boxed_sep : bits32 box_kind
type exists = E : 'a * ('a -> int) -> exists
let wrap_box (x : t_boxed_sep) (f : t_boxed_sep -> int) = E (x, f);;
[%%expect{|
type t_boxed_sep : bits32 box_kind mod non_float
type exists = E : 'a * ('a -> int) -> exists
val wrap_box : t_boxed_sep -> (t_boxed_sep -> int) -> exists = <fun>
|}]

(*******************************************)
(* Test 20: box_kind types in arrays and refs *)

type t_arr : bits32 box_kind
let make_arr (x : t_arr) = [| x |];;
[%%expect{|
type t_arr : bits32 box_kind mod non_float
val make_arr : t_arr -> t_arr array = <fun>
|}]

type t_ref_box : bits64 box_kind
let make_ref (x : t_ref_box) = ref x;;
[%%expect{|
type t_ref_box : bits64 box_kind mod non_float
val make_ref : t_ref_box -> t_ref_box ref = <fun>
|}]
