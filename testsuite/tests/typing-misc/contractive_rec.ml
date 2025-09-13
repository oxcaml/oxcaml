(* TEST
 expect;
 flags = "-extension rec_type_parameters";
*)

(* Tests for [@rec] annotated type parameters *)

(* Fresh variant types can be annotated as contractive *)
type ('a, 'b [@rec]) basic_list =
  | Nil
  | Cons of 'a * 'b;;

type int_list = (int, int_list) basic_list;;
[%%expect{|
type ('a, 'b [@rec]) basic_list = Nil | Cons of 'a * 'b
type int_list = (int, int_list) basic_list
|}]

(* Fresh record types can be annotated as contractive *)
type ('a [@rec]) contractive_record = {
  field : 'a;
  other : int;
};;

type recursive_record = recursive_record contractive_record;;
[%%expect{|
type ('a [@rec]) contractive_record = { field : 'a; other : int; }
type recursive_record = recursive_record contractive_record
|}]

(* Multiple parameters with mixed annotations *)
type ('a [@rec], 'b) mixed_type =
  | Left of 'a * ('a, 'b) mixed_type  
  | Right of 'b;;

(* Should work: recurses through first parameter which is contractive *)
type mixed_first = (mixed_first, int) mixed_type;;
[%%expect{|
type ('a [@rec], 'b) mixed_type =
    Left of 'a * ('a, 'b) mixed_type
  | Right of 'b
type mixed_first = (mixed_first, int) mixed_type
|}]

(* Should fail: recurses through second parameter which is not contractive *)
type bad_mixed = (int, bad_mixed) mixed_type;;
[%%expect{|
Line 1, characters 0-44:
1 | type bad_mixed = (int, bad_mixed) mixed_type;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation "bad_mixed" is cyclic:
         "bad_mixed" = "(int, bad_mixed) mixed_type",
         "(int, bad_mixed) mixed_type" contains "bad_mixed"
|}]

(* Abstract types with [@rec] *)
module AbstractContractive : sig
  type ('a [@rec]) t
  val create : 'a -> 'a t
  val get : 'a t -> 'a
end = struct
  type ('a [@rec]) t = { value : 'a }
  let create x = { value = x }
  let get t = t.value
end;;

type recursive_abstract = recursive_abstract AbstractContractive.t;;
[%%expect{|
module AbstractContractive :
  sig type ('a [@rec]) t val create : 'a -> 'a t val get : 'a t -> 'a end
type recursive_abstract = recursive_abstract AbstractContractive.t
|}]

(* Non-contractive abstract type should fail *)
module AbstractNonContractive : sig
  type 'a t
  val create : 'a -> 'a t  
end = struct
  type 'a t = 'a
  let create x = x
end;;

type bad_recursive = bad_recursive AbstractNonContractive.t;;
[%%expect{|
module AbstractNonContractive : sig type 'a t val create : 'a -> 'a t end
Line 9, characters 0-59:
9 | type bad_recursive = bad_recursive AbstractNonContractive.t;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation "bad_recursive" is cyclic:
         "bad_recursive" = "bad_recursive AbstractNonContractive.t",
         "bad_recursive AbstractNonContractive.t" contains "bad_recursive"
|}]

(* Object types - naturally contractive *)
type ('a [@rec]) obj_wrapper = < value : 'a >;;
type recursive_obj = recursive_obj obj_wrapper;;
[%%expect{|
type 'a obj_wrapper = < value : 'a >
type recursive_obj = recursive_obj obj_wrapper
|}]

(* Polymorphic variants - naturally contractive *)  
type ('a [@rec]) variant_wrapper = [ `Value of 'a ];;
type recursive_variant = recursive_variant variant_wrapper;;
[%%expect{|
type 'a variant_wrapper = [ `Value of 'a ]
type recursive_variant = recursive_variant variant_wrapper
|}]

(* Mutually recursive types *)
type ('a [@rec]) even_list =
  | ENil
  | ECons of 'a * 'a odd_list

and ('a [@rec]) odd_list =
  | OCons of 'a * 'a even_list;;

type very_even = very_even even_list;;
type very_odd = very_odd odd_list;;
[%%expect{|
type ('a [@rec]) even_list = ENil | ECons of 'a * 'a odd_list
and ('a [@rec]) odd_list = OCons of 'a * 'a even_list
type very_even = very_even even_list
type very_odd = very_odd odd_list
|}]

(* Module with multiple contractive parameters *)
module MultiParam : sig
  type ('a [@rec], 'b [@rec]) both_contractive
  type ('a [@rec], 'b) mixed_contractive
end = struct
  type ('a [@rec], 'b [@rec]) both_contractive = { a : 'a; b : 'b }
  type ('a [@rec], 'b) mixed_contractive = ('a, 'b) both_contractive
end;;

type double_recursive = 
  (double_recursive, double_recursive) MultiParam.both_contractive;;

type mixed_recursive = 
  (mixed_recursive, int) MultiParam.mixed_contractive;;
[%%expect{|
module MultiParam :
  sig
    type ('a [@rec], 'b [@rec]) both_contractive
    type ('a [@rec], 'b) mixed_contractive
  end
type double_recursive =
    (double_recursive, double_recursive) MultiParam.both_contractive
type mixed_recursive = (mixed_recursive, int) MultiParam.mixed_contractive
|}]

(* Definition-based contractiveness *)
type 'a non_contractive_record = { field: 'a };;
type ('a [@rec]) contractive_record2 = { field: 'a };;
type ('a [@rec]) good_wrapper = 'a contractive_record2;;
type recursive_good_wrapper = recursive_good_wrapper good_wrapper;;
[%%expect{|
type 'a non_contractive_record = { field : 'a; }
type ('a [@rec]) contractive_record2 = { field : 'a; }
type 'a good_wrapper = 'a contractive_record2
type recursive_good_wrapper = recursive_good_wrapper good_wrapper
|}]

type ('a [@rec]) bad_wrapper = 'a non_contractive_record;;
[%%expect{|
Line 1, characters 0-56:
1 | type ('a [@rec]) bad_wrapper = 'a non_contractive_record;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be recursive,
       but it is nonrecursive.
|}]

(* Edge cases *)

(* Empty datatype with [@rec] *)
type ('a [@rec]) empty_type = |;;
type recursive_empty = recursive_empty empty_type;;
[%%expect{|
type ('a [@rec]) empty_type = |
type recursive_empty = recursive_empty empty_type
|}]

(* Single constructor *)
type ('a [@rec]) single_constructor = Single of 'a;;
type recursive_single = recursive_single single_constructor;;
[%%expect{|
type ('a [@rec]) single_constructor = Single of 'a
type recursive_single = recursive_single single_constructor
|}]

(* Phantom type parameter *)
type ('a [@rec]) phantom = int;;
type recursive_phantom = recursive_phantom phantom;;
[%%expect{|
type 'a phantom = int
type recursive_phantom = recursive_phantom phantom
|}]

(* GADT with [@rec] *)
type ('a [@rec]) gadt =
  | Int : int gadt
  | String : string gadt;;

type recursive_gadt = recursive_gadt gadt;;
[%%expect{|
type ('a [@rec]) gadt = Int : int gadt | String : string gadt
type recursive_gadt = recursive_gadt gadt
|}]

(* Variance interaction tests *)
type (+'a [@rec]) covariant_rec = { foo : 'a option };;
type recursive_covariant = recursive_covariant covariant_rec;;
[%%expect{|
type ('a [@rec]) covariant_rec = { foo : 'a option; }
type recursive_covariant = recursive_covariant covariant_rec
|}]

type (-'a [@rec]) contravariant_rec = { foo :  'a -> unit };;
type recursive_contravariant = recursive_contravariant contravariant_rec;;
[%%expect{|
type ('a [@rec]) contravariant_rec = { foo : 'a -> unit; }
type recursive_contravariant = recursive_contravariant contravariant_rec
|}]

(* Custom list type that is contractive *)
type ('a [@rec]) my_list =
  | MyNil
  | MyCons of 'a * 'a my_list;;

type recursive_my_list = recursive_my_list my_list;;
[%%expect{|
type ('a [@rec]) my_list = MyNil | MyCons of 'a * 'a my_list
type recursive_my_list = recursive_my_list my_list
|}]

(* Custom option type that is contractive *)
type ('a [@rec]) my_option =
  | MyNone
  | MySome of 'a;;

type recursive_my_option = recursive_my_option my_option;;
[%%expect{|
type ('a [@rec]) my_option = MyNone | MySome of 'a
type recursive_my_option = recursive_my_option my_option
|}]

(* Recursive types via unification variables *)

(* Recursive types created via unification in types *)
type ('a [@rec]) container = C of 'a;;

let unify_recursive : ('a container as 'a) -> int =
  fun _ -> 0;;
[%%expect{|
type ('a [@rec]) container = C of 'a
val unify_recursive : ('a container as 'a) -> int = <fun>
|}]

(* Non-contractive unification should fail *)
type 'a non_contractive_container = 'a * int;;

let bad_unify_recursive : ('a non_contractive_container as 'a) -> int =
  fun _ -> 0;;
[%%expect{|
type 'a non_contractive_container = 'a * int
Line 3, characters 60-61:
3 | let bad_unify_recursive : ('a non_contractive_container as 'a) -> int =
                                                                ^
Error: This alias is bound to type "'a non_contractive_container" = "'a * int"
       but is used as an instance of type "'a"
       The type variable "'a" occurs inside "'a non_contractive_container"
|}]

(* Recursive types constructed during type inference *)

(* Recursive types created via unification during inference *)
let infer_recursive (make : 'a container -> 'a) f x = 
  f x; f (make x);;
[%%expect{|
val infer_recursive :
  (('a container as 'a) container -> 'a) -> ('a -> 'b) -> 'a -> 'b = <fun>
|}]

let bad_infer_recursive (make : 'a non_contractive_container -> 'a) f x = 
  f x; f (make x);;
[%%expect{|
Line 2, characters 9-17:
2 |   f x; f (make x);;
             ^^^^^^^^
Error: This expression has type "'a" but an expression was expected of type
         "'a non_contractive_container" = "'a * int"
       The type variable "'a" occurs inside "'a non_contractive_container"
|}]

(* Recursive types via GADT equations *)

(* Basic GADT equation creating recursive type *)
type ('a, 'b) eq = Refl : ('a, 'a) eq;;

type ('a [@rec]) boxed = Box of 'a;;

let gadt_recursive (type a) (Refl : (a, a boxed) eq) (x : a) : a boxed = x;;
[%%expect{|
type ('a, 'b) eq = Refl : ('a, 'a) eq
type ('a [@rec]) boxed = Box of 'a
val gadt_recursive : ('a, 'a boxed) eq -> 'a -> 'a boxed = <fun>
|}]

(* GADT equation with non-contractive type should fail *)
module M : sig
  type 'a unboxed
end = struct
  type 'a unboxed = 'a
end

let bad_gadt_recursive (type a) (Refl : (a, a M.unboxed) eq) (x : a) 
  : a M.unboxed = x;;
[%%expect{|
module M : sig type 'a unboxed end
Line 8, characters 18-19:
8 |   : a M.unboxed = x;;
                      ^
Error: This expression has type "a" but an expression was expected of type
         "a M.unboxed"
|}]

(* Constrained [@rec] parameters *)

(* [@rec] parameter used in non-contractive constraint should error *)
type ('a [@rec]) bad_constraint = 'b
  constraint 'a = < foo : 'b >;;
[%%expect{|
Lines 1-2, characters 0-30:
1 | type ('a [@rec]) bad_constraint = 'b
2 |   constraint 'a = < foo : 'b >..
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be recursive,
       but it is nonrecursive.
|}]

(* [@rec] parameter with type variable in contractive position *)
type ('a [@rec]) contractive_var_constraint = < bar : 'b >
  constraint 'a = < foo : 'b >;;
[%%expect{|
type 'a contractive_var_constraint = < bar : 'b >
  constraint 'a = < foo : 'b >
|}]

(* Using non-contractive type in contractive position *)
type 'a non_contractive = 'a;;
type ('a [@rec]) bad_wrapper = 'a non_contractive;;
[%%expect{|
type 'a non_contractive = 'a
Line 2, characters 0-49:
2 | type ('a [@rec]) bad_wrapper = 'a non_contractive;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be recursive,
       but it is nonrecursive.
|}]

(* Function types with [@rec] *)
type ('a [@rec]) func_wrapper = 'a -> int;;
type recursive_func = recursive_func func_wrapper;;
[%%expect{|
Line 1, characters 0-41:
1 | type ('a [@rec]) func_wrapper = 'a -> int;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be recursive,
       but it is nonrecursive.
|}]

(* Tuple with [@rec] *)
type ('a [@rec]) tuple_wrapper = 'a * int;;
type recursive_tuple = recursive_tuple tuple_wrapper;;
[%%expect{|
Line 1, characters 0-41:
1 | type ('a [@rec]) tuple_wrapper = 'a * int;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be recursive,
       but it is nonrecursive.
|}]

(* Deep recursion through structural types *)
type ('a [@rec]) deep = ('a * int) list;;
type recursive_deep = recursive_deep deep;;
[%%expect{|
Line 1, characters 0-39:
1 | type ('a [@rec]) deep = ('a * int) list;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be recursive,
       but it is nonrecursive.
|}]

(* Type parameters in constraints with invalid usage *)
type ('a [@rec]) constrained = 'b constraint 'b = int * 'a;;
[%%expect{|
Line 1, characters 0-58:
1 | type ('a [@rec]) constrained = 'b constraint 'b = int * 'a;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be recursive,
       but it is nonrecursive.
|}]

(* Constraint with mixed contractive/non-contractive variables *)
type 'a mixed_constraint = 'b * < foo : 'c > constraint 'a = 'b * 'c;;
type ('a [@rec]) bad_mixed_wrapper = 'a mixed_constraint;;
[%%expect{|
type 'a mixed_constraint = 'b * < foo : 'c > constraint 'a = 'b * 'c
Line 2, characters 0-56:
2 | type ('a [@rec]) bad_mixed_wrapper = 'a mixed_constraint;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be recursive,
       but it is nonrecursive.
|}]

(* Private types with [@rec] *)
type ('a [@rec]) private_contractive = private P of 'a;;
type test_private = test_private private_contractive;;
[%%expect{|
type ('a [@rec]) private_contractive = private P of 'a
type test_private = test_private private_contractive
|}]

(* Deep nesting *)
type ('a [@rec]) deep1 = D1 of 'a;;
type ('a [@rec]) deep2 = D2 of 'a deep1;;
type ('a [@rec]) deep3 = D3 of 'a deep2;;
type ('a [@rec]) deep4 = D4 of 'a deep3;;
type ('a [@rec]) deep5 = D5 of 'a deep4;;
type test_deep = test_deep deep5;;
[%%expect{|
type ('a [@rec]) deep1 = D1 of 'a
type ('a [@rec]) deep2 = D2 of 'a deep1
type ('a [@rec]) deep3 = D3 of 'a deep2
type ('a [@rec]) deep4 = D4 of 'a deep3
type ('a [@rec]) deep5 = D5 of 'a deep4
type test_deep = test_deep deep5
|}]

(* Recursive type with multiple occurrences *)
type ('a [@rec]) multi_occur = 
  | Branch of 'a * 'a multi_occur * 'a multi_occur
  | Leaf of 'a;;
type test_multi = test_multi multi_occur;;
[%%expect{|
type ('a [@rec]) multi_occur =
    Branch of 'a * 'a multi_occur * 'a multi_occur
  | Leaf of 'a
type test_multi = test_multi multi_occur
|}]

(* Inclusion checks *)

module Inclusion_contractive : sig
  type ('a [@rec]) t = { value : 'a }
end = struct
  type ('a [@rec]) t = { value : 'a }
end;;
[%%expect{|
module Inclusion_contractive : sig type ('a [@rec]) t = { value : 'a; } end
|}]

module Inclusion_noncontractive : sig
  type 'a t = { value : 'a }
end = struct
  type ('a [@rec]) t = { value : 'a }
end;;
[%%expect{|
module Inclusion_noncontractive : sig type 'a t = { value : 'a; } end
|}]

module Bad_inclusion_contractive : sig
  type ('a [@rec]) t = { value : 'a }
end = struct
  type 'a t = { value : 'a }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = { value : 'a }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = { value : 'a; } end
       is not included in
         sig type ('a [@rec]) t = { value : 'a; } end
       Type declarations do not match:
         type 'a t = { value : 'a; }
       is not included in
         type ('a [@rec]) t = { value : 'a; }
       Their variances do not agree.
|}]

module Abstract_inclusion_contractive : sig
  type ('a [@rec]) t
end = struct
  type ('a [@rec]) t = { value : 'a }
end;;
[%%expect{|
module Abstract_inclusion_contractive : sig type ('a [@rec]) t end
|}]

module Abstract_inclusion_noncontractive : sig
  type 'a t
end = struct
  type ('a [@rec]) t = { value : 'a }
end;;
[%%expect{|
module Abstract_inclusion_noncontractive : sig type 'a t end
|}]

module Bad_abstract_inclusion_contractive : sig
  type ('a [@rec]) t
end = struct
  type 'a t = { value : 'a }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = { value : 'a }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = { value : 'a; } end
       is not included in
         sig type ('a [@rec]) t end
       Type declarations do not match:
         type 'a t = { value : 'a; }
       is not included in
         type ('a [@rec]) t
       Their variances do not agree.
|}]
