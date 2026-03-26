(* TEST
 flags = "-extension-universe upstream_compatible";
 expect;
*)

(* Some [@@unboxed] existentials are non-separable and thus forbidden. *)
(* CR separability: mark them as non-separable instead. *)

type 'a abstract

type packed = P : 'a abstract -> packed [@@unboxed]
[%%expect{|
type 'a abstract
Line 3, characters 0-51:
3 | type packed = P : 'a abstract -> packed [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

(* [non_float] annotations allow us to bypass this check, but are erased. *)
type 'a non_float : value mod non_float

type packed = P : 'a non_float -> packed [@@unboxed]

[%%expect{|
type 'a non_float : value mod non_float
Line 3, characters 0-52:
3 | type packed = P : 'a non_float -> packed [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 187 [incompatible-with-upstream]: This type relies on OxCaml's extended separability checking
and would not be accepted by upstream OCaml.

type packed = P : 'a non_float -> packed [@@unboxed]
|}]

type 'a non_float_external : value mod non_float external_
[%%expect{|
type 'a non_float_external : value mod external_ non_float
|}]

(* CR separability: this type should warn, but the incompatibility-with-upstream
   warning doesn't account for the fact that externality crossing
   information is not available upstream *)
type packed_non_float_external =
    P : 'a non_float_external -> packed_non_float_external [@@unboxed]
[%%expect{|
type packed_non_float_external =
    P : 'a non_float_external -> packed_non_float_external [@@unboxed]
|}]


(* [: immediate] gets erased to [@@immediate] and is upstream-compatible. *)

type 'a immediate : immediate

type packed = P : 'a immediate -> packed [@@unboxed]

[%%expect{|
type 'a immediate : immediate
type packed = P : 'a immediate -> packed [@@unboxed]
|}]

(* Annots on existential variables. *)

type exists = E : ('a : value mod non_float) . 'a -> exists [@@unboxed]

[%%expect{|
Line 1, characters 0-71:
1 | type exists = E : ('a : value mod non_float) . 'a -> exists [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 187 [incompatible-with-upstream]: This type relies on OxCaml's extended separability checking
and would not be accepted by upstream OCaml.

type exists = E : ('a : value mod non_float). 'a -> exists [@@unboxed]
|}]


(* Non-value layouts. *)

type 'a void_not_external : void with string

type packed_void_not_external =
    P : 'a void_not_external -> packed_void_not_external [@@unboxed]
[%%expect{|
type 'a void_not_external : void mod unique_implies_uncontended non_float
Lines 3-4, characters 0-68:
3 | type packed_void_not_external =
4 |     P : 'a void_not_external -> packed_void_not_external [@@unboxed]
Warning 187 [incompatible-with-upstream]: This type relies on OxCaml's extended separability checking
and would not be accepted by upstream OCaml.

type packed_void_not_external =
    P : 'a void_not_external -> packed_void_not_external [@@unboxed]
|}]

type exists_word = W : ('a : word) . 'a -> exists_word [@@unboxed]

[%%expect{|
Line 1, characters 0-66:
1 | type exists_word = W : ('a : word) . 'a -> exists_word [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 187 [incompatible-with-upstream]: This type relies on OxCaml's extended separability checking
and would not be accepted by upstream OCaml.

type exists_word = W : ('a : word). 'a -> exists_word [@@unboxed]
|}]
