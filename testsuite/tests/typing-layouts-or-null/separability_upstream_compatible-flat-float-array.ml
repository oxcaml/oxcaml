(* TEST
 flags = "-extension-universe upstream_compatible";
 flat-float-array;
 expect;
*)

(* See separability_upstream_compatible.ml for the bulk of the tests.
   This file collects the cases that depend on the flat float array
   optimization being enabled. *)

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
