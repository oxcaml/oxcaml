(* TEST
 flat-float-array;
 expect;
*)

(* Separability and [@@unboxed] existential types whose acceptance
   depends on the flat float array optimization.
   See separability.ml for tests that are independent of the
   optimization. *)

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
