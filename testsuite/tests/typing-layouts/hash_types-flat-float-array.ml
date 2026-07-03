(* TEST
 flags = "-extension layouts_beta";
 flat-float-array;
 expect;
*)

(* See hash_types.ml for the bulk of the tests. This file collects the
   cases that depend on the flat float array optimization being enabled. *)

type 'a t = { i : 'a }
and bad = P : 'a t# -> bad [@@unboxed]
[%%expect{|
Line 2, characters 0-38:
2 | and bad = P : 'a t# -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

(* The negative counterpart of the [array#]/[iarray#] separability tests in
   hash_types.ml: an abstract any-layout type's parameter gets the worst-case
   mode, so the existential is rejected. *)
type ('a : value) abstr : any
type bad = B : 'a abstr -> bad [@@unboxed]
[%%expect{|
type 'a abstr : any
Line 2, characters 0-42:
2 | type bad = B : 'a abstr -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]
