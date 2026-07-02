(* TEST
 flags = "-extension layouts_beta";
 flat-float-array;
 expect;
*)

(* See hash_types.ml for the bulk of the tests. This file collects the
   case that depends on the flat float array optimization being enabled. *)

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
