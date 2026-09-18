(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 flat-float-array;
 {
   expect;
 }
*)

(* See separability.ml for the bulk of separability tests. This file
   collects the cases that depend on the flat float array optimization
   being enabled: with the optimization on, [@@unboxed] existentials
   that might contain both float and non-float values are rejected. *)

type 'a r = #{ a : 'a }
type bad = F : 'a r -> bad [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
Line 2, characters 0-38:
2 | type bad = F : 'a r -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type 'a r = #{ a : 'a }
type bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
Line 2, characters 0-46:
2 | type bad = F : { x : 'a r } -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type 'a r = #{ a : 'a }
and 'a r2 = #{ a : 'a r }
and bad = F : 'a r2 -> bad [@@unboxed]
[%%expect{|
Line 3, characters 0-38:
3 | and bad = F : 'a r2 -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type 'a r = #{ a : 'a }
and bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
Line 2, characters 0-45:
2 | and bad = F : { x : 'a r } -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]
