(* TEST
 readonly_files = "with_bounds_a.ml with_bounds_b.ml";
 setup-ocamlc.byte-build-env;
 module = "with_bounds_a.ml";
 ocamlc.byte;
 module = "with_bounds_b.ml";
 ocamlc.byte;
 script = "rm -f with_bounds_a.cmi";
 script;
 expect;
*)

#directory "ocamlc.byte";;
#load "with_bounds_b.cmo";;

(* Normalizing the with-bounds of these types visits [With_bounds_a.t] more
   than once, even though its cmi is missing. *)

type r = { x : With_bounds_b.v; y : With_bounds_b.w }

[%%expect{|
type r = { x : With_bounds_b.v; y : With_bounds_b.w; }
|}]

(* The missing type stays in the with-bounds. *)

type s : value mod portable = r

[%%expect{|
Line 1, characters 0-31:
1 | type s : value mod portable = r
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "r" is immutable_data with With_bounds_a.t
         because of the definition of r at line 1, characters 0-53.
       But the kind of type "r" must be a subkind of value mod portable
         because of the definition of s at line 1, characters 0-31.
|}]

let f (r : With_bounds_b.r) = match r with { y; _ } -> y

[%%expect{|
val f : With_bounds_b.r -> int = <fun>
|}]
