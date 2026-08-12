(* TEST
 flags = "-extension layouts_beta";
 no-flat-float-array;
 expect;
*)

(* See hash_types.ml for the bulk of the tests. This file collects the
   case that depends on the flat float array optimization being disabled. *)

type 'a t = { i : 'a }
and bad = P : 'a t# -> bad [@@unboxed]
[%%expect{|
type 'a t = { i : 'a; }
and bad = P : 'a t# -> bad [@@unboxed]
|}]
