(* TEST
 flags = "-extension-universe upstream_compatible";
 no-flat-float-array;
 expect;
*)

(* See separability_upstream_compatible.ml for the bulk of the tests. This
   file collects the cases that depend on the flat float array optimization
   being disabled. *)

type 'a abstract

type packed = P : 'a abstract -> packed [@@unboxed]
[%%expect{|
type 'a abstract
type packed = P : 'a abstract -> packed [@@unboxed]
|}]
