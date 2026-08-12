(* TEST
 no-flat-float-array;
 expect;
*)

(* See separability.ml for the bulk of separability tests. This file
   collects the cases that depend on the flat float array optimization
   being disabled: with the optimization off, [@@unboxed] existentials
   that might contain both float and non-float values are accepted
   because there is no float array optimization to break. *)

type 'a abstract

type packed = P : 'a abstract -> packed [@@unboxed]
[%%expect{|
type 'a abstract
type packed = P : 'a abstract -> packed [@@unboxed]
|}]
