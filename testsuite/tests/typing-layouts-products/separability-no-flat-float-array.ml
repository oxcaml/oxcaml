(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 no-flat-float-array;
 {
   expect;
 }
*)

(* See separability.ml for the bulk of separability tests. This file
   collects the cases that depend on the flat float array optimization
   being disabled: with the optimization off, [@@unboxed] existentials
   that might contain both float and non-float values are accepted
   because there is no float array optimization to break. *)

type 'a r = #{ a : 'a }
type bad = F : 'a r -> bad [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
type bad = F : 'a r -> bad [@@unboxed]
|}]

type 'a r = #{ a : 'a }
type bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
type bad = F : { x : 'a r; } -> bad [@@unboxed]
|}]

type 'a r = #{ a : 'a }
and 'a r2 = #{ a : 'a r }
and bad = F : 'a r2 -> bad [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
and 'a r2 = #{ a : 'a r; }
and bad = F : 'a r2 -> bad [@@unboxed]
|}]

type 'a r = #{ a : 'a }
and bad = F : { x : 'a r } -> bad [@@unboxed]
[%%expect{|
type 'a r = #{ a : 'a; }
and bad = F : { x : 'a r; } -> bad [@@unboxed]
|}]
