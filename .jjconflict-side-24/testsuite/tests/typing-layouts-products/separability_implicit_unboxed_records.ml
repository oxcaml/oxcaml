(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   expect;
 }
*)

(* NOTE: When adding tests to this file, consider updating
   [typing-layouts-products/separability.ml] *)

type 'a r = { a : 'a }
and 'a ok = F : 'a r# -> 'a ok [@@unboxed]
[%%expect{|
type 'a r = { a : 'a; }
and 'a ok = F : 'a r# -> 'a ok [@@unboxed]
|}]

type 'a r = { a : 'a }
and 'a ok = F : { x : 'a r# } -> 'a ok [@@unboxed]
[%%expect{|
type 'a r = { a : 'a; }
and 'a ok = F : { x : 'a r#; } -> 'a ok [@@unboxed]
|}]


(* See separability_implicit_unboxed_records-no-flat-float-array.ml for [@@unboxed]
   existential tests whose acceptance depends on the flat float array
   optimization. *)


(* #(value & void) and similar kinds are always considered separable,
   since we don't apply the float array optimization for them. *)
type t_void : void
and 'a r = { a : 'a ; v : t_void }
and ok = F : 'a r# -> ok [@@unboxed]
[%%expect{|
type t_void : void
and 'a r = { a : 'a; v : t_void; }
and ok = F : 'a r# -> ok [@@unboxed]
|}]

type t_void : void
and 'a r = { a : 'a ; v : t_void }
and ok = F : { x : 'a r# } -> ok [@@unboxed]
[%%expect{|
type t_void : void
and 'a r = { a : 'a; v : t_void; }
and ok = F : { x : 'a r#; } -> ok [@@unboxed]
|}]
