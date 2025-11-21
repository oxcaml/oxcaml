(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   expect;
 }
*)

(*****************************)
(* Unboxed records with void *)

type t_void  : void

type ('a : void) t = #{ x : 'a ; y : t_void }
[%%expect{|
type t_void : void
type ('a : void) t = #{ x : 'a; y : t_void; }
|}]

type t = { x : t_void } [@@unboxed]
[%%expect{|
type t = { x : t_void; } [@@unboxed]
|}]

type bad : void = #{ bad : bad }
[%%expect{|
Line 1, characters 0-32:
1 | type bad : void = #{ bad : bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]

type ('a : void) bad  = #{ bad : 'a bad ; u : 'a}
[%%expect{|
Line 1, characters 0-49:
1 | type ('a : void) bad  = #{ bad : 'a bad ; u : 'a}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "'a bad" contains "'a bad"
|}]

(******************************************************************************)
(* The below is adapted from
   [testsuite/tests/typing-layouts-products/basics_alpha.ml]. *)

(* [t3] is allowed for unboxed tuples, and disallowed for (un)boxed records *)
type t1 : any mod non_null separable
type t2 : value
type t3 : any mod non_null separable = #{ t1 : t1 ; t2 : t2};;
[%%expect{|
type t1 : any mod non_null separable
type t2
type t3 = #{ t1 : t1; t2 : t2; }
|}]

(* CR layouts v7.2: once [any] is allowed in unboxed record declarations, check
   that [non_null] behaves correctly in the following tests. *)

type t1 : any mod non_null separable
type t2 : value
type t3 : any & value mod non_null separable = #{ t1 : t1 ; t2 : t2};;
[%%expect{|
type t1 : any mod non_null separable
type t2
type t3 = #{ t1 : t1; t2 : t2; }
|}]

type t1 : any mod non_null separable
type t2 : value
type t3 : (any mod non_null separable) & (value mod non_null separable) = #{ t1 : t1 ; t2 : t2};;
[%%expect{|
type t1 : any mod non_null separable
type t2
type t3 = #{ t1 : t1; t2 : t2; }
|}]

type t1 : any
type t2 : any mod non_null separable
type t3 : any & (any mod non_null separable) = #{ t1 : t1 ; t2 : t2 };;
[%%expect{|
type t1 : any
type t2 : any mod non_null separable
type t3 = #{ t1 : t1; t2 : t2; }
|}]

(* Should not be allowed for either unboxed tuples or (un)boxed records. *)
type t1 : any
type t2 : any mod non_null separable
type t3 : any mod non_null separable = #{ t1 : t1 ; t2 : t2 };;
[%%expect{|
type t1 : any
type t2 : any mod non_null separable
type t3 = #{ t1 : t1; t2 : t2; }
|}]

type t1 : any
type t2 : any mod non_null separable
type t3 : any & any mod non_null separable = #{ t1 : t1 ; t2 : t2 };;
[%%expect{|
type t1 : any
type t2 : any mod non_null separable
type t3 = #{ t1 : t1; t2 : t2; }
|}]

type t1 : any
type t2 : any mod non_null separable
type t3 : (any mod non_null separable) & (any mod non_null separable) = #{ t1 : t1 ; t2 : t2 };;
[%%expect{|
type t1 : any
type t2 : any mod non_null separable
type t3 = #{ t1 : t1; t2 : t2; }
|}]

type ur1 = #{ a : int64#; b : float# }
and ur4 = #{ a : ur1 }
[%%expect{|
type ur1 = #{ a : int64#; b : float#; }
and ur4 = #{ a : ur1; }
|}]
