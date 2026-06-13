(* TEST
 flags = "-extension-universe upstream_compatible";
 expect;
*)

type void : void mod everything
[%%expect{|
type void : void mod everything
|}]

type t = A of void [@all_void_constructor]
[%%expect{|
Line 1, characters 0-42:
1 | type t = A of void [@all_void_constructor]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 187 [incompatible-with-upstream]: This variant is immediate
  because all its constructors have all-void arguments,
  but after erasure for upstream compatibility, void is no longer zero-width,
  so it won't be immediate.

type t = A of void
|}]

(* Multiple constructors, all void *)
type t2 =
  | A of void [@all_void_constructor]
  | B of void * void [@all_void_constructor]
  | C of #(void * void) [@all_void_constructor]
[%%expect{|
Lines 1-4, characters 0-47:
1 | type t2 =
2 |   | A of void [@all_void_constructor]
3 |   | B of void * void [@all_void_constructor]
4 |   | C of #(void * void) [@all_void_constructor]
Warning 187 [incompatible-with-upstream]: This variant is immediate
  because all its constructors have all-void arguments,
  but after erasure for upstream compatibility, void is no longer zero-width,
  so it won't be immediate.

type t2 = A of void | B of void * void | C of #(void * void)
|}]

(* Mixed - should not warn *)
type t3 = A of void [@all_void_constructor] | B of int
[%%expect{|
type t3 = A of void | B of int
|}]

(* No args - should not warn *)
type t4 = A | B | C
[%%expect{|
type t4 = A | B | C
|}]
