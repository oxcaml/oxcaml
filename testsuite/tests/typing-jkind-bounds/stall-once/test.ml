(* TEST
   readonly_files = "foo.ml";
   setup-ocamlc.byte-build-env;
   module = "foo.ml";
   ocamlc.byte;
   expect;
*)

(* This file tests that we reduce the amount of fuel for normalization if we
   ran out of fuel previously. This causes us to accept less programs, but it
   is an important optimization. *)

(* We run out of fuel normalizing the jkind on this decl because of [b]. *)
type 'a t =
  { a : 'a;
    b : int list list list list list list list list list list list list list @@ portable
  }
let require_portable (_ : (_ : value mod portable)) = ()
(* Accepting this line requires at least 2 fuel, which is less than normal but
   more than we use if we ran out previously. *)
let f (t : int list list list t) = require_portable t
[%%expect {|
type 'a t = {
  a : 'a;
  b : int list list list list list list list list list list list list list @@
    portable;
}
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
Line 8, characters 52-53:
8 | let f (t : int list list list t) = require_portable t
                                                        ^
Error: This expression has type "int list list list t"
       but an expression was expected of type "('a : value mod portable)"
       The kind of int list list list t is
           immutable_data
             with int list list list

             with int list list list list list list list list list list list list list
                    @@
                    portable
         because of the definition of t at lines 1-4, characters 0-3.
       But the kind of int list list list t must be a subkind of
           value mod portable
         because of the definition of require_portable at line 5, characters 21-56.

       The first mode-crosses less than the second along:
         portability: mod portable with int list list list ≰ mod portable
       Note: I gave up trying to find the simplest kind for the first,
       as it is very large or deeply recursive.
|}]

(* Test the same scenario, except it requires remembering that we ran out of
   fuel across a compilation-unit boundary. *)

#directory "ocamlc.byte";;
#load "foo.cmo";;

type 'a t = 'a Foo.t
let require_portable (_ : (_ : value mod portable)) = ()
let f (t : int list list list Foo.t) = require_portable t
[%%expect {|
type 'a t = 'a Foo.t
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
Line 3, characters 56-57:
3 | let f (t : int list list list Foo.t) = require_portable t
                                                            ^
Error: This expression has type "int list list list Foo.t"
       but an expression was expected of type "('a : value mod portable)"
       The kind of int list list list Foo.t is
           immutable_data
             with int list list list

             with int list list list list list list list list list list list list list
                    @@
                    portable.
       But the kind of int list list list Foo.t must be a subkind of
           value mod portable
         because of the definition of require_portable at line 2, characters 21-56.

       The first mode-crosses less than the second along:
         portability: mod portable with int list list list ≰ mod portable
       Note: I gave up trying to find the simplest kind for the first,
       as it is very large or deeply recursive.
|}]
