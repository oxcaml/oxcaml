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
[%%expect {|
type 'a t = {
  a : 'a;
  b : int list list list list list list list list list list list list list @@
    portable;
}
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
|}]
(* Accepting this line requires at least 2 fuel, which is less than normal but
   more than we use if we ran out previously. *)
let f (t : int list list list t) = require_portable t
[%%expect {|
val f : int list list list t -> unit = <fun>
|}]

(* Test the same scenario, except it requires remembering that we ran out of
   fuel across a compilation-unit boundary. *)

#directory "ocamlc.byte";;
#load "foo.cmo";;

type 'a t = 'a Foo.t
let require_portable (_ : (_ : value mod portable)) = ()
[%%expect {|
type 'a t = 'a Foo.t
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
|}]
let f (t : int list list list Foo.t) = require_portable t
[%%expect {|
val f : int list list list Foo.t -> unit = <fun>
|}]
