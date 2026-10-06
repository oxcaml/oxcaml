(* GPR#816 *)
(* This PR means that Foo(Bar).t is known to be equal to Foo(Baz).t
   when Bar is an alias for Baz, even when the definition for Foo is unknown.
   This can happen when .cmi files depend on other .cmi files not in the path
   -- a situation that is partially supported. *)

module A = M

type t1 = M.Foo(M).t
type t2 = A.Foo(A).t

(* Also test that we don't need the .cmi for a type to be present to know a
   constructor argument is representable *)

type v = A of A.a | Z

(* [M.pair] cannot be expanded when compiling [main_ok.ml]. *)
type ('a, 'b : any) any_record = { x : 'a; y : 'b }
  constraint 'a = int * int

let use_any_record (_ : (M.pair, float#) any_record) = ()

type ('a, 'b : any) any_variant = V of 'a * 'b
  constraint 'a = int * int

let use_any_variant (_ : (M.pair, float#) any_variant) = ()
