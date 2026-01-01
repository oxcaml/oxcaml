(* TEST
 expect;
*)

(* This is a regression test for the bug fixed by
   https://github.com/oxcaml/oxcaml/pull/5178 *)

type t = int or_null

module type S = sig
  type t : any mod separable
end

[%%expect{|
type t = int or_null
module type S = sig type t : any separable end
|}]


module type S' = S with type t = t

[%%expect{|
module type S' = sig type t = t end
|}]


module M : sig
  type t : immediate_or_null & value
end = struct
  type t = #(int or_null * string)
end
[%%expect{|
module M : sig type t : value_or_null non_pointer & value end
|}]

type t : immediate_or_null = #{ i : int or_null }
[%%expect{|
type t = #{ i : int or_null; }
|}]

type 'a t : value_or_null mod global  = #{ i : 'a or_null @@ global }
[%%expect{|
type 'a t = #{ i : 'a or_null @@ global; }
|}]

type 'a bad : value_or_null mod global  = #{ i : 'a or_null @@ portable }
[%%expect{|
Line 1, characters 0-73:
1 | type 'a bad : value_or_null mod global  = #{ i : 'a or_null @@ portable }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "bad" is
           value_or_null mod everything with 'a @@ portable
         because it is an unboxed record.
       But the kind of type "bad" must be a subkind of value_or_null mod global
         because of the annotation on the declaration of the type bad.

       The first mode-crosses less than the second along:
         locality: mod global with 'a ≰ mod global
         uniqueness: mod aliased with 'a ≰ mod aliased
         forkable: mod forkable with 'a ≰ mod forkable
         yielding: mod unyielding with 'a ≰ mod unyielding
|}]

(* This is a regression test for an intermediate version of the bug fix that
   errored when estimating the kind of ['a portended or_null] because ['a
   portended] is [maybe_null] before ['a] is lowered (and thus can't be an
   argument to [or_null]). *)
type ('a : value_or_null) portended = { a : 'a } [@@unboxed]
let peek  (aon : 'a portended or_null) =
  match aon with
  | This _ -> assert false
  | Null -> assert false
;;
[%%expect{|
type ('a : value_or_null) portended = { a : 'a; } [@@unboxed]
val peek : ('a : value maybe_separable) 'b. 'a portended or_null -> 'b =
  <fun>
|}]


type t : scannable & scannable = #(int or_null or_null * int)
[%%expect{|
Line 1, characters 35-46:
1 | type t : scannable & scannable = #(int or_null or_null * int)
                                       ^^^^^^^^^^^
Error: This type "int or_null" should be an instance of type
         "('a : value maybe_separable)"
       The layout of int or_null is value maybe_separable maybe_null
         because it is the primitive type or_null.
       But the layout of int or_null must be a sublayout of
           value maybe_separable
         because the type argument of or_null has layout value.
|}]
