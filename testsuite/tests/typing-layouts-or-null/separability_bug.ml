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
Line 1, characters 17-34:
1 | module type S' = S with type t = t
                     ^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = t
       is not included in
         type t : any separable
       The layout of the first is value maybe_separable maybe_null
         because it is the primitive type or_null.
       But the layout of the first must be a sublayout of any separable
         because of the definition of t at line 4, characters 2-28.
|}]


module M : sig
  type t : immediate_or_null & value
end = struct
  type t = #(int or_null * string)
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #(int or_null * string)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #(int or_null * string) end
       is not included in
         sig type t : value_or_null non_pointer & value end
       Type declarations do not match:
         type t = #(int or_null * string)
       is not included in
         type t : value_or_null non_pointer & value
       The layout of the first is
           value maybe_separable maybe_null & value non_float
         because it is an unboxed tuple.
       But the layout of the first must be a sublayout of
           value non_pointer maybe_null & value
         because of the definition of t at line 2, characters 2-36.
|}]

type t : immediate_or_null = #{ i : int or_null }
[%%expect{|
Line 1, characters 0-49:
1 | type t : immediate_or_null = #{ i : int or_null }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is value maybe_separable maybe_null
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of
           value non_pointer maybe_null
         because of the annotation on the declaration of the type t.
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
