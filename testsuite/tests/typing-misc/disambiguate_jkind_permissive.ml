(* TEST
 expect;
*)

(* Test that type-directed disambiguation selects the most permissive
   type when multiple aliases provide the same constructor.
   
   When we have type aliases with different jkind constraints pointing to
   the same type, disambiguation should select the original (most expanded)
   type to avoid unnecessary jkind constraints. *)

type 'a t = A of 'a

(* Create an alias with a more restrictive jkind *)
type ('a : immediate) t_immediate = 'a t = A of 'a

(* This should work: disambiguation should use the original 'a t
   which has no immediate constraint *)
let f (type a) (a : a) : a t = A a
[%%expect{|
type 'a t = A of 'a
type ('a : immediate) t_immediate = 'a t = A of 'a
val f : 'a -> 'a t = <fun>
|}]

(* Multiple levels of aliases *)
type ('a : immediate) t_immediate2 = 'a t_immediate = A of 'a
type ('a : immediate) t_immediate3 = 'a t_immediate2 = A of 'a

let g (type a) (a : a) : a t = A a
[%%expect{|
type ('a : immediate) t_immediate2 = 'a t_immediate = A of 'a
type ('a : immediate) t_immediate3 = 'a t_immediate2 = A of 'a
val g : 'a -> 'a t = <fun>
|}]

(* Direct use of constrained type should still enforce constraints *)
let h (type a) (a : a) : a t_immediate = A a
[%%expect{|
Line 1, characters 25-26:
1 | let h (type a) (a : a) : a t_immediate = A a
                             ^
Error: This type "a" should be an instance of type "('a : immediate)"
       The kind of a is value
         because it is or unifies with an unannotated universal variable.
       But the kind of a must be a subkind of immediate
         because of the definition of t_immediate at line 4, characters 0-50.
|}]

(* Confirm that when we explicitly use the immediate-constrained type,
   it works with immediate types *)
let h_fixed (x : int) : int t_immediate = A x
[%%expect{|
val h_fixed : int -> int t_immediate = <fun>
|}]

(* Test with record types too *)
type 'a r = { x : 'a }
type ('a : immediate) r_immediate = 'a r = { x : 'a }

let make_r (type a) (a : a) : a r = { x = a }
[%%expect{|
type 'a r = { x : 'a; }
type ('a : immediate) r_immediate = 'a r = { x : 'a; }
val make_r : 'a -> 'a r = <fun>
|}]

(* Project from a record - should also use the most permissive type *)
let project_r (type a) (r : a r) : a = r.x
[%%expect{|
val project_r : 'a r -> 'a = <fun>
|}]

(* Test that the correct constructor is marked as used for warnings.
   This test module structure ensures M2 is opened last. *)
module M1 = struct
  type 'a t = A of 'a
  type 'a first = 'a t
end

module M2 = struct  
  type 'a t = A of 'a
end

open M1
open M2

(* This should fail because the most recent t is from M2 *)
let fail_case (type a) (a : a) : a M1.first = (A a : a t)
[%%expect{|
module M1 : sig type 'a t = A of 'a type 'a first = 'a t end
module M2 : sig type 'a t = A of 'a end
Line 14, characters 46-57:
14 | let fail_case (type a) (a : a) : a M1.first = (A a : a t)
                                                   ^^^^^^^^^^^
Error: This expression has type "a M2.t" but an expression was expected of type
         "a M1.first" = "a M1.t"
|}]