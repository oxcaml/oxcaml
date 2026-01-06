(* TEST
   flags = "-extension layout_poly_alpha";
   expect;
*)

exception Force_type
;;

(** Instantiation. Currently fails, will pass in the future. *)
let const_one : (repr_ 'a). 'a -> int =
  let f x = 1 in f
;;
[%%expect {|
exception Force_type
Line 6, characters 17-18:
6 |   let f x = 1 in f
                     ^
Error: This expression has type "'b -> int"
       but an expression was expected of type "(repr_ 'a). 'a -> int"
|}];;

(** [(repr_ 'a). t] and [t] do not unify. *)
let const_one_with_fun : (repr_ 'a). 'a -> int =
  fun (x : any) -> 1
;;
[%%expect {|
Line 2, characters 2-20:
2 |   fun (x : any) -> 1
      ^^^^^^^^^^^^^^^^^^
Error: This expression should not be a function, the expected type is
       "(repr_ 'a). 'a -> int"
|}];;

(** Polymorphic identity, should succeed with [let]. Currently fails. *)
let ident : (repr_ 'a). 'a -> 'a =
  let f x = x in f
;;
[%%expect {|
Line 2, characters 17-18:
2 |   let f x = x in f
                     ^
Error: This expression has type "'b -> 'b"
       but an expression was expected of type "(repr_ 'a). 'a -> 'a"
|}];;

(** Polymorphic identity, fails with [fun x -> x]. *)
let ident : (repr_ 'a). 'a -> 'a =
  fun x -> x
;;
[%%expect {|
Line 2, characters 2-12:
2 |   fun x -> x
      ^^^^^^^^^^
Error: This expression should not be a function, the expected type is
       "(repr_ 'a). 'a -> 'a"
|}];;

(** These test the interaction between Tpoly and Trepr. *)

let const_fun_repr_arg : ((repr_ 'a). 'a -> unit) -> unit =
  fun _ -> ();;
[%%expect {|
val const_fun_repr_arg : ((repr_ 'a). 'a -> unit) -> unit = <fun>
|}]

let poly_and_repr_1 : 'b. ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  fun _ -> ();;
[%%expect {|
val poly_and_repr_1 : ((repr_ 'a). 'a -> 'b -> unit) -> unit = <fun>
|}]

let poly_and_repr_2 : ('b : any). ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  fun _ -> ();;
[%%expect {|
val poly_and_repr_2 : ('b : any). ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  <fun>
|}]

let poly_and_repr_3 : ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  fun _ -> ();;
[%%expect {|
val poly_and_repr_3 : ('b : any). ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  <fun>
|}]

let poly_and_repr_4 : ('b : value). ((repr_ 'a). 'a -> 'b -> unit) -> unit =
  fun _ -> ();;
[%%expect {|
val poly_and_repr_4 : ((repr_ 'a). 'a -> 'b -> unit) -> unit = <fun>
|}]

(** [Tpoly] is not an instance of [Trepr]. *)
module Test_1 : sig
  val x : ((repr_ 'a). 'a -> unit) -> unit
end = struct
  let x (_ : 'a. 'a -> unit) = ()
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x (_ : 'a. 'a -> unit) = ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val x : ('a. 'a -> unit) -> unit end
       is not included in
         sig val x : ((repr_ 'a). 'a -> unit) -> unit end
       Values do not match:
         val x : ('a. 'a -> unit) -> unit
       is not included in
         val x : ((repr_ 'a). 'a -> unit) -> unit
       The type "('a. 'a -> unit) -> unit" is not compatible with the type
         "((repr_ 'a). 'a -> unit) -> unit"
       Type "'a -> unit" is not compatible with type "(repr_ 'a0). 'a0 -> unit"
|}]

(** [Trepr] is not an instance of [Tpoly]. *)
module Test_2 : sig
  val x : ('a. 'a -> unit) -> unit
end = struct
  let x (_ : (repr_ 'a). 'a -> unit) = ()
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x (_ : (repr_ 'a). 'a -> unit) = ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val x : ((repr_ 'a). 'a -> unit) -> unit end
       is not included in
         sig val x : ('a. 'a -> unit) -> unit end
       Values do not match:
         val x : ((repr_ 'a). 'a -> unit) -> unit
       is not included in
         val x : ('a. 'a -> unit) -> unit
       The type "((repr_ 'a). 'a -> unit) -> unit"
       is not compatible with the type "('a. 'a -> unit) -> unit"
       Type "(repr_ 'a). 'a -> unit" is not compatible with type "'a. 'a -> unit"
|}]

(** Is alpha-conversion performed? *)
module Test_3 (X : sig val f : (repr_ 'a). 'a -> unit end)
  : sig val f : (repr_ 'b). 'b -> unit end
  = X
[%%expect {|
module Test_3 :
  functor (X : sig val f : (repr_ 'a). 'a -> unit end) ->
    sig val f : (repr_ 'b). 'b -> unit end
|}]

(** Test alpha conversion for multiple variables. *)
module Test_3' (X : sig val f : (repr_ 'a) (repr_ 'b). 'a -> 'b end)
  : sig val f : (repr_ 'c) (repr_ 'd). 'c -> 'd end
  = X
[%%expect {|
module Test_3' :
  functor (X : sig val f : (repr_ 'a) (repr_ 'b). 'a -> 'b end) ->
    sig val f : (repr_ 'c) (repr_ 'd). 'c -> 'd end
|}]

(** [repr_]-bound type variable subtyping is handled reasonably. *)
module Test_4 (X : sig val f : (repr_ 'a). 'a -> unit end)
  : sig val f : (repr_ 'b). ('b -> 'b) -> unit end
  = X
[%%expect {|
Line 3, characters 4-5:
3 |   = X
        ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : (repr_ 'a). 'a -> unit end
       is not included in
         sig val f : (repr_ 'b). ('b -> 'b) -> unit end
       Values do not match:
         val f : (repr_ 'a). 'a -> unit
       is not included in
         val f : (repr_ 'b). ('b -> 'b) -> unit
       The type "(repr_ 'a). 'a -> unit" is not compatible with the type
         "(repr_ 'b). ('b -> 'b) -> unit"
       Type "'a" is not compatible with type "'b -> 'b"
|}]

(** Order of [repr_]-bound type variables matters. *)

module Test_5 (X : sig val f : (repr_ 'a) (repr_ 'b). 'a -> 'b end)
  : sig val f : (repr_ 'a) (repr_ 'b). 'b -> 'a end
  = X
[%%expect {|
Line 5, characters 4-5:
5 |   = X
        ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : (repr_ 'a) (repr_ 'b). 'a -> 'b end
       is not included in
         sig val f : (repr_ 'a) (repr_ 'b). 'b -> 'a end
       Values do not match:
         val f : (repr_ 'a) (repr_ 'b). 'a -> 'b
       is not included in
         val f : (repr_ 'a) (repr_ 'b). 'b -> 'a
       The type "(repr_ 'a) (repr_ 'b). 'a -> 'b"
       is not compatible with the type "(repr_ 'a) (repr_ 'b). 'b -> 'a"
       Type "'a" is not compatible with type "'b"
|}]

module Test_5' (X : sig val f : (repr_ 'a) (repr_ 'b). 'a -> 'b end)
  : sig val f : (repr_ 'b) (repr_ 'a). 'b -> 'a end
  = X
[%%expect {|
module Test_5' :
  functor (X : sig val f : (repr_ 'a) (repr_ 'b). 'a -> 'b end) ->
    sig val f : (repr_ 'b) (repr_ 'a). 'b -> 'a end
|}]


(** This should pass and generate a module coercion.
    Currently not accepted.
    CR layout-polymorphism aivaskovic: internal ticket 6187. *)
module Test_6 (X : sig val f : (repr_ 'a). 'a -> unit end)
  : sig val f : 'a -> unit end
  = X
[%%expect {|
Line 3, characters 4-5:
3 |   = X
        ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : (repr_ 'a). 'a -> unit end
       is not included in
         sig val f : 'a -> unit end
       Values do not match:
         val f : (repr_ 'a). 'a -> unit
       is not included in
         val f : 'a -> unit
       The type "(repr_ 'a). 'a -> unit" is not compatible with the type
         "'b -> unit"
|}]

(** Check that type-checking works end-to-end. *)

let forced : (repr_ 'a). unit =
  raise Force_type
;;
[%%expect {|
Exception: Force_type.
|}];;

let forced2 : (repr_ 'a) (repr_ 'b). unit =
  raise Force_type
;;
[%%expect {|
Exception: Force_type.
|}];;

let forced3 : (repr_ 'a) (repr_ 'b) (repr_ 'c). unit =
  raise Force_type
;;
[%%expect {|
Exception: Force_type.
|}];;
