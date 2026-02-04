(* TEST
 expect;
*)

(* PR#5907 *)

type 'a t = 'a;;
let f (g : 'a list -> 'a t -> 'a) s = g s s;;
[%%expect{|
type 'a t = 'a
Line 2, characters 42-43:
2 | let f (g : 'a list -> 'a t -> 'a) s = g s s;;
                                              ^
Error: This expression has type "'a list"
       but an expression was expected of type "'a t" = "'a"
       The type variable "'a" occurs inside "'a list"
|}];;

let f (g : 'a * 'b -> 'a t -> 'a) s = g s s;;
[%%expect{|
Line 1, characters 42-43:
1 | let f (g : 'a * 'b -> 'a t -> 'a) s = g s s;;
                                              ^
Error: This expression has type "'a * 'b"
       but an expression was expected of type "'a t" = "'a"
       The type variable "'a" occurs inside "'a * 'b"
|}];;

(* #12971 *)

module Seq : sig
  type 'a t = unit -> 'a node
  and 'a node

  val empty : 'a t
  val cons : 'a -> 'a t -> 'a t
end = struct
  type 'a t = unit -> 'a node
  and 'a node = unit

  let empty () = ()
  let cons x xs () = ()
end;;
[%%expect{|
module Seq :
  sig
    type 'a t = unit -> 'a node
    and 'a node
    val empty : 'a t
    val cons : 'a -> 'a t -> 'a t
  end
|}];;

type 'a t = T of 'a;;
let wrong_to_seq (xt : 'a t) : 'a Seq.t =
  let T x = xt in
  Seq.cons Seq.empty x
;;
(* Note: the current behavior of this function is believed to be
   a bug, in the sense that it creates an equi-recursive type even in
   absence of the -rectypes flag. On the other hand, it does not fail
   with the Ctype.Escape exception, as it did from 4.13 to 5.1. *)
[%%expect{|
type 'a t = T of 'a
Line 4, characters 2-22:
4 |   Seq.cons Seq.empty x
      ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a Seq.t Seq.t" = "unit -> 'a Seq.t Seq.node"
       but an expression was expected of type
         "(unit -> 'a Seq.t Seq.node) Seq.t" =
           "unit -> (unit -> 'a Seq.t Seq.node) Seq.node"
       Type "'a Seq.t" = "unit -> 'a Seq.node" is not compatible with type
         "unit -> 'a Seq.t Seq.node"
       Type "'a" is not compatible with type "'a Seq.t" = "unit -> 'a Seq.node"
       The type variable "'a" occurs inside "'a Seq.t"
|}];;

let strange x = Seq.[cons x empty; cons empty x];;
[%%expect{|
Line 1, characters 35-47:
1 | let strange x = Seq.[cons x empty; cons empty x];;
                                       ^^^^^^^^^^^^
Error: This expression has type "'a Seq.t Seq.t" = "unit -> 'a Seq.t Seq.node"
       but an expression was expected of type
         "unit -> (unit -> 'a Seq.t Seq.node) Seq.node"
       Type "'a Seq.t" = "unit -> 'a Seq.node" is not compatible with type
         "unit -> 'a Seq.t Seq.node"
       Type "'a" is not compatible with type "'a Seq.t" = "unit -> 'a Seq.node"
       The type variable "'a" occurs inside "'a Seq.t"
|}, Principal{|
Line 1, characters 35-47:
1 | let strange x = Seq.[cons x empty; cons empty x];;
                                       ^^^^^^^^^^^^
Error: This expression has type "'a Seq.t Seq.t" = "unit -> 'a Seq.t Seq.node"
       but an expression was expected of type
         "(unit -> 'a Seq.t Seq.node) Seq.t" =
           "unit -> (unit -> 'a Seq.t Seq.node) Seq.node"
       Type "'a Seq.t" = "unit -> 'a Seq.node" is not compatible with type
         "unit -> 'a Seq.t Seq.node"
       Type "'a" is not compatible with type "'a Seq.t" = "unit -> 'a Seq.node"
       The type variable "'a" occurs inside "'a Seq.t"
|}];;

(* The occurrence check used to take an exponential amount of time on this
   case. *)

type 'a t0 = ('a * 'a)
type 'a t1 = ('a * 'a) t0
type 'a t2 = ('a * 'a) t1
type 'a t3 = ('a * 'a) t2
type 'a t4 = ('a * 'a) t3
type 'a t5 = ('a * 'a) t4
type 'a t6 = ('a * 'a) t5
type 'a t7 = ('a * 'a) t6
type 'a t8 = ('a * 'a) t7
type 'a t9 = ('a * 'a) t8
type 'a t10 = ('a * 'a) t9
type 'a t11 = ('a * 'a) t10
type 'a t12 = ('a * 'a) t11
type 'a t13 = ('a * 'a) t12
type 'a t14 = ('a * 'a) t13
type 'a t15 = ('a * 'a) t14
type 'a t16 = ('a * 'a) t15
type 'a t17 = ('a * 'a) t16
type 'a t18 = ('a * 'a) t17
type 'a t19 = ('a * 'a) t18
type 'a t20 = ('a * 'a) t19
type 'a t21 = ('a * 'a) t20
type 'a t22 = ('a * 'a) t21
type 'a t23 = ('a * 'a) t22
type 'a t24 = ('a * 'a) t23
type 'a t25 = ('a * 'a) t24
type 'a t26 = ('a * 'a) t25
type 'a t27 = ('a * 'a) t26
type 'a t28 = ('a * 'a) t27
type 'a t29 = ('a * 'a) t28
type 'a t30 = ('a * 'a) t29
type 'a t = 'a t30

let foo (t : int t) : int t = t
[%%expect{|
type 'a t0 = 'a * 'a
type 'a t1 = ('a * 'a) t0
type 'a t2 = ('a * 'a) t1
type 'a t3 = ('a * 'a) t2
type 'a t4 = ('a * 'a) t3
type 'a t5 = ('a * 'a) t4
type 'a t6 = ('a * 'a) t5
type 'a t7 = ('a * 'a) t6
type 'a t8 = ('a * 'a) t7
type 'a t9 = ('a * 'a) t8
type 'a t10 = ('a * 'a) t9
type 'a t11 = ('a * 'a) t10
type 'a t12 = ('a * 'a) t11
type 'a t13 = ('a * 'a) t12
type 'a t14 = ('a * 'a) t13
type 'a t15 = ('a * 'a) t14
type 'a t16 = ('a * 'a) t15
type 'a t17 = ('a * 'a) t16
type 'a t18 = ('a * 'a) t17
type 'a t19 = ('a * 'a) t18
type 'a t20 = ('a * 'a) t19
type 'a t21 = ('a * 'a) t20
type 'a t22 = ('a * 'a) t21
type 'a t23 = ('a * 'a) t22
type 'a t24 = ('a * 'a) t23
type 'a t25 = ('a * 'a) t24
type 'a t26 = ('a * 'a) t25
type 'a t27 = ('a * 'a) t26
type 'a t28 = ('a * 'a) t27
type 'a t29 = ('a * 'a) t28
type 'a t30 = ('a * 'a) t29
type 'a t = 'a t30
val foo : int t -> int t = <fun>
|}]
