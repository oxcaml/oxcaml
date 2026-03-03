(* TEST
 flags += "-extension mode_polymorphism_alpha";
 expect;
*)

(* PR#5835 *)
let f ~x = x + 1;;
f ?x:0;;
[%%expect{|
val f : x:int @ 'm -> int @ [< global] = <fun>
Line 2, characters 5-6:
2 | f ?x:0;;
         ^
Warning 43 [nonoptional-label]: the label x is not optional.

- : int = 1
|}];;

(* PR#6352 *)
let foo (f : unit -> unit) = ();;
let g ?x () = ();;
foo ((); g);;
[%%expect{|
val foo : (unit -> unit) @ 'm -> unit @ [< global] = <fun>
val g :
  ?x:'a @ [< 'm.future & global] ->
  (unit @ 'n -> unit @ [< global]) @ [< global > 'm.future] = <fun>
- : unit = ()
|}];;

(* PR#5748 *)
foo (fun ?opt () -> ()) ;; (* fails *)
[%%expect{|
Line 1, characters 4-23:
1 | foo (fun ?opt () -> ()) ;; (* fails *)
        ^^^^^^^^^^^^^^^^^^^
Error: This function should have type "unit -> unit"
       but its first argument is labeled "?opt" instead of being unlabeled
|}];;

(* filter_arrow *)

let (f : x:int -> int) = fun y -> y
[%%expect{|
Line 1, characters 25-35:
1 | let (f : x:int -> int) = fun y -> y
                             ^^^^^^^^^^
Error: This function should have type "x:int -> int"
       but its first argument is unlabeled instead of being labeled "~x"
|}];;

let (f : int -> int) = fun ~y -> y
[%%expect{|
Line 1, characters 23-34:
1 | let (f : int -> int) = fun ~y -> y
                           ^^^^^^^^^^^
Error: This function should have type "int -> int"
       but its first argument is labeled "~y" instead of being unlabeled
|}];;

let (f : x:int -> int) = fun ~y -> y
[%%expect{|
Line 1, characters 25-36:
1 | let (f : x:int -> int) = fun ~y -> y
                             ^^^^^^^^^^^
Error: This function should have type "x:int -> int"
       but its first argument is labeled "~y" instead of "~x"
|}];;

(* More examples *)

let f g = ignore (g ?x:(Some 2) ()); g ~x:3 () ;;
[%%expect{|
Line 1, characters 37-38:
1 | let f g = ignore (g ?x:(Some 2) ()); g ~x:3 () ;;
                                         ^
Error: This function is applied to arguments
       in an order different from other calls.
       This is only allowed when the real type is known.
|}];;

let f g = let _ = g ?x:(Some 2) () in g ~x:3 () ;;
[%%expect{|
Line 1, characters 38-39:
1 | let f g = let _ = g ?x:(Some 2) () in g ~x:3 () ;;
                                          ^
Error: This function is applied to arguments
       in an order different from other calls.
       This is only allowed when the real type is known.
|}];;

(* principality warning *)
let f g = ignore (g : ?x:int -> unit -> int); g ~x:3 () ;;
[%%expect{|
val f :
  (?x:int @ [< 'mm0.future & 'mm1.future & global many > 'p.future | 'mm0.future | aliased nonportable] ->
   (unit @ [< 'p.future & 'q.future > aliased nonportable] ->
    int @ [< global many uncontended > 'm.future]) @ [< 'm.future & 'n.future & 'o.future & global many uncontended > 'n.future | nonportable]) @ [< global many > 'q.future | 'o.future | 'mm1.future] ->
  int @ [< global] = <fun>
|}, Principal{|
Line 1, characters 51-52:
1 | let f g = ignore (g : ?x:int -> unit -> int); g ~x:3 () ;;
                                                       ^
Warning 18 [not-principal]: using an optional argument here is not principal.

val f :
  (?x:int @ [< 'mm1.future & 'mm2.future & global many > 'q.future | 'mm1.future | aliased nonportable] ->
   (unit @ [< 'q.future & 'mm0.future > aliased nonportable] ->
    int @ [< 'p & global many uncontended > 'm.future]) @ [< 'm.future & 'n.future & 'o.future & global many uncontended > 'n.future | nonportable]) @ [< global many > 'mm0.future | 'o.future | 'mm2.future] ->
  int @ [< global > 'p] = <fun>
|}];;

let f g = ignore (g : ?x:int -> unit -> int); g ();;
[%%expect{|
val f :
  (?x:int @ [< 'mm0.future & 'mm1.future & global many > 'p.future | 'mm0.future | aliased nonportable] ->
   (unit @ [< 'p.future & 'q.future > aliased nonportable] ->
    int @ [< global many uncontended > 'm.future]) @ [< 'm.future & 'n.future & 'o.future & global many uncontended > 'n.future | nonportable]) @ [< global many > 'q.future | 'o.future | 'mm1.future] ->
  int @ [< global] = <fun>
|}, Principal{|
Line 1, characters 46-47:
1 | let f g = ignore (g : ?x:int -> unit -> int); g ();;
                                                  ^
Warning 19 [non-principal-labels]: eliminated omittable argument without principality.

val f :
  (?x:int @ [< 'mm1.future & 'mm2.future & global many > 'q.future | 'mm1.future | aliased nonportable] ->
   (unit @ [< 'q.future & 'mm0.future > aliased nonportable] ->
    int @ [< 'p & global many uncontended > 'm.future]) @ [< 'm.future & 'n.future & 'o.future & global many uncontended > 'n.future | nonportable]) @ [< global many > 'mm0.future | 'o.future | 'mm2.future] ->
  int @ [< global > 'p] = <fun>
|}];;

let f g = ignore (g : x:int -> unit -> int); g ();;
[%%expect{|
val f :
  (x:int @ [< 'mm2.future & 'mm3.future & global many > 'q.future | 'mm2.future | aliased nonportable]as 'mm4 ->
   (unit @ [< 'q.future & 'mm0.future > 'mm1 | aliased nonportable] ->
    int @ [< global many uncontended > 'm.future]as 'p) @ [< 'm.future & 'n.future & 'o.future & global many uncontended > 'n.future | nonportable]) @ [< 'mm6.future & global many > 'mm0.future | 'o.future | 'mm3.future] ->
  (x:int @ 'mm4 -> int @ 'p) @ [< global > 'mm5 | close('mm1) | close('mm5) | 'mm6.future] =
  <fun>
|}, Principal{|
Line 1, characters 45-46:
1 | let f g = ignore (g : x:int -> unit -> int); g ();;
                                                 ^
Warning 19 [non-principal-labels]: commuted an argument without principality.

val f :
  (x:int @ [< 'mm2.future & 'mm3.future & global many > 'q.future | 'mm2.future | aliased nonportable]as 'mm4 ->
   (unit @ [< 'q.future & 'mm0.future > 'mm1 | aliased nonportable] ->
    int @ [< global many uncontended > 'm.future]as 'p) @ [< 'm.future & 'n.future & 'o.future & global many uncontended > 'n.future | nonportable]) @ [< 'mm6.future & global many > 'mm0.future | 'o.future | 'mm3.future] ->
  (x:int @ 'mm4 -> int @ 'p) @ [< global > 'mm5 | close('mm1) | close('mm5) | 'mm6.future] =
  <fun>
|}];;

(* 9859: inferred function types may appear in the right hand side of :> *)
class setup = object
  method with_ f = (f 0:unit)
end
class virtual fail = object (self)
  method trigger = (self :> setup )
end
[%%expect {|
class setup :
  object
    method with_ :
      (int @ 'n -> unit @ [< 'm.future & global]) @ 'o ->
      unit @ [< global > 'm.future mod many portable]
  end
class virtual fail :
  object
    method trigger : setup
    method virtual with_ :
      (int @ 'n -> unit @ [< 'm.future & global]) @ 'o ->
      unit @ [< global > 'm.future mod many portable]
  end
|}]

module type T = sig type t  end
let type_of (type x) (x: x) = (module struct type t = x end: T with type t = x)
let f g = 1 + g ~x:0 ~y:0;;
module E = (val type_of f)
let g = ( (fun _ -> f) :> 'a -> E.t)
[%%expect {|
module type T = sig type t end
val type_of : 'x @ 'm -> (module T with type t = 'x) @ [< global] = <fun>
val f :
  (x:int @ [< 'm.future > 'n] ->
   (y:int @ 'q -> int @ 'p) @ [> 'm.future | monadic_to_comonadic_min('n) | 'o.future]) @ [< 'o.future] ->
  int @ [< global] = <fun>
Line 4, characters 11-26:
4 | module E = (val type_of f)
               ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

module E : sig type t = (x:int -> y:int -> int) -> int end
val g : 'a -> E.t = <fun>
|}]
