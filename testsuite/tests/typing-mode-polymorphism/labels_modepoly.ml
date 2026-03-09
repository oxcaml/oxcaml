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
  ?x:'a @ [< 'm @@ past & global] ->
  (unit @ 'n -> unit @ [< global]) @ [< global > 'm] = <fun>
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
  (?x:int @ [< 'mm0 @@ past & 'mm1 @@ past & global many > 'p | 'mm0 | aliased nonportable] ->
   (unit @ [< 'p @@ past & 'q @@ past > aliased nonportable] ->
    int @ [< global many uncontended > 'm]) @ [< 'm @@ past & 'n @@ past & 'o @@ past & global many uncontended > 'n | nonportable]) @ [< global many > 'q | 'o | 'mm1] ->
  int @ [< global] = <fun>
|}, Principal{|
Line 1, characters 51-52:
1 | let f g = ignore (g : ?x:int -> unit -> int); g ~x:3 () ;;
                                                       ^
Warning 18 [not-principal]: using an optional argument here is not principal.

val f :
  (?x:int @ [< 'mm1 @@ past & 'mm2 @@ past & global many > 'q | 'mm1 | aliased nonportable] ->
   (unit @ [< 'q @@ past & 'mm0 @@ past > aliased nonportable] ->
    int @ [< 'p & global many uncontended > 'm]) @ [< 'm @@ past & 'n @@ past & 'o @@ past & global many uncontended > 'n | nonportable]) @ [< global many > 'mm0 | 'o | 'mm2] ->
  int @ [< global > 'p] = <fun>
|}];;

let f g = ignore (g : ?x:int -> unit -> int); g ();;
[%%expect{|
val f :
  (?x:int @ [< 'mm0 @@ past & 'mm1 @@ past & global many > 'p | 'mm0 | aliased nonportable] ->
   (unit @ [< 'p @@ past & 'q @@ past > aliased nonportable] ->
    int @ [< global many uncontended > 'm]) @ [< 'm @@ past & 'n @@ past & 'o @@ past & global many uncontended > 'n | nonportable]) @ [< global many > 'q | 'o | 'mm1] ->
  int @ [< global] = <fun>
|}, Principal{|
Line 1, characters 46-47:
1 | let f g = ignore (g : ?x:int -> unit -> int); g ();;
                                                  ^
Warning 19 [non-principal-labels]: eliminated omittable argument without principality.

val f :
  (?x:int @ [< 'mm1 @@ past & 'mm2 @@ past & global many > 'q | 'mm1 | aliased nonportable] ->
   (unit @ [< 'q @@ past & 'mm0 @@ past > aliased nonportable] ->
    int @ [< 'p & global many uncontended > 'm]) @ [< 'm @@ past & 'n @@ past & 'o @@ past & global many uncontended > 'n | nonportable]) @ [< global many > 'mm0 | 'o | 'mm2] ->
  int @ [< global > 'p] = <fun>
|}];;

let f g = ignore (g : x:int -> unit -> int); g ();;
[%%expect{|
val f :
  (x:int @ [< 'mm1 @@ past & 'mm2 @@ past & global many > 'q | 'mm1 | aliased nonportable]as 'mm3 ->
   (unit @ [< 'q @@ past & 'mm0 @@ past > aliased nonportable] ->
    int @ [< global many uncontended > 'm]as 'p) @ [< 'm @@ past & 'n @@ past & 'o @@ past & global many uncontended > 'n | nonportable]) @ [< 'mm4 @@ past & global many > 'mm0 | 'o | 'mm2] ->
  (x:int @ 'mm3 -> int @ 'p) @ [< global > 'mm4] = <fun>
|}, Principal{|
Line 1, characters 45-46:
1 | let f g = ignore (g : x:int -> unit -> int); g ();;
                                                 ^
Warning 19 [non-principal-labels]: commuted an argument without principality.

val f :
  (x:int @ [< 'mm1 @@ past & 'mm2 @@ past & global many > 'q | 'mm1 | aliased nonportable]as 'mm3 ->
   (unit @ [< 'q @@ past & 'mm0 @@ past > aliased nonportable] ->
    int @ [< global many uncontended > 'm]as 'p) @ [< 'm @@ past & 'n @@ past & 'o @@ past & global many uncontended > 'n | nonportable]) @ [< 'mm4 @@ past & global many > 'mm0 | 'o | 'mm2] ->
  (x:int @ 'mm3 -> int @ 'p) @ [< global > 'mm4] = <fun>
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
      (int @ 'n -> unit @ [< 'm @@ past & global]) @ 'o ->
      unit @ [< global > 'm @@ many portable]
  end
class virtual fail :
  object
    method trigger : setup
    method virtual with_ :
      (int @ 'n -> unit @ [< 'm @@ past & global]) @ 'o ->
      unit @ [< global > 'm @@ many portable]
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
  (x:int @ [< 'm @@ past] -> (y:int @ 'p -> int @ 'o) @ [> 'm | 'n]) @ [< 'n @@ past] ->
  int @ [< global] = <fun>
Line 4, characters 11-26:
4 | module E = (val type_of f)
               ^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

module E : sig type t = (x:int -> y:int -> int) -> int end
val g : 'a -> E.t = <fun>
|}]
