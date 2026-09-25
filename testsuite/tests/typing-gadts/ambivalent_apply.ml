(* TEST
 expect;
*)

type (_,_) eq = Refl : ('a,'a) eq;;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
|}]

(* Both should fail *)
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) =
   let Refl = w1 in let Refl = w2 in g 3;;
[%%expect{|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> 'b = <fun>
|}, Principal{|
Line 2, characters 37-40:
2 |    let Refl = w1 in let Refl = w2 in g 3;;
                                         ^^^
Error: This expression has type "b" = "int"
       but an expression was expected of type "'a"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
|}]
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) =
   let Refl = w2 in let Refl = w1 in g 3;;
[%%expect{|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> int = <fun>
|}, Principal{|
Line 2, characters 37-40:
2 |    let Refl = w2 in let Refl = w1 in g 3;;
                                         ^^^
Error: This expression has type "int" but an expression was expected of type "'a"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
|}]

(* Ok *)
let f (type a b) (w1 : (a, b -> b) eq) (w2 : (a, int -> int) eq) (g : a) : b =
   let Refl = w2 in let Refl = w1 in g 3;;
[%%expect{|
val f : ('a, 'b -> 'b) eq -> ('a, int -> int) eq -> 'a -> 'b = <fun>
|}]

(* Inspecting a function's return type for application warnings must not
   make its result depend on an unused GADT equation. *)
let direct (type a b) (w : (a, b) eq) (f : unit -> b) =
  let Refl = w in
  f ();;
[%%expect{|
val direct : ('a, 'b) eq -> (unit -> 'b) -> 'b = <fun>
|}]

let labelled (type a b) (w : (a, b) eq) (f : x:unit -> b) =
  let Refl = w in
  f ~x:();;
[%%expect{|
val labelled : ('a, 'b) eq -> (x:unit -> 'b) -> 'b = <fun>
|}]

let partial (type a b) (w : (a, b) eq) (f : unit -> unit -> b) =
  let Refl = w in
  f ();;
[%%expect{|
val partial : ('a, 'b) eq -> (unit -> unit -> 'b) -> unit -> 'b = <fun>
|}]

(* The inspection must still share and lower type variables. *)
let shared_result (type a b) (w : (a, b) eq) f x =
  let Refl = w in
  let (_ : int) = f x in
  f x;;
[%%expect{|
val shared_result : ('a, 'b) eq -> ('c -> int) -> 'c -> int = <fun>
|}]

let nonreturning (type a b) (w : (a, b) eq) =
  let Refl = w in
  (raise Exit) 3;;
[%%expect{|
Line 3, characters 15-16:
3 |   (raise Exit) 3;;
                   ^
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.

val nonreturning : ('a, 'b) eq -> 'c = <fun>
|}]
