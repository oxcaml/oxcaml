(* TEST
 flags += "-extension mode_polymorphism_alpha";
 expect;
*)

(*
 * This file tests that mode polymorphism works, without printing mode variables.
 * The modes printed are not always representative of the underlying modes: they have
 * been zapped in order to be printed
*)

let use_uncontended (x @ uncontended) = ()
let use_portable (x @ portable) = ()
let use_unique (x @ unique) = ()
let use_static (x @ static) = ()
let use_global (x @ global) = ()
[%%expect{|
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_static : 'a -> unit = <fun>
val use_global : 'a -> unit = <fun>
|}]

(* FUNCTION APPLICATION *)

(* mode polymorphism is preserved across function applications *)
let id x = x
let id' x = id x
[%%expect{|
val id : 'a -> 'a = <fun>
val id' : 'a -> 'a = <fun>
|}]

let foo (x @ portable) (y @ nonportable) =
  let x = id' x in
  let y = id' y in
  use_portable x;
  use_portable y
[%%expect{|
Line 5, characters 15-16:
5 |   use_portable y
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* [id'] imposes a global bound on its argument *)
let foo (x @ local) =
  let y = id' x in ()
[%%expect{|
Line 2, characters 14-15:
2 |   let y = id' x in ()
                  ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(* HIGHER-ORDER FUNCTIONS *)

let foo (x @ portable) (y @ nonportable) =
  let x = (fun x -> x) x in
  let y = (fun y -> y) y in
  use_portable x;
  use_portable y
[%%expect{|
Line 5, characters 15-16:
5 |   use_portable y
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* higher-order application should propagate mode constraints *)
let apply f = fun x -> f x
[%%expect{|
val apply : ('a -> 'b) -> 'a -> 'b = <fun>
|}]

let foo (x @ unique) (y @ aliased) =
  let x = apply id x in
  let y = apply id y in
  use_unique x;
  use_unique y
[%%expect{|
Line 5, characters 13-14:
5 |   use_unique y
                 ^
Error: This value is "aliased" but is expected to be "unique".
|}]

let foo (x @ portable) (y @ nonportable) =
  let id x = x in
  let x = apply id x in
  let y = apply id y in
  use_portable x;
  use_portable y
[%%expect{|
Line 6, characters 15-16:
6 |   use_portable y
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* COMPOSITION *)

let compose f g x = f (g x)
[%%expect{|
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>
|}]

(* mode polymorphism propagates through composition *)

let foo (x @ portable) (y @ nonportable) =
  let x = compose id id x in
  let y = compose id id y in
  use_portable x;
  use_portable y
[%%expect{|
Line 5, characters 15-16:
5 |   use_portable y
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* CHAINING APPLICATIONS *)

(* mode constraints propagate through a chain of polymorphic applications *)
let chain x =
  let y = id x in
  let z = id y in
  z
[%%expect{|
val chain : 'a -> 'a = <fun>
|}]

let foo (x @ unique) = use_unique (chain x)
[%%expect{|
val foo : 'a @ unique -> unit = <fun>
|}]

let foo (x @ portable) = use_portable (chain x)
[%%expect{|
val foo : 'a @ portable -> unit = <fun>
|}]

(* RECURSIVE FUNCTIONS *)

let rec recursive x n =
  if n <= 0 then x else recursive x (n - 1)
[%%expect{|
val recursive : 'a -> int -> 'a = <fun>
|}]

(* CR ageorges: after using the recursive function once, the bounds of
the instantiation are added to the original mode polymorphic definition *)
let foo (x @ portable) =
  let x = recursive x 10 in
  use_portable x
[%%expect{|
val foo : 'a @ portable -> unit = <fun>
|}]

let recursive' = recursive
[%%expect{|
val recursive' : 'a @ portable -> int -> 'a @ portable = <fun>
|}]

let foo (x @ nonportable) =
  let x = recursive x 10 in
  use_portable x
[%%expect{|
Line 2, characters 20-21:
2 |   let x = recursive x 10 in
                        ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs
[%%expect{|
val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
|}]

let foo (y @ portable) =
  let g () = y in
  let l = [(); (); ()] in
  let lg = map g l in
  use_portable lg
[%%expect{|
val foo : 'a @ portable -> unit = <fun>
|}]

let foo (y @ nonportable) =
  let g () = y in
  let l = [(); (); ()] in
  let lg = map g l in
  use_portable lg
[%%expect{|
Line 4, characters 15-16:
4 |   let lg = map g l in
                   ^
Error: This expression has type "unit -> 'a"
       but an expression was expected of type "'b -> 'c @ portable"
       Hint: Did you forget to provide "()" as argument?
|}]

(* SEQUENCING *)

(* using a value and returning it *)
let use_and_return x = ignore x; x
[%%expect{|
val use_and_return : 'a -> 'a = <fun>
|}]

(* contended values cannot be use_and_returned due to uncontended bound *)
let foo (x @ contended) = use_and_return x
[%%expect{|
Line 1, characters 41-42:
1 | let foo (x @ contended) = use_and_return x
                                             ^
Error: This value is "contended" but is expected to be "uncontended".
|}]
