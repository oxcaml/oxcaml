(* TEST
 flags += "-extension mode_polymorphism_alpha";
 expect;
*)

let use_uncontended (x @ uncontended) = ()
let use_portable (x @ portable) = ()
let use_unique (x @ unique) = ()
let use_static (x @ static) = ()
let use_global (x @ global) = ()
[%%expect{|
val use_uncontended : 'a @ [< uncontended] -> unit @ 'm = <fun>
val use_portable : 'a @ [< portable] -> unit @ 'm = <fun>
val use_unique : 'a @ [< unique] -> unit @ 'm = <fun>
val use_static : 'a @ 'n -> unit @ 'm = <fun>
val use_global : 'a @ [< global] -> unit @ 'm = <fun>
|}]

(* FUNCTION APPLICATION *)

(* mode polymorphism is preserved across function applications *)
let id x = x
let id' x = id x
[%%expect{|
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
val id' : 'a @ [< 'm & global] -> 'a @ [> 'm] = <fun>
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
val apply :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'o @@ past & global] ->
  ('a @ [< 'n] -> 'b @ [> 'm]) @ [> 'o] = <fun>
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
val compose :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'mm0 @@ past & 'o @@ past] ->
  (('c @ [> 'p] -> 'a @ [< 'n & global]) @ [< 'q @@ past] ->
   ('c @ [< 'p] -> 'b @ [> 'm]) @ [> 'q | 'mm0]) @ [> 'o] =
  <fun>
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
val chain : 'a @ [< 'm & global] -> 'a @ [> 'm] = <fun>
|}]

let foo (x @ unique) = use_unique (chain x)
[%%expect{|
val foo : 'a @ [< global unique] -> unit @ 'm = <fun>
|}]

let foo (x @ portable) = use_portable (chain x)
[%%expect{|
val foo : 'a @ [< global portable] -> unit @ 'm = <fun>
|}]

(* RECURSIVE FUNCTIONS *)

let rec recursive x n =
  if n <= 0 then x else recursive x (n - 1)
[%%expect{|
val recursive :
  'a @ [< 'q & 'p & 'p & global > 'p | 'p] ->
  (int @ [< 'mm2 & many uncontended > 'mm2] ->
   'a @ [< 'mm0 & 'mm0 & 'mm1 & 'mm1 & global > 'mm0 | 'mm0 | 'mm1 | 'mm1 | 'q]) @ [< 'm @@ past & 'n @@ past & 'o @@ past > close('p) | close('p) | monadic_to_comonadic_min('p) | monadic_to_comonadic_min('p) | close('p) | close('p) | monadic_to_comonadic_min('p) | monadic_to_comonadic_min('p) | close('q) | close('q) | 'm | 'n @@ many portable | 'o | nonportable] =
  <fun>
|}]

(* CR ageorges: after using the recursive function once, the bounds of
the instantiation are added to the original mode polymorphic definition *)
let foo (x @ portable) =
  let x = recursive x 10 in
  use_portable x
[%%expect{|
val foo : 'a @ [< global portable] -> unit @ 'm = <fun>
|}]

let recursive' = recursive
[%%expect{|
val recursive' :
  'a @ [< 'mm1 & 'mm1 & 'mm1 & global portable > 'mm1 | 'mm1 | 'mm1] ->
  (int @ [< 'mm0 & many uncontended > 'mm0] ->
   'a @ [< 'p & 'p & 'p & 'p & 'p & 'p & 'q & 'q & 'q & global portable > 'p | 'p | 'p | 'p | 'p | 'p | 'q | 'q | 'q]) @ [< 'm @@ past & 'n @@ past & 'o @@ past & 'n @@ past & 'o @@ past > 'm | 'n @@ many portable | 'o | 'n @@ many portable | 'o | nonportable] =
  <fun>
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
val map :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'mm0 & 'mm0 . aliased contended & 'mm0 & many > 'mm0 | 'mm0 | 'mm0 | aliased] ->
  ('a list @ [< 'mm3 & 'mm3 & 'n > 'mm3 | 'mm3] ->
   'b list @ [< 'mm1 & 'mm1 & 'mm2 & 'mm2 & global > 'mm1 | 'mm1 | 'mm2 | 'mm2 | 'm]) @ [< 'o @@ past & 'p @@ past & 'q @@ past > monadic_to_comonadic_min('mm0) | monadic_to_comonadic_min('mm0) | monadic_to_comonadic_min('mm0) | monadic_to_comonadic_min('mm0) | monadic_to_comonadic_min('mm0) | monadic_to_comonadic_min('mm0) | 'o | 'p @@ many portable | 'q | nonportable] =
  <fun>
|}]

let foo (y @ portable) =
  let g () = y in
  let l = [(); (); ()] in
  let lg = map g l in
  use_portable lg
[%%expect{|
val foo : 'a @ [< global many portable] -> unit @ 'm = <fun>
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

let foo (y @ portable) =
  let g () = y in
  let l = [(); (); ()] in
  let lg = map g l in
  use_portable lg
[%%expect{|
val foo : 'a @ [< global many portable] -> unit @ 'm = <fun>
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
val use_and_return :
  'a @ [< 'm & global many uncontended] -> 'a @ [> 'm | aliased] = <fun>
|}]

(* contended values cannot be use_and_returned due to uncontended bound *)
let foo (x @ contended) = use_and_return x
[%%expect{|
Line 1, characters 41-42:
1 | let foo (x @ contended) = use_and_return x
                                             ^
Error: This value is "contended" but is expected to be "uncontended".
|}]
