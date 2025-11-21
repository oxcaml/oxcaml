(* TEST
 expect;
*)

let x @ static = 42
[%%expect{|
val x : int = 42
|}]

let f (b @ dynamic) @ static =
    if b then "hello"
    else "world"
[%%expect{|
Line 2, characters 7-8:
2 |     if b then "hello"
           ^
Error: This value is "dynamic" but is expected to be "static"
       because it controls the expression at Lines 2-3, characters 4-16
       which is expected to be "static".
|}]

let (f @ static) b =
    if b then "hello"
    else "world"
[%%expect{|
val f : bool -> string = <fun>
|}]

let x @ static = f true
[%%expect{|
Line 1, characters 17-23:
1 | let x @ static = f true
                     ^^^^^^
Error: The expression is "dynamic"
       because function applications are always dynamic.
       However, the expression highlighted is expected to be "static".
|}]

(* Testing pattern match *)

(** Single branch is static, even if not exhaustive (staticity doesn't track
    exceptions) *)
let foo (b @ dynamic) @ static =
    match[@warning "-8"] b with
    | 42 -> "hello"
[%%expect{|
val foo : int -> string = <fun>
|}]

(* more than one branch - we require the matched value to be static as well *)
let foo (b @ dynamic) @ static =
    match[@warning "-8"] b with
    | 42 -> "hello"
    | 24 -> "hello"
[%%expect{|
Line 3, characters 6-8:
3 |     | 42 -> "hello"
          ^^
Error: The pattern is "dynamic" but is expected to be "static"
       because it controls the expression at Lines 2-4, characters 4-19
       which is expected to be "static".
|}]

(* this should be allowed, but ocaml unfold this to a chain of [Or] patterns,
   which introduces branching (even though all the branches leads to the same
   body) *)
let foo (b @ dynamic) @ static =
    match[@warning "-8"] b with
    | 'a' .. 'z' -> "hello"
[%%expect{|
Line 3, characters 6-16:
3 |     | 'a' .. 'z' -> "hello"
          ^^^^^^^^^^
Error: The pattern is "dynamic" but is expected to be "static"
       because it controls the expression at Lines 2-3, characters 4-27
       which is expected to be "static".
|}]

type t = Foo of bool

type u = Bar of bool | Baz of bool

let foo (b @ dynamic) @ static =
    match b with
    | Foo x -> "hello"
[%%expect{|
type t = Foo of bool
type u = Bar of bool | Baz of bool
val foo : t -> string = <fun>
|}]

let foo (b @ dynamic) @ static =
    match[@warning "-8"] b with
    | Bar _ -> "hello"
[%%expect{|
val foo : u -> string = <fun>
|}]

let foo (b @ dynamic) @ static =
    match[@warning "-8"] b with
    | Bar _ -> "hello"
    | Baz _ -> "hello"
[%%expect{|
Line 3, characters 6-11:
3 |     | Bar _ -> "hello"
          ^^^^^
Error: The pattern is "dynamic" but is expected to be "static"
       because it controls the expression at Lines 2-4, characters 4-22
       which is expected to be "static".
|}]

(* Or pattern is considered branching, because they introduces different bindings *)
let foo (b @ dynamic) @ static =
    match[@warning "-8"] b with
    | (Bar x | Baz x) -> x
[%%expect{|
Line 3, characters 7-12:
3 |     | (Bar x | Baz x) -> x
           ^^^^^
Error: The pattern is "dynamic" but is expected to be "static"
       because it controls the expression at Lines 2-3, characters 4-26
       which is expected to be "static".
|}]

let foo (b @ dynamic) @ static =
    match[@warning "-8"] b with
    | Foo true -> "hello"
[%%expect{|
val foo : t -> string = <fun>
|}]

let foo (b @ dynamic) @ static =
    match b with
    | `Foo -> "hello"
[%%expect{|
val foo : [< `Foo ] -> string = <fun>
|}]

let foo (b @ dynamic) @ static =
    match[@warning "-8"] b with
    | [| |] -> "hello"
[%%expect{|
val foo : ('a : value_or_null mod separable). 'a array -> string = <fun>
|}]

(* Testing exceptions *)

(* mixing exception pattern and regular patterns are not good, even if [b] is static. *)
let foo (b @ static) @ static =
    match b with
    | x -> "hello"
    | exception _ -> "world"
[%%expect{|
Line 4, characters 6-17:
4 |     | exception _ -> "world"
          ^^^^^^^^^^^
Error: The pattern is "dynamic"
       because exception patterns are always dynamic.
       However, the pattern highlighted is expected to be "static"
       because it controls the expression at Lines 2-4, characters 4-28
       which is expected to be "static".
|}]

let foo (b @ static) @ static =
    match b with
    | true -> "hello"
    | false -> "world"
[%%expect{|
val foo : bool -> string = <fun>
|}]

let foo (b @ dynamic) @ static =
    match b with
    | true | false -> "hello"
[%%expect{|
Line 3, characters 6-10:
3 |     | true | false -> "hello"
          ^^^^
Error: The pattern is "dynamic" but is expected to be "static"
       because it controls the expression at Lines 2-3, characters 4-29
       which is expected to be "static".
|}]

let foo (b @ dynamic) @ static =
    match b with
    | b' -> "hello"
[%%expect{|
val foo : 'a -> string = <fun>
|}]

let foo (b @ dynamic) @ static =
    match b with
    | _ -> "hello"
[%%expect{|
val foo : 'a -> string = <fun>
|}]

let foo (b : unit @ dynamic) @ static =
    match b with
    | () -> "hello"
[%%expect{|
val foo : unit -> string = <fun>
|}]

let foo (b @ dynamic) @ static =
    match b with
    | (a, b) -> "hello"
[%%expect{|
val foo : 'a * 'b -> string = <fun>
|}]

(* TESTING dynamic modality *)
type dynamic = Dynamic of bool @@ dynamic * int

let foo (b : dynamic @ static) @ static =
    match b with
    | Dynamic (_, 42) -> "hello"
    | _ -> "world"
[%%expect{|
type dynamic = Dynamic of bool @@ dynamic * int
val foo : dynamic -> string = <fun>
|}]

let foo (b : dynamic @ static) @ static =
    match b with
    | Dynamic (true, _) -> "hello"
    | _ -> "world"
[%%expect{|
Line 3, characters 15-19:
3 |     | Dynamic (true, _) -> "hello"
                   ^^^^
Error: The pattern is "dynamic" but is expected to be "static"
       because it controls the expression at Lines 2-4, characters 4-18
       which is expected to be "static".
|}]

(* When we swap the two cases, it's accepted! *)
let foo (b : dynamic @ static) @ static =
    match[@warning "-11"] b with
    | _ -> "world"
    | Dynamic (true, _) -> "hello"
[%%expect{|
val foo : dynamic -> string = <fun>
|}]

(* TESTING patterns as function parameters *)
let[@warning "-8"] foo : _ @ dynamic -> _ @ static = fun (Foo true) -> "hello"
[%%expect{|
val foo : t -> string = <fun>
|}]

let[@warning "-8"] foo : _ @ dynamic -> _ @ static = fun (Bar x | Baz x ) -> x
[%%expect{|
Line 1, characters 58-63:
1 | let[@warning "-8"] foo : _ @ dynamic -> _ @ static = fun (Bar x | Baz x ) -> x
                                                              ^^^^^
Error: The pattern is "dynamic" but is expected to be "static"
       because it controls the return of the function at Line 1, characters 53-78
       which is expected to be "static".
|}]


let foo : _ @ dynamic -> _ @ static = fun (Foo x) -> "hello"
[%%expect{|
val foo : t -> string = <fun>
|}]

let[@warning "-8"] foo : _ @ dynamic -> _ @ static = function
    | Foo true -> "hello"
[%%expect{|
val foo : t -> string = <fun>
|}]

let foo : _ @ dynamic -> _ @ static = function
    | Foo x -> "hello"
[%%expect{|
val foo : t -> string = <fun>
|}]

let foo : _ @ dynamic -> _ @ static = function
    | Bar x | Baz x -> x
[%%expect{|
Line 2, characters 6-11:
2 |     | Bar x | Baz x -> x
          ^^^^^
Error: The pattern is "dynamic" but is expected to be "static"
       because it controls the return of the function at Lines 1-2, characters 38-24
       which is expected to be "static".
|}]

(* TESTING let *)

let foo (b : t @ dynamic) @ static =
    let[@warning "-8"] (Foo true) = b in
    "hello"
[%%expect{|
val foo : t -> string = <fun>
|}]


let foo (b : t @ dynamic) @ static =
    let[@warning "-8"] (Foo x) = b in
    "hello"
[%%expect{|
Line 2, characters 28-29:
2 |     let[@warning "-8"] (Foo x) = b in
                                ^
Warning 26 [unused-var]: unused variable x.

val foo : t -> string = <fun>
|}]


let foo (b  @ dynamic) @ static =
    let[@warning "-8"] (Bar x | Baz x) = b in
    "hello"
[%%expect{|
Line 2, characters 41-42:
2 |     let[@warning "-8"] (Bar x | Baz x) = b in
                                             ^
Error: This value is "dynamic" but is expected to be "static"
       because it controls the expression at Lines 2-3, characters 4-11
       which is expected to be "static".
|}]


let foo (b : t @ dynamic) @ static =
    try b with e -> "hello"
[%%expect{|
Line 2, characters 4-27:
2 |     try b with e -> "hello"
        ^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression is "dynamic"
       because try clauses are always dynamic.
       However, the expression highlighted is expected to be "static".
|}]


let foo (b : t @ dynamic) @ static =
    let (Foo x) = b in
    "hello"
[%%expect{|
val foo : t -> string = <fun>
|}]

(* TESTING modules *)

(* structure can always be defined to be static, even if containing dynamic things *)
module (M @ static) = struct
    let x = Random.bool ()
end
[%%expect{|
module M : sig val x : bool @@ dynamic end
|}]

let x = Random.bool ()

(* functor can always be defined to be static, even if close over dynamic things *)
module (F @ static) (M : sig end) = struct
    let x = x
end
[%%expect{|
val x : bool = true
module F : functor (M : sig end) -> sig val x : bool end
|}]

module (M @ static) = struct
    let[@warning "-8"] (Foo true) = Foo (Random.bool ())
end
[%%expect{|
module M : sig end
|}]

(* [or] pattern potentially causes different bindings *)
module (M @ static) = struct
    let[@warning "-8"] (Foo (true | false)) = Foo (Random.bool ())
end
[%%expect{|
Line 2, characters 50-66:
2 |     let[@warning "-8"] (Foo (true | false)) = Foo (Random.bool ())
                                                      ^^^^^^^^^^^^^^^^
Error: The expression is "dynamic"
       because function applications are always dynamic.
       However, the expression highlighted is expected to be "static"
       because it is contained (via constructor "Foo") in the value at Line 2, characters 46-66
       which is expected to be "static".
|}]


module (M @ static) = struct
    let (Foo x) = Foo (Random.bool ())
end
[%%expect{|
module M : sig val x : bool @@ dynamic end
|}]

(* the following M is static, because we statically know its construction raises
   exception. *)
module (M @ static) = struct
    let[@warning "-8"] (Foo true) = Foo false
end
[%%expect{|
Exception: Match_failure ("", 2, 23).
|}]

module M = struct
    let[@warning "-8"] (Foo true) = Foo (Random.bool ())
end
[%%expect{|
Exception: Match_failure ("", 2, 23).
|}]
