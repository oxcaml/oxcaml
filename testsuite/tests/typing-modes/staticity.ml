(* TEST
 expect;
*)

let use_static (x @ static) = ()
[%%expect{|
val use_static : 'a -> unit = <fun>
|}]

let x @ static = 42
[%%expect{|
val x : int = 42
|}]

let f (b @ dynamic) @ static =
    if b then "hello"
    else "world"
[%%expect{|
Lines 2-3, characters 4-16:
2 | ....if b then "hello"
3 |     else "world"
Error: The expression is "dynamic"
       because it has branches.
       However, the expression highlighted is expected to be "static".
|}]

let f (b @ static) @ static =
    if b then "hello"
    else "world"
[%%expect{|
Lines 2-3, characters 4-16:
2 | ....if b then "hello"
3 |     else "world"
Error: The expression is "dynamic"
       because it has branches.
       However, the expression highlighted is expected to be "static".
|}]

let f (b @ dynamic) @ dynamic =
    if b then "hello"
    else "world"
[%%expect{|
val f : bool -> string = <fun>
|}]


(* CR-someday zqian: make it finer grained once slambda support static branching *)
let (f @ static) b =
    if b then "hello"
    else "world"
[%%expect{|
val f : bool -> string = <fun>
|}]

(* Applications are always dynamic *)
(* CR-someday zqian: make it finer grained once slambda support generic static
   application. *)
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

(** single branch is static, but the value matched stays at its mode *)
let foo (b @ dynamic) =
    match[@warning "-8"] b with
    | Some x -> use_static x
[%%expect{|
Line 3, characters 27-28:
3 |     | Some x -> use_static x
                               ^
Error: This value is "dynamic"
       because it is contained (via constructor "Some") in the value at Line 3, characters 6-12
       which is "dynamic".
       However, the highlighted expression is expected to be "static".
|}]

(* refuted case is not counted *)
let foo (b @ dynamic) =
    match[@warning "-8"] b with
    | () -> "hello"
    | _ -> .
[%%expect{|
val foo : unit -> string = <fun>
|}]

(* more than one branch - the result becomes dynamic. We don't support static
    branching, so the matched value being static doesn't help. *)
(* CR-someday zqian: make it finer grained once slambda support static branching *)
let foo (b @ static) @ static =
    match b with
    | true -> "hello"
    | false -> "hello"
[%%expect{|
Lines 2-4, characters 4-22:
2 | ....match b with
3 |     | true -> "hello"
4 |     | false -> "hello"
Error: The result of the cases is "dynamic"
       because it has branches.
       However, the result of the cases highlighted is expected to be "static".
|}]

(* Similiarly, the matched value becomes dynamic *)
let foo (b @ static) =
    match b with
    | Some x -> use_static x
    | None -> ()
[%%expect{|
Line 3, characters 27-28:
3 |     | Some x -> use_static x
                               ^
Error: This value is "dynamic"
       because it is contained (via constructor "Some") in the value at Line 3, characters 6-12
       which is "dynamic"
       because it has branches.
       However, the highlighted expression is expected to be "static".
|}]

let foo (b @ dynamic) @ static =
    match[@warning "-8"] b with
    | 'a' .. 'z' -> "hello"
[%%expect{|
val foo : char -> string = <fun>
|}]

type t = Foo of bool

type u = Bar of bool | Baz of bool
[%%expect{|
type t = Foo of bool
type u = Bar of bool | Baz of bool
|}]

(* Or pattern is considered branching, because they introduces different bindings *)
let foo (b @ static) =
    match b with
    | (Bar x | Baz x) -> use_static x
[%%expect{|
Line 3, characters 36-37:
3 |     | (Bar x | Baz x) -> use_static x
                                        ^
Error: This value is "dynamic"
       because it is contained (via constructor "Baz") in the value at Line 3, characters 15-20
       which is "dynamic"
       because it has branches.
       However, the highlighted expression is expected to be "static".
|}]

(* or pattern makes the bound value dynamic, but the branch can still return static *)
let foo (b @ static) @ static =
    match b with
    | (Bar x | Baz x) -> fun () -> x = true
[%%expect{|
val foo : u -> (unit -> bool) = <fun>
|}]

let foo (b @ dynamic) @ static =
    match b with
    | true | false -> "hello"
[%%expect{|
val foo : bool -> string = <fun>
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
val foo : ('a : value maybe_null). 'a array -> string = <fun>
|}]

(* Testing exceptions *)

(* mixing exception pattern and regular patterns are not good, even if [b] is static. *)
let foo (b @ static) @ static =
    match b with
    | x -> "hello"
    | exception _ -> "world"
[%%expect{|
Lines 2-4, characters 4-28:
2 | ....match b with
3 |     | x -> "hello"
4 |     | exception _ -> "world"
Error: The result of the cases is "dynamic"
       because it has branches.
       However, the result of the cases highlighted is expected to be "static".
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

(* CR-someday zqian: test dynamic modality once we care about the staticity
   of matched value. *)

(* TESTING patterns as function parameters *)
let foo : _ @ dynamic -> _ @ static = fun (Foo _) -> "hello"
[%%expect{|
val foo : t -> string = <fun>
|}]

let foo : _ @ dynamic -> _ @ static = fun (Foo x) -> x
[%%expect{|
Line 1, characters 53-54:
1 | let foo : _ @ dynamic -> _ @ static = fun (Foo x) -> x
                                                         ^
Error: This value is "dynamic"
       because it is contained (via constructor "Foo") in the value at Line 1, characters 42-49
       which is "dynamic".
       However, the highlighted expression is expected to be "static".
|}]

let foo : _ @ static -> _ @ static = fun (Bar x | Baz x ) -> x
[%%expect{|
Line 1, characters 61-62:
1 | let foo : _ @ static -> _ @ static = fun (Bar x | Baz x ) -> x
                                                                 ^
Error: This value is "dynamic"
       because it is contained (via constructor "Baz") in the value at Line 1, characters 50-55
       which is "dynamic"
       because it has branches.
       However, the highlighted expression is expected to be "static".
|}]

let foo : _ @ dynamic -> _ @ static = fun (Bar x | Baz x ) -> "hello"
[%%expect{|
val foo : u -> string = <fun>
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
Line 2, characters 23-24:
2 |     | Bar x | Baz x -> x
                           ^
Error: This value is "dynamic"
       because it is contained (via constructor "Baz") in the value at Line 2, characters 14-19
       which is "dynamic".
       However, the highlighted expression is expected to be "static".
|}]

(* TESTING let *)

let foo (b : t @ dynamic) @ static =
    let[@warning "-8"] (Foo true) = b in
    "hello"
[%%expect{|
val foo : t -> string = <fun>
|}]


let foo (b : t @ dynamic) @ static =
    let[@warning "-8"] (Foo _x) = b in
    "hello"
[%%expect{|
val foo : t -> string = <fun>
|}]


let foo (b  @ dynamic) @ static =
    let (Bar _x | Baz _x) = b in
    "hello"
[%%expect{|
val foo : u -> string = <fun>
|}]

let foo (b  @ static) =
    let (Bar x | Baz x) = b in
    use_static x
[%%expect{|
Line 3, characters 15-16:
3 |     use_static x
                   ^
Error: This value is "dynamic"
       because it is contained (via constructor "Baz") in the value at Line 2, characters 17-22
       which is "dynamic"
       because it has branches.
       However, the highlighted expression is expected to be "static".
|}]

let foo (b : t @ dynamic) @ static =
    try b with e -> Foo true
[%%expect{|
Line 2, characters 4-28:
2 |     try b with e -> Foo true
        ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression is "dynamic"
       because try-with clauses are always dynamic.
       However, the expression highlighted is expected to be "static".
|}]

let foo (b : t @ static) @ static =
    try b with e -> Foo true
[%%expect{|
Line 2, characters 4-28:
2 |     try b with e -> Foo true
        ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression is "dynamic"
       because try-with clauses are always dynamic.
       However, the expression highlighted is expected to be "static".
|}]

let foo (b : t @ static) @ dynamic =
    try b with e -> Foo true
[%%expect{|
val foo : t -> t = <fun>
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
    let (Foo x) = Foo (Random.bool ())
end
[%%expect{|
module M : sig val x : bool @@ dynamic end
|}]

(* or pattern makes [x] dynamic, even though the RHS is static. *)
module (M @ static) = struct
    let (Bar x | Baz x) @ static = Bar true
end
[%%expect{|
module M : sig val x : bool @@ dynamic end
|}]

module (M @ static) = struct
    let[@warning "-8"] (Baz x) = Bar false

    let () = use_static x
end
[%%expect{|
Exception: Match_failure ("", 2, 23).
|}]

(* functor application are always dynamic. *)
(* CR-soon zqian: remove this restriction *)
module F (X : sig end) = struct
end
module (Y @ static) = F(struct end)
[%%expect{|
module F : functor (X : sig end) -> sig end
Line 3, characters 22-35:
3 | module (Y @ static) = F(struct end)
                          ^^^^^^^^^^^^^
Error: This is "dynamic", but expected to be "static".
|}]

(* persistent modules are currently dynamic. *)
(* CR-soon zqian: remove this restriction *)
module (L @ static) = List
[%%expect{|
Line 1, characters 22-26:
1 | module (L @ static) = List
                          ^^^^
Error: This is "dynamic", but expected to be "static".
|}]

(* primitives are always static, unless you override *)
external reraise : exn -> 'a @@ portable = "%reraise"
let _ @ static = reraise
[%%expect{|
external reraise : exn -> 'a = "%reraise"
- : exn -> 'a = <fun>
|}]

external reraise : exn -> 'a @@ portable dynamic = "%reraise"
let _ @ static = reraise
[%%expect{|
external reraise : exn -> 'a = "%reraise"
Line 2, characters 17-24:
2 | let _ @ static = reraise
                     ^^^^^^^
Error: This value is "dynamic" but is expected to be "static".
|}]
