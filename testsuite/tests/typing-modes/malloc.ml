(* TEST
 expect;
*)


let foo () = malloc_ (1,2)
[%%expect {|
val foo : unit -> (int * int) mallocd = <fun>
|}]

let foo () = malloc_ (1,"Asdf")
[%%expect {|
val foo : unit -> (int * string) mallocd = <fun>
|}]

let foo (x @ external_) = malloc_ (x,1)
[%%expect {|
val foo : 'a -> ('a * int) mallocd = <fun>
|}]

let asdf (x @ internal) = malloc_ (x,2)
[%%expect {|
Line 1, characters 35-36:
1 | let asdf (x @ internal) = malloc_ (x,2)
                                       ^
Error: This value is "internal" but expected to be "external_".
|}]

type t = {x : int; y : int}
let f () = malloc_ {x = 3; y = 4}
[%%expect{|
type t = { x : int; y : int; }
val f : unit -> t mallocd = <fun>
|}]

let f (x @ external_) = malloc_ {x = x; y = x}
[%%expect{|
val f : int -> t mallocd = <fun>
|}]

type 'a t = {x : 'a ; y : 'a option mallocd }
let f x =
  let y = malloc_ (Some x) in
  malloc_ {x;y}
[%%expect {|
type 'a t = { x : 'a; y : 'a option mallocd; }
val f : 'a -> 'a t mallocd = <fun>
|}]




let foo () = malloc_ []
[%%expect {|
Line 1, characters 21-23:
1 | let foo () = malloc_ []
                         ^^
Error: This expression is not an allocation site.
|}]

let foo () = malloc_ [1]
[%%expect {|
val foo : unit -> int list mallocd = <fun>
|}]

let foo () = malloc_ [1;2;3]
[%%expect {|
val foo : unit -> int list mallocd = <fun>
|}]


let foo1 () = malloc_ [|1|]
[%%expect {|
Line 1, characters 22-27:
1 | let foo1 () = malloc_ [|1|]
                          ^^^^^
Error: This value is "internal" but expected to be "external_".
|}]


let foo3 (x @ external_) (y @ external_ ) = malloc_ [|x;y|]
[%%expect {|
Line 1, characters 52-59:
1 | let foo3 (x @ external_) (y @ external_ ) = malloc_ [|x;y|]
                                                        ^^^^^^^
Error: This value is "internal" but expected to be "external_".
|}]

let foo5 () = malloc_ [|"Kerplink";"Kerplank";"Kerplunk"|]
[%%expect {|
Line 1, characters 22-58:
1 | let foo5 () = malloc_ [|"Kerplink";"Kerplank";"Kerplunk"|]
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "internal" but expected to be "external_".
|}]


(* CR external-mode: The following error correctly catches an error: to malloc a
   record with mutable fields, all mutable fields must be marked @@ external_.
   However, it doesn't really explain that this is *why*, and worse, it's
   actively misleading. High priority to fix this. *)
type t = {mutable x : string; y : bool}
let bar1 x y = malloc_ {x;y}
[%%expect {|
type t = { mutable x : string; y : bool; }
Line 2, characters 23-28:
2 | let bar1 x y = malloc_ {x;y}
                           ^^^^^
Error: This value is "internal" but expected to be "external_".
|}]

type t = {mutable x : string @@ external_ ; y : bool}
let bar2 x y = malloc_ {x;y}
[%%expect {|
type t = { mutable x : string @@ external_; y : bool; }
val bar2 : string -> bool -> t mallocd = <fun>
|}]



type 'a t = Foo | Bar of 'a | Baz of 'a * string
let baz1 () = malloc_ Foo
[%%expect {|
type 'a t = Foo | Bar of 'a | Baz of 'a * string
Line 2, characters 22-25:
2 | let baz1 () = malloc_ Foo
                          ^^^
Error: This expression is not an allocation site.
|}]

let baz2 () = malloc_ (Bar 1)
[%%expect {|
val baz2 : unit -> int t mallocd = <fun>
|}]

let baz3 () = malloc_ (Bar "asdf")
[%%expect {|
val baz3 : unit -> string t mallocd = <fun>
|}]

let baz4 (x @ external_) = malloc_ (Baz (1,x))
[%%expect {|
val baz4 : string -> int t mallocd = <fun>
|}]

type 'a t = FooBar of {mutable x : 'a @@ external_ ; mutable y : 'a @@ external_}
let foobar (x @ external_) (y @ external_ ) = malloc_ (FooBar {x ; y})
[%%expect {|
type 'a t =
    FooBar of { mutable x : 'a @@ external_; mutable y : 'a @@ external_; }
val foobar : 'a -> 'a -> 'a t mallocd = <fun>
|}]

type 'a t = FooBar of {mutable x : 'a ; mutable y : 'a @@ external_}
let foobar (x @ external_) (y @ external_ ) = malloc_ (FooBar {x ; y})
[%%expect {|
type 'a t = FooBar of { mutable x : 'a; mutable y : 'a @@ external_; }
Line 2, characters 62-69:
2 | let foobar (x @ external_) (y @ external_ ) = malloc_ (FooBar {x ; y})
                                                                  ^^^^^^^
Error: This value is "internal" but expected to be "external_".
|}]

let blah () = malloc_ (lazy (2 + 2))
[%%expect {|
Line 1, characters 22-36:
1 | let blah () = malloc_ (lazy (2 + 2))
                          ^^^^^^^^^^^^^^
Error: Externally allocating lazy expressions is not supported yet.
|}]

let blargh () = malloc_ ((fun x -> x + 1) 2)
[%%expect {|
Line 1, characters 24-44:
1 | let blargh () = malloc_ ((fun x -> x + 1) 2)
                            ^^^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let floop () = malloc_ (fun x -> x + 1)
[%%expect {|
Line 1, characters 23-39:
1 | let floop () = malloc_ (fun x -> x + 1)
                           ^^^^^^^^^^^^^^^^
Error: Externally allocating functions is not supported yet.
|}]

let f () = malloc_ `Apple
[%%expect {|
Line 1, characters 19-25:
1 | let f () = malloc_ `Apple
                       ^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (`Apple 5)
[%%expect {|
val f : unit -> [> `Apple of int ] mallocd = <fun>
|}]

let f () = malloc_ (`Apple (1,2,3))
[%%expect {|
val f : unit -> [> `Apple of int * int * int ] mallocd = <fun>
|}]

let f () = malloc_ #(1,2)
[%%expect {|
Line 1, characters 19-25:
1 | let f () = malloc_ #(1,2)
                       ^^^^^^
Error: This expression is not an allocation site.
|}]


type t = #{x : int; y : char}
let f () = malloc_ #{x = 3; y = 'z'}
[%%expect {|
type t = #{ x : int; y : char; }
Line 2, characters 19-36:
2 | let f () = malloc_ #{x = 3; y = 'z'}
                       ^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let flarb () =
  malloc_ (1,
    let f x = (x,x) in
    let (a,b) = f 2 in
    a
  )
[%%expect {|
val flarb : unit -> (int * int) mallocd = <fun>
|}]
