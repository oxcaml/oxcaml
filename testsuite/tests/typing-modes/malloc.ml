(* TEST
 expect;
*)

type t : bits64 = int mallocd
[%%expect {|
type t = int mallocd
|}]

type t : bits64 mod external_ = int mallocd
[%%expect {|
Line 1, characters 0-43:
1 | type t : bits64 mod external_ = int mallocd
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int mallocd" is bits64
         because it is the primitive type mallocd.
       But the kind of type "int mallocd" must be a subkind of
           bits64 mod external_
         because of the definition of t at line 1, characters 0-43.
|}]

type t = float# mallocd
[%%expect {|
Line 1, characters 9-15:
1 | type t = float# mallocd
             ^^^^^^
Error: This type "float#" should be an instance of type "('a : value_or_null)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a sublayout of value
         because the type argument of mallocd has layout value_or_null.
|}]

type t = int or_null mallocd
[%%expect {|
type t = int or_null mallocd
|}]

let foo () = malloc_ (1,2)
[%%expect {|
val foo : unit -> (int * int) mallocd = <fun>
|}]

let f (x @ external_) (y @ external_) @ unique =
  malloc_ (x,y)
[%%expect {|
val f : 'a @ unique -> 'b @ unique -> ('a * 'b) mallocd @ unique = <fun>
|}]

let f (x @ external_ aliased) (y @ external_) @ unique =
  malloc_ (x,y)
[%%expect {|
Line 2, characters 11-12:
2 |   malloc_ (x,y)
               ^
Error: This value is "aliased" but expected to be "unique".
|}]

let f (x @ external_ unique) @ unique =
  malloc_ (x,x)
[%%expect {|
Line 2, characters 13-14:
2 |   malloc_ (x,x)
                 ^
Error: This value is used here, but it is already being used as unique:
Line 2, characters 11-12:
2 |   malloc_ (x,x)
               ^

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
Line 1, characters 24-28:
1 | let foo () = malloc_ [1;2;3]
                            ^^^^
Error: This value is "internal" but expected to be "external_".
|}]

let foo (x @ external_) (xs @ external_) =
  malloc_ (x :: xs)
[%%expect {|
val foo : 'a -> 'a list -> 'a list mallocd = <fun>
|}]

let foo (x @ internal) (xs @ internal) =
  malloc_ (x :: xs)
[%%expect {|
Line 2, characters 11-12:
2 |   malloc_ (x :: xs)
               ^
Error: This value is "internal" but expected to be "external_".
|}]

let foo (x @ external_) (xs @ internal) =
  malloc_ (x :: xs)
[%%expect {|
Line 2, characters 16-18:
2 |   malloc_ (x :: xs)
                    ^^
Error: This value is "internal" but expected to be "external_".
|}]


type t1 = Foo of int | Bar
type t2 = Baz of t1

let f () = malloc_ (Baz (Foo 3))
[%%expect {|
type t1 = Foo of int | Bar
type t2 = Baz of t1
Line 4, characters 24-31:
4 | let f () = malloc_ (Baz (Foo 3))
                            ^^^^^^^
Error: This value is "internal" but expected to be "external_".
|}]



let foo1 () = malloc_ [|1;2;3|]
[%%expect {|
Line 1, characters 22-31:
1 | let foo1 () = malloc_ [|1;2;3|]
                          ^^^^^^^^^
Error: Externally allocating arrays is not supported yet.
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

type 'a r = {mutable x : 'a @@ external_ ; mutable y : 'a @@ external_}
let flop (x @ external_) (y @ external_ ) = malloc_ {x ; y}
[%%expect {|
type 'a r = { mutable x : 'a @@ external_; mutable y : 'a @@ external_; }
val flop : 'a -> 'a -> 'a r mallocd = <fun>
|}]

type 'a t = FooBar of 'a
let fleep (x @ external_) (y @ external_ ) = malloc_ (FooBar {x ; y})
[%%expect {|
type 'a t = FooBar of 'a
Line 2, characters 61-68:
2 | let fleep (x @ external_) (y @ external_ ) = malloc_ (FooBar {x ; y})
                                                                 ^^^^^^^
Error: This value is "internal" but expected to be "external_".
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
Line 1, characters 27-34:
1 | let f () = malloc_ (`Apple (1,2,3))
                               ^^^^^^^
Error: This value is "internal" but expected to be "external_".
|}]

type t = {a : int; b : int}
let f () = malloc_ (`Apple {a = 1; b = 3})
[%%expect {|
type t = { a : int; b : int; }
Line 2, characters 27-41:
2 | let f () = malloc_ (`Apple {a = 1; b = 3})
                               ^^^^^^^^^^^^^^
Error: This value is "internal" but expected to be "external_".
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

let obj () = malloc_ (object method x () = 42 end)
[%%expect{|
Line 1, characters 21-50:
1 | let obj () = malloc_ (object method x () = 42 end)
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Externally allocating objects is not supported yet.
|}]


exception Exn of int
let exn () = malloc_ (Exn 2)
[%%expect{|
exception Exn of int
val exn : unit -> exn mallocd = <fun>
|}]

let exn () = malloc_ (raise (Exn 2))
[%%expect{|
Line 1, characters 21-36:
1 | let exn () = malloc_ (raise (Exn 2))
                         ^^^^^^^^^^^^^^^
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

let fleep () = malloc_ (1,(2,3))
[%%expect{|
Line 1, characters 26-31:
1 | let fleep () = malloc_ (1,(2,3))
                              ^^^^^
Error: This value is "internal" but expected to be "external_".
|}]

type 'a t = Foo of 'a
let glorb1 () = malloc_ (Foo (1,2))
[%%expect {|
type 'a t = Foo of 'a
Line 2, characters 29-34:
2 | let glorb1 () = malloc_ (Foo (1,2))
                                 ^^^^^
Error: This value is "internal" but expected to be "external_".
|}]

type 'a t = Foo of 'a * 'a
let glorb2 () = malloc_ (Foo (1,2))
[%%expect {|
type 'a t = Foo of 'a * 'a
val glorb2 : unit -> int t mallocd = <fun>
|}]

type t = Pack : 'k. 'k -> t
let f (x @ external_) = malloc_ (Pack x)
[%%expect{|
type t = Pack : 'k -> t
val f : 'a -> t mallocd = <fun>
|}]

let f (x @ internal) = malloc_ (Pack x)
[%%expect{|
Line 1, characters 37-38:
1 | let f (x @ internal) = malloc_ (Pack x)
                                         ^
Error: This value is "internal" but expected to be "external_".
|}]

type 'a t =
| Foo : int * 'a -> int t
| Bar : { x : char ; y : 'a } -> char t
let f (x @ external_) = malloc_ (Foo (3,x))
[%%expect{|
type 'a t = Foo : int * 'a -> int t | Bar : { x : char; y : 'a; } -> char t
val f : 'a -> int t mallocd = <fun>
|}]

let f (y @ external_) = malloc_ (Bar {x = 'c'; y })
[%%expect{|
val f : 'a -> char t mallocd = <fun>
|}]
