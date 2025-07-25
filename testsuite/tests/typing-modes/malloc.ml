(* TEST
 expect;
*)

(*** Kind and mode-crossing behavior of mallocd ***)

type t : word = int mallocd
[%%expect {|
type t = int mallocd
|}]

type t : word mod byte_external = unit mallocd
[%%expect {|
Line 1, characters 0-46:
1 | type t : word mod byte_external = unit mallocd
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "unit mallocd" is
           word mod many unyielding stateless immutable external_
         because it is the primitive type mallocd.
       But the kind of type "unit mallocd" must be a subkind of
           word mod external_
         because of the definition of t at line 1, characters 0-46.
|}]

type 'a t : word mod external_ = 'a mallocd
[%%expect {|
type 'a t = 'a mallocd
|}]

type 'a t : word mod global = 'a mallocd
[%%expect{|
Line 1, characters 0-40:
1 | type 'a t : word mod global = 'a mallocd
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a mallocd" is
           word
             mod many unyielding stateless immutable external_
             with 'a @@ external_
         because it is the primitive type mallocd.
       But the kind of type "'a mallocd" must be a subkind of word mod global
         because of the definition of t at line 1, characters 0-40.

       The first mode-crosses less than the second along:
         locality: mod local ≰ mod global
         yielding: mod unyielding with 'a ≰ mod unyielding
|}]

type 'a t : word mod global with 'a = 'a mallocd
[%%expect{|
Line 1, characters 0-48:
1 | type 'a t : word mod global with 'a = 'a mallocd
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a mallocd" is
           word
             mod many unyielding stateless immutable external_
             with 'a @@ external_
         because it is the primitive type mallocd.
       But the kind of type "'a mallocd" must be a subkind of
           word mod global with 'a
         because of the definition of t at line 1, characters 0-48.

       The first mode-crosses less than the second along:
         locality: mod local ≰ mod global with 'a
|}]

type 'a t : word mod many = 'a mallocd
[%%expect{|
Line 1, characters 0-38:
1 | type 'a t : word mod many = 'a mallocd
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a mallocd" is
           word
             mod many unyielding stateless immutable external_
             with 'a @@ external_
         because it is the primitive type mallocd.
       But the kind of type "'a mallocd" must be a subkind of word mod many
         because of the definition of t at line 1, characters 0-38.

       The first mode-crosses less than the second along:
         linearity: mod many with 'a ≰ mod many
|}]


type 'a t : word mod many with 'a = 'a mallocd
[%%expect{|
type 'a t = 'a mallocd
|}]

type 'a t : word mod portable = 'a mallocd
[%%expect{|
Line 1, characters 0-42:
1 | type 'a t : word mod portable = 'a mallocd
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a mallocd" is
           word
             mod many unyielding stateless immutable external_
             with 'a @@ external_
         because it is the primitive type mallocd.
       But the kind of type "'a mallocd" must be a subkind of word mod portable
         because of the definition of t at line 1, characters 0-42.

       The first mode-crosses less than the second along:
         portability: mod portable with 'a ≰ mod portable
|}]

type 'a t : word mod portable with 'a = 'a mallocd
[%%expect{|
type 'a t = 'a mallocd
|}]

type 'a t : word mod aliased = 'a mallocd
[%%expect{|
Line 1, characters 0-41:
1 | type 'a t : word mod aliased = 'a mallocd
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a mallocd" is
           word
             mod many unyielding stateless immutable external_
             with 'a @@ external_
         because it is the primitive type mallocd.
       But the kind of type "'a mallocd" must be a subkind of word mod aliased
         because of the definition of t at line 1, characters 0-41.

       The first mode-crosses less than the second along:
         uniqueness: mod unique ≰ mod aliased
|}]

type 'a t : word mod aliased with 'a = 'a mallocd
[%%expect{|
Line 1, characters 0-49:
1 | type 'a t : word mod aliased with 'a = 'a mallocd
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a mallocd" is
           word
             mod many unyielding stateless immutable external_
             with 'a @@ external_
         because it is the primitive type mallocd.
       But the kind of type "'a mallocd" must be a subkind of
           word mod aliased with 'a
         because of the definition of t at line 1, characters 0-49.

       The first mode-crosses less than the second along:
         uniqueness: mod unique ≰ mod aliased with 'a
|}]

type 'a t : word mod unyielding = 'a mallocd
[%%expect{|
Line 1, characters 0-44:
1 | type 'a t : word mod unyielding = 'a mallocd
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a mallocd" is
           word
             mod many unyielding stateless immutable external_
             with 'a @@ external_
         because it is the primitive type mallocd.
       But the kind of type "'a mallocd" must be a subkind of
           word mod unyielding
         because of the definition of t at line 1, characters 0-44.

       The first mode-crosses less than the second along:
         yielding: mod unyielding with 'a ≰ mod unyielding
|}]

type 'a t : word mod unyielding with 'a = 'a mallocd
[%%expect{|
type 'a t = 'a mallocd
|}]

type 'a t : word mod stateless = 'a mallocd
[%%expect{|
Line 1, characters 0-43:
1 | type 'a t : word mod stateless = 'a mallocd
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a mallocd" is
           word
             mod many unyielding stateless immutable external_
             with 'a @@ external_
         because it is the primitive type mallocd.
       But the kind of type "'a mallocd" must be a subkind of
           word mod stateless
         because of the definition of t at line 1, characters 0-43.

       The first mode-crosses less than the second along:
         portability: mod portable with 'a ≰ mod portable
         statefulness: mod stateless with 'a ≰ mod stateless
|}]

type 'a t : word mod stateless with 'a = 'a mallocd
[%%expect{|
type 'a t = 'a mallocd
|}]

type 'a t : word mod contended = 'a mallocd
[%%expect{|
Line 1, characters 0-43:
1 | type 'a t : word mod contended = 'a mallocd
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a mallocd" is
           word
             mod many unyielding stateless immutable external_
             with 'a @@ external_
         because it is the primitive type mallocd.
       But the kind of type "'a mallocd" must be a subkind of
           word mod contended
         because of the definition of t at line 1, characters 0-43.

       The first mode-crosses less than the second along:
         contention: mod contended with 'a ≰ mod contended
|}]

type 'a t : word mod contended with 'a = 'a mallocd
[%%expect{|
type 'a t = 'a mallocd
|}]

type 'a t : word mod immutable = 'a mallocd
[%%expect{|
Line 1, characters 0-43:
1 | type 'a t : word mod immutable = 'a mallocd
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a mallocd" is
           word
             mod many unyielding stateless immutable external_
             with 'a @@ external_
         because it is the primitive type mallocd.
       But the kind of type "'a mallocd" must be a subkind of
           word mod immutable
         because of the definition of t at line 1, characters 0-43.

       The first mode-crosses less than the second along:
         contention: mod contended with 'a ≰ mod contended
         visibility: mod immutable with 'a ≰ mod immutable
|}]

type 'a t : word mod immutable with 'a = 'a mallocd
[%%expect{|
type 'a t = 'a mallocd
|}]

(* Layout checks: [t mallocd] is ok only if t is value_or_null *)
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

type t = #(int * int)
[%%expect {|
type t = #(int * int)
|}]

type t = int or_null mallocd
[%%expect {|
type t = int or_null mallocd
|}]

(* Basic type behavior: does malloc_ return a mallocd, and does it
   require that its arguments are passed @ external *)
let foo () = malloc_ (1,2)
[%%expect {|
val foo : unit -> (int * int) mallocd = <fun>
|}]

let f x y =
  malloc_ (x,y)
[%%expect {|
val f : 'a @ external_ -> 'b @ external_ -> ('a * 'b) mallocd = <fun>
|}]

let f x y : _ @ unique =
  malloc_ (x,y)
[%%expect {|
val f :
  'a @ unique external_ ->
  'b @ unique external_ -> ('a * 'b) mallocd @ unique = <fun>
|}]

let f (x @ aliased) y : _ @ unique =
  malloc_ (x,y)
[%%expect {|
Line 2, characters 11-12:
2 |   malloc_ (x,y)
               ^
Error: This value is "aliased" but expected to be "unique".
|}]

let f (x @ external_) (y @ external_) =
  malloc_ (x,y)
[%%expect {|
val f : 'a @ external_ -> 'b @ external_ -> ('a * 'b) mallocd = <fun>
|}]


let f (x @ internal) y =
  malloc_ (x,y)
[%%expect {|
Line 2, characters 11-12:
2 |   malloc_ (x,y)
               ^
Error: This value is "internal" but expected to be "external_".
|}]

(* Int crosses externality so this is fine*)
let f (x : int @ internal) y =
  malloc_ (x,y)
[%%expect {|
val f : int -> 'a @ external_ -> (int * 'a) mallocd = <fun>
|}]

let f x y @ unique =
  malloc_ (x,y)
[%%expect {|
val f :
  'a @ unique external_ ->
  'b @ unique external_ -> ('a * 'b) mallocd @ unique = <fun>
|}]

let f (x @ aliased) y =
  malloc_ (x,y)
[%%expect {|
val f : 'a @ external_ -> 'b @ external_ -> ('a * 'b) mallocd = <fun>
|}]


let f (x @ aliased) y @ unique =
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


(* Can allocate records *)

type t = {x : int; y : int}
let f () = malloc_ {x = 3; y = 4}
[%%expect{|
type t = { x : int; y : int; }
val f : unit -> t mallocd = <fun>
|}]

let f (x @ external_) = malloc_ {x = x; y = x}
[%%expect{|
val f : int @ external_ -> t mallocd = <fun>
|}]

type 'a t = {z : 'a ; y : 'a option mallocd }
let f x z =
  let y = malloc_ (Some x) in
  malloc_ {z;y}
[%%expect {|
type 'a t = { z : 'a; y : 'a option mallocd; }
val f : 'a @ external_ -> 'a @ external_ -> 'a t mallocd = <fun>
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
val bar2 : string @ external_ -> bool -> t mallocd = <fun>
|}]


(* Can allocate variants *)

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

type 'a t = Foo | Bar of 'a | Baz of 'a * string


let baz2 () = malloc_ (Bar 1)
[%%expect {|
type 'a t = Foo | Bar of 'a | Baz of 'a * string
val baz2 : unit -> int t mallocd = <fun>
|}]

let baz3 () = malloc_ (Bar "asdf")
[%%expect {|
val baz3 : unit -> string t mallocd = <fun>
|}]

let baz4 x = malloc_ (Baz (1,x))
[%%expect {|
val baz4 : string @ external_ -> int t mallocd = <fun>
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

type ('a : value & value) t = Foo of 'a
let glorb1 () = malloc_ (Foo #(1,2))
[%%expect {|
type ('a : value & value) t = Foo of 'a
val glorb1 : unit -> #(int * int) t mallocd = <fun>
|}]

type 'a t = Foo of 'a * 'a
let glorb2 () = malloc_ (Foo (1,2))
[%%expect {|
type 'a t = Foo of 'a * 'a
val glorb2 : unit -> int t mallocd = <fun>
|}]

(* Can allocate variants with record arguments *)

type 'a t = FooBar of {x : 'a; y : 'a}
let f x y = FooBar {x;y}
[%%expect{|
type 'a t = FooBar of { x : 'a; y : 'a; }
val f : 'a -> 'a -> 'a t = <fun>
|}]

type r = {x : int; y : int}
type 'a t = FooBar of 'a
let fleep (x @ external_) (y @ external_ ) = malloc_ (FooBar {x ; y})
[%%expect {|
type r = { x : int; y : int; }
type 'a t = FooBar of 'a
Line 3, characters 61-68:
3 | let fleep (x @ external_) (y @ external_ ) = malloc_ (FooBar {x ; y})
                                                                 ^^^^^^^
Error: This value is "internal" but expected to be "external_".
|}]

(* The same mutability modality rules apply to variants that carry records as
   normal records *)
type 'a t = FooBar of {mutable x : 'a ; mutable y : 'a @@ external_}
let foobar (x @ external_) (y @ external_ ) = malloc_ (FooBar {x ; y})
[%%expect {|
type 'a t = FooBar of { mutable x : 'a; mutable y : 'a @@ external_; }
Line 2, characters 62-69:
2 | let foobar (x @ external_) (y @ external_ ) = malloc_ (FooBar {x ; y})
                                                                  ^^^^^^^
Error: This value is "internal" but expected to be "external_".
|}]


type 'a t = FooBar of {mutable x : 'a @@ external_ ; mutable y : 'a @@ external_}
let foobar (x @ external_) (y @ external_ ) = malloc_ (FooBar {x ; y})
[%%expect {|
type 'a t =
    FooBar of { mutable x : 'a @@ external_; mutable y : 'a @@ external_; }
val foobar : 'a @ external_ -> 'a @ external_ -> 'a t mallocd = <fun>
|}]



(* GADT allocation works like variants *)

type t = Pack : 'k. 'k -> t
let f (x @ external_) = malloc_ (Pack x)
[%%expect{|
type t = Pack : 'k -> t
val f : 'a @ external_ -> t mallocd = <fun>
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
val f : 'a @ external_ -> int t mallocd = <fun>
|}]

let f (y @ external_) = malloc_ (Bar {x = 'c'; y })
[%%expect{|
val f : 'a @ external_ -> char t mallocd = <fun>
|}]


(* You can allocate polymorphic variants *)

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

(* Expressions that are not allocation sites cannot be allocated *)

type t = Foo
let bazz () = malloc_ Foo
[%%expect {|
type t = Foo
Line 2, characters 22-25:
2 | let bazz () = malloc_ Foo
                          ^^^
Error: This expression is not an allocation site.
|}]

let foo () = malloc_ []
[%%expect {|
Line 1, characters 21-23:
1 | let foo () = malloc_ []
                         ^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ `Apple
[%%expect {|
Line 1, characters 19-25:
1 | let f () = malloc_ `Apple
                       ^^^^^^
Error: This expression is not an allocation site.
|}]

let exn () = malloc_ (raise (Exn 2))
[%%expect{|
Line 1, characters 21-36:
1 | let exn () = malloc_ (raise (Exn 2))
                         ^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let blargh () = malloc_ ((fun x -> x + 1) 2)
[%%expect {|
Line 1, characters 24-44:
1 | let blargh () = malloc_ ((fun x -> x + 1) 2)
                            ^^^^^^^^^^^^^^^^^^^^
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

let f () = malloc_ #(1,2)
[%%expect {|
Line 1, characters 19-25:
1 | let f () = malloc_ #(1,2)
                       ^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ 3
[%%expect {|
Line 1, characters 19-20:
1 | let f () = malloc_ 3
                       ^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (let x = 3 in 5)
[%%expect {|
Line 1, characters 19-35:
1 | let f () = malloc_ (let x = 3 in 5)
                       ^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]


let f () = malloc_ (List.hd [1;2])
[%%expect {|
Line 1, characters 19-34:
1 | let f () = malloc_ (List.hd [1;2])
                       ^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let f xs = malloc_ (match xs with _ -> 0)
[%%expect {|
Line 1, characters 19-41:
1 | let f xs = malloc_ (match xs with _ -> 0)
                       ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]



(* Allocating lists *)
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

let foo x y =
  malloc_ (x :: xs)
[%%expect {|
Line 2, characters 16-18:
2 |   malloc_ (x :: xs)
                    ^^
Error: Unbound value "xs"
|}]

(* The following kinds of allocations are not yet supported *)
let foo1 () = malloc_ [|1;2;3|]
[%%expect {|
Line 1, characters 22-31:
1 | let foo1 () = malloc_ [|1;2;3|]
                          ^^^^^^^^^
Error: Externally allocating arrays is not supported yet.
|}]

let blah () = malloc_ (lazy (2 + 2))
[%%expect {|
Line 1, characters 22-36:
1 | let blah () = malloc_ (lazy (2 + 2))
                          ^^^^^^^^^^^^^^
Error: Externally allocating lazy expressions is not supported yet.
|}]

let blah () = malloc_ ([a for a = 1 to 10])
[%%expect {|
Line 1, characters 22-43:
1 | let blah () = malloc_ ([a for a = 1 to 10])
                          ^^^^^^^^^^^^^^^^^^^^^
Error: Externally allocating comprehensions is not supported yet.
|}]


let floop () = malloc_ (fun x -> x + 1)
[%%expect {|
Line 1, characters 23-39:
1 | let floop () = malloc_ (fun x -> x + 1)
                           ^^^^^^^^^^^^^^^^
Error: Externally allocating functions is not supported yet.
|}]

let obj () = malloc_ (object method x () = 42 end)
[%%expect{|
Line 1, characters 21-50:
1 | let obj () = malloc_ (object method x () = 42 end)
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Externally allocating objects is not supported yet.
|}]

class t = object
  method x () = 42
end

let f () = malloc_ (new t)
[%%expect {|
class t : object method x : unit -> int end
Line 5, characters 19-26:
5 | let f () = malloc_ (new t)
                       ^^^^^^^
Error: Externally allocating objects is not supported yet.
|}]




(* Malloc acts only shallowly: only the topmost allocation is externally
   allocated *)
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


(* Additional tests for non-allocation site syntax forms *)

let f () = malloc_ (try 1 with _ -> 2)
[%%expect {|
Line 1, characters 19-38:
1 | let f () = malloc_ (try 1 with _ -> 2)
                       ^^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

type t = {a : int; b : int}
let f x = malloc_ (x.a)
[%%expect {|
type t = { a : int; b : int; }
Line 2, characters 18-23:
2 | let f x = malloc_ (x.a)
                      ^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (if true then 1 else 2)
[%%expect {|
Line 1, characters 19-42:
1 | let f () = malloc_ (if true then 1 else 2)
                       ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (print_int 1; 2)
[%%expect {|
Line 1, characters 19-35:
1 | let f () = malloc_ (print_int 1; 2)
                       ^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (while false do () done; 1)
[%%expect {|
Line 1, characters 19-46:
1 | let f () = malloc_ (while false do () done; 1)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (for i = 1 to 10 do () done; 1)
[%%expect {|
Line 1, characters 19-50:
1 | let f () = malloc_ (for i = 1 to 10 do () done; 1)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (1 : int)
[%%expect {|
Line 1, characters 19-28:
1 | let f () = malloc_ (1 : int)
                       ^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (1 :> int)
[%%expect {|
Line 1, characters 19-29:
1 | let f () = malloc_ (1 :> int)
                       ^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (assert true)
[%%expect {|
Line 1, characters 19-32:
1 | let f () = malloc_ (assert true)
                       ^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (let module M = struct end in 1)
[%%expect {|
Line 1, characters 19-51:
1 | let f () = malloc_ (let module M = struct end in 1)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

exception E
let f () = malloc_ (let exception E in 1)
[%%expect {|
exception E
Line 2, characters 19-41:
2 | let f () = malloc_ (let exception E in 1)
                       ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (let open List in [])
[%%expect {|
Line 1, characters 19-40:
1 | let f () = malloc_ (let open List in [])
                       ^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

type t = {mutable a : int}
let f x = malloc_ (x.a <- 5)
[%%expect {|
type t = { mutable a : int; }
Line 2, characters 18-28:
2 | let f x = malloc_ (x.a <- 5)
                      ^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (fun (type a) (x : a) -> x)
[%%expect {|
Line 1, characters 19-46:
1 | let f () = malloc_ (fun (type a) (x : a) -> x)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Externally allocating functions is not supported yet.
|}]

class c = object method m = 1 end
let f x = malloc_ (x#m)
[%%expect {|
class c : object method m : int end
Line 2, characters 18-23:
2 | let f x = malloc_ (x#m)
                      ^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ {< >}
[%%expect {|
Line 1, characters 19-24:
1 | let f () = malloc_ {< >}
                       ^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ (malloc_ (1,2))
[%%expect {|
Line 1, characters 19-34:
1 | let f () = malloc_ (malloc_ (1,2))
                       ^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

type t = {mutable a : int}
let f x = malloc_ (overwrite_ x with {a = 5})
[%%expect {|
type t = { mutable a : int; }
Line 2, characters 18-45:
2 | let f x = malloc_ (overwrite_ x with {a = 5})
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ [%extension_constructor List.Cons]
[%%expect {|
Line 1, characters 19-53:
1 | let f () = malloc_ [%extension_constructor List.Cons]
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let f () = malloc_ [%here]
[%%expect {|
Line 1, characters 19-26:
1 | let f () = malloc_ [%here]
                       ^^^^^^^
Error: This expression is not an allocation site.
|}]
