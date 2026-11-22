(* TEST
 readonly_files = "sum_shape.mli";
 setup-ocamlc.byte-build-env;
 module = "sum_shape.mli";
 ocamlc.byte;
 {
   expect;
 }{
   flags = "-no-ikinds";
   expect;
 }
*)

#directory "ocamlc.byte";;

module Reordered : sig
  type t : (value + void) + (bits32 & bits64)
end = struct type t = Sum_shape.t end
[%%expect{|
Line 3, characters 6-37:
3 | end = struct type t = Sum_shape.t end
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = Sum_shape.t end
       is not included in
         sig type t : (value + void) + (bits32 & bits64) end
       Type declarations do not match:
         type t = Sum_shape.t
       is not included in
         type t : (value + void) + (bits32 & bits64)
       The layout of the first is (bits32 & bits64) + (value + void).
       But the layout of the first must be a sublayout of
           (value + void) + (bits32 & bits64)
         because of the definition of t at line 2, characters 2-45.
|}]

module Flattened : sig
  type t : (bits32 & bits64) + value + void
end = struct type t = Sum_shape.t end
[%%expect{|
Line 3, characters 6-37:
3 | end = struct type t = Sum_shape.t end
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = Sum_shape.t end
       is not included in
         sig type t : (bits32 & bits64) + value + void end
       Type declarations do not match:
         type t = Sum_shape.t
       is not included in
         type t : (bits32 & bits64) + value + void
       The layout of the first is (bits32 & bits64) + (value + void).
       But the layout of the first must be a sublayout of
           (bits32 & bits64) + value + void
         because of the definition of t at line 2, characters 2-43.
|}]

module Different_component : sig
  type t : (bits32 & bits32) + (value + void)
end = struct type t = Sum_shape.t end
[%%expect{|
Line 3, characters 6-37:
3 | end = struct type t = Sum_shape.t end
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = Sum_shape.t end
       is not included in
         sig type t : (bits32 & bits32) + (value + void) end
       Type declarations do not match:
         type t = Sum_shape.t
       is not included in
         type t : (bits32 & bits32) + (value + void)
       The layout of the first is (bits32 & bits64) + (value + void).
       But the layout of the first must be a sublayout of
           (bits32 & bits32) + (value + void)
         because of the definition of t at line 2, characters 2-45.
|}]

module Product : sig
  type t : (bits32 & bits64) & (value + void)
end = struct type t = Sum_shape.t end
[%%expect{|
Line 3, characters 6-37:
3 | end = struct type t = Sum_shape.t end
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = Sum_shape.t end
       is not included in
         sig type t : (bits32 & bits64) & (value + void) end
       Type declarations do not match:
         type t = Sum_shape.t
       is not included in
         type t : (bits32 & bits64) & (value + void)
       The layout of the first is (bits32 & bits64) + (value + void).
       But the layout of the first must be a sublayout of
           (bits32 & bits64) & (value + void)
         because of the definition of t at line 2, characters 2-45.
|}]

let pass (x : Sum_shape.t) = x
[%%expect{|
val pass : Sum_shape.t -> Sum_shape.t = <fun>
|}]

let empty : Sum_shape.t array = [||]
[%%expect{|
Line 1, characters 32-36:
1 | let empty : Sum_shape.t array = [||]
                                    ^^^^
Error: Storage for an array element of type Sum_shape.t contains both
       GC-scannable fields and raw fields. Inline array elements must be
       entirely GC-scannable or entirely GC-ignorable.
|}]

module Read (V : sig val x : Sum_shape.t end) = struct let x = V.x end
[%%expect{|
module Read :
  functor (V : sig val x : Sum_shape.t end) -> sig val x : Sum_shape.t end
|}]

external pass_external : Sum_shape.t -> unit = "pass_external"
[%%expect{|
Line 1, characters 0-62:
1 | external pass_external : Sum_shape.t -> unit = "pass_external"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}]

type record = { payload : Sum_shape.t }
let project r = r.payload
[%%expect{|
type record = { payload : Sum_shape.t; }
val project : record -> Sum_shape.t = <fun>
|}]

module Narrowed : sig
  type refined : bits32 + (value non_pointer64)
end = struct type refined : bits32 + value end
[%%expect{|
Line 3, characters 6-46:
3 | end = struct type refined : bits32 + value end
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type refined : bits32 + value end
       is not included in
         sig type refined : bits32 + value non_pointer64 end
       Type declarations do not match:
         type refined : bits32 + value
       is not included in
         type refined : bits32 + value non_pointer64
       The layout of the first is bits32 + value
         because of the definition of refined at line 3, characters 13-42.
       But the layout of the first must be a sublayout of
           bits32 + value non_pointer64
         because of the definition of refined at line 2, characters 2-47.
       Note: The layout of immediate64 is value non_pointer64.
|}]

external size : Sum_shape.t array -> int = "%array_element_size_in_bytes"
let element_size xs = size xs
[%%expect{|
external size : Sum_shape.t array -> int = "%array_element_size_in_bytes"
Line 2, characters 27-29:
2 | let element_size xs = size xs
                               ^^
Error: Storage for an array element of type Sum_shape.t contains both
       GC-scannable fields and raw fields. Inline array elements must be
       entirely GC-scannable or entirely GC-ignorable.
|}]

type cycle = Leaf | Next of cycle [@@unboxed]
[%%expect{|
Line 1, characters 0-45:
1 | type cycle = Leaf | Next of cycle [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "cycle" is recursive without boxing:
         "cycle" contains "cycle"
|}]

type finite = Leaf | Next of finite list [@@unboxed]
[%%expect{|
type finite = Leaf | Next of finite list [@@unboxed]
|}]

type ('a : any) unsized = Empty | Item of 'a [@@unboxed]
[%%expect{|
Line 1, characters 42-44:
1 | type ('a : any) unsized = Empty | Item of 'a [@@unboxed]
                                              ^^
Error: This type cannot be unboxed because an arm has unknown size.
|}]

type mixed = Text of string | Bits of int64_u [@@unboxed]
let mixed_array s = [|Text s|]
[%%expect{|
type mixed = Text of string | Bits of int64_u [@@unboxed]
Line 2, characters 20-30:
2 | let mixed_array s = [|Text s|]
                        ^^^^^^^^^^
Error: Storage for an array element of type mixed contains both
       GC-scannable fields and raw fields. Inline array elements must be
       entirely GC-scannable or entirely GC-ignorable.
|}]

type wrapped = Wrap of mixed
let wrapped = Wrap (Text "payload")
[%%expect{|
type wrapped = Wrap of mixed
val wrapped : wrapped = Wrap <unboxed sum>
|}]

type printed = { choice : mixed; label : string }
let printed = { choice = Bits #42L; label = "next field" }
[%%expect{|
type printed = { choice : mixed; label : string; }
val printed : printed = {choice = <unboxed sum>; label = "next field"}
|}]

type ('a : any) first = 'a option = None | Some of 'a
type ('a : any) second = 'a option = None | Some of 'a
type ('a : any) alias = 'a first
[%%expect{|
type ('a : any) first = 'a option = None | Some of 'a
type ('a : any) second = 'a option = None | Some of 'a
type ('a : any) alias = 'a first
|}]

type rejected = int first#
[%%expect{|
Line 1, characters 20-26:
1 | type rejected = int first#
                        ^^^^^^
Error: The type "first" has no unboxed version.
|}]

type rejected = int second#
[%%expect{|
Line 1, characters 20-27:
1 | type rejected = int second#
                        ^^^^^^^
Error: The type "second" has no unboxed version.
|}]

type rejected = int alias#
[%%expect{|
Line 1, characters 20-26:
1 | type rejected = int alias#
                        ^^^^^^
Error: The type "alias" has no unboxed version.
|}]

type public = Stop | More of int
module Private = struct type t = public = private Stop | More of int end
[%%expect{|
type public = Stop | More of int
module Private : sig type t = public = private Stop | More of int end
|}]

let rejected () = Private.#Stop
[%%expect{|
Line 1, characters 18-31:
1 | let rejected () = Private.#Stop
                      ^^^^^^^^^^^^^
Error: Cannot create values of the private type "public#"
|}]
