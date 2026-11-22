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

type mutable_arm = End | Item of { mutable count : int }
let set = function #End -> () | #Item r -> r.count <- 1
[%%expect{|
type mutable_arm = End | Item of { mutable count : int; }
Line 2, characters 43-55:
2 | let set = function #End -> () | #Item r -> r.count <- 1
                                               ^^^^^^^^^^^^
Error: The unboxed record field "count" is declared mutable,
       but fields of unboxed records can't be mutated.
|}]

type inner_arm = Off | On of int
type nested_record_arm = Stop | Entry of { inner : inner_arm# }
type retained : void + (void + value) = nested_record_arm#
[%%expect{|
type inner_arm = Off | On of int
type nested_record_arm = Stop | Entry of { inner : inner_arm#; }
type retained = nested_record_arm#
|}]

type direct_arm = Stop | Entry of inner_arm#
type fused : void + void + value = direct_arm#
type wrong : void + (void + value) = direct_arm#
[%%expect{|
type direct_arm = Stop | Entry of inner_arm#
type fused = direct_arm#
Line 3, characters 0-48:
3 | type wrong : void + (void + value) = direct_arm#
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "direct_arm#" is void + void + value non_pointer
         because it's an [@@unboxed] type.
       But the layout of type "direct_arm#" must be a sublayout of
           void + (void + value)
         because of the definition of wrong at line 3, characters 0-48.
       Note: The layout of immediate is value non_pointer.
|}]

type wrong : void + void + value = nested_record_arm#
[%%expect{|
Line 1, characters 0-53:
1 | type wrong : void + void + value = nested_record_arm#
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "nested_record_arm#" is
           void + (void + value non_pointer)
         because it's an [@@unboxed] type.
       But the layout of type "nested_record_arm#" must be a sublayout of
           void + void + value
         because of the definition of wrong at line 1, characters 0-53.
       Note: The layout of immediate is value non_pointer.
|}]

type void_record_arm = Stop | Entry of { ignored : unit# }
type rejected = void_record_arm#
[%%expect{|
type void_record_arm = Stop | Entry of { ignored : unit#; }
Line 2, characters 16-32:
2 | type rejected = void_record_arm#
                    ^^^^^^^^^^^^^^^^
Error: The type "void_record_arm" has no unboxed version.
|}]

type atomic_record_arm = Stop | Entry of { mutable count : int [@atomic] }
type rejected = atomic_record_arm#
[%%expect{|
type atomic_record_arm = Stop | Entry of { mutable count : int [@atomic]; }
Line 2, characters 16-34:
2 | type rejected = atomic_record_arm#
                    ^^^^^^^^^^^^^^^^^^
Error: The type "atomic_record_arm" has no unboxed version.
|}]

module Private_arm : sig
  type t = private Stop | Entry of { count : int }
end = struct
  type t = Stop | Entry of { count : int }
end
let read_private = Private_arm.(function #Stop -> 0 | #Entry r -> r.#count)
let rejected () = Private_arm.#Entry { count = 1 }
[%%expect{|
module Private_arm :
  sig type t = private Stop | Entry of { count : int; } end
val read_private : Private_arm.t# -> int = <fun>
Line 7, characters 37-50:
7 | let rejected () = Private_arm.#Entry { count = 1 }
                                         ^^^^^^^^^^^^^
Error: Cannot create values of the private type "Private_arm.t#.Entry"
|}]

type recursive_arm = Stop | More of { tail : recursive_arm } [@@unboxed]
[%%expect{|
Line 1, characters 0-72:
1 | type recursive_arm = Stop | More of { tail : recursive_arm } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "recursive_arm" is recursive without boxing:
         "recursive_arm" contains "recursive_arm"
|}]

type 'a local_arm = Stop | Entry of { left : 'a; right : 'a }
let select (local_ left) (local_ right) =
  match #Entry { left; right } with #Stop -> right | #Entry arm -> arm.#left
[%%expect{|
type 'a local_arm = Stop | Entry of { left : 'a; right : 'a; }
val select : 'a @ local -> 'a @ local -> 'a @ local = <fun>
|}]

type atomic = End | Item of { mutable n : int [@atomic] } [@@unboxed]
[%%expect{|
Line 1, characters 30-55:
1 | type atomic = End | Item of { mutable n : int [@atomic] } [@@unboxed]
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed record labels cannot be atomic
|}]

type direct = Empty | Entry of { mutable count : int } [@@unboxed]
let set (boxed : direct box) =
  match boxed with Empty -> () | Entry r -> r.count <- 1
[%%expect{|
type direct = Empty | Entry of { mutable count : int; } [@@unboxed]
Line 3, characters 44-56:
3 |   match boxed with Empty -> () | Entry r -> r.count <- 1
                                                ^^^^^^^^^^^^
Error: The unboxed record field "count" is declared mutable,
       but fields of unboxed records can't be mutated.
|}]
