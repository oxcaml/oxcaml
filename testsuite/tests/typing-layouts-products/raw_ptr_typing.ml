(* TEST
 flambda2;
 {
   expect;
 }
*)

(* Typing tests for the [@raw_ptr] attribute on external arguments. *)

(* Valid: a fat pointer (value & bits64) argument, on a [@@noalloc]
   external with a separate native name. *)
external ok : (#(bytes * int64_u)[@raw_ptr]) -> unit = "b" "n" [@@noalloc]
[%%expect{|
external ok : (#(bytes * int64_u) [@raw_ptr]) -> unit = "b" "n" [@@noalloc]
|}]

(* Valid: value_or_null base, addressable wrapping, extra arguments. *)
external ok2 :
  (#(string or_null * int64_u)[@raw_ptr]) -> int -> unit = "b" "n" [@@noalloc]
[%%expect{|
external ok2 : (#(string or_null * int64_u) [@raw_ptr]) -> int -> unit = "b"
  "n" [@@noalloc]
|}]

(* Invalid: wrong layout (not a fat pointer). *)
external bad_layout : (int[@raw_ptr]) -> unit = "b" "n" [@@noalloc]
[%%expect{|
Line 1, characters 23-26:
1 | external bad_layout : (int[@raw_ptr]) -> unit = "b" "n" [@@noalloc]
                           ^^^
Error: The "[@raw_ptr]" attribute may only be used on external arguments
       whose layout is "value_or_null & bits64" (a fat pointer).
|}]

(* Invalid: components swapped. *)
external bad_swapped :
  (#(int64_u * bytes)[@raw_ptr]) -> unit = "b" "n" [@@noalloc]
[%%expect{|
Line 2, characters 3-21:
2 |   (#(int64_u * bytes)[@raw_ptr]) -> unit = "b" "n" [@@noalloc]
       ^^^^^^^^^^^^^^^^^^
Error: The "[@raw_ptr]" attribute may only be used on external arguments
       whose layout is "value_or_null & bits64" (a fat pointer).
|}]

(* Invalid: on the result. *)
external bad_result : unit -> (#(bytes * int64_u)[@raw_ptr]) = "b" "n" [@@noalloc]
[%%expect{|
Line 1, characters 31-49:
1 | external bad_result : unit -> (#(bytes * int64_u)[@raw_ptr]) = "b" "n" [@@noalloc]
                                   ^^^^^^^^^^^^^^^^^^
Error: The "[@raw_ptr]" attribute may only be used on external arguments
       whose layout is "value_or_null & bits64" (a fat pointer).
|}]

(* Invalid: not noalloc. *)
external bad_alloc : (#(bytes * int64_u)[@raw_ptr]) -> unit = "b" "n"
[%%expect{|
Line 1, characters 0-69:
1 | external bad_alloc : (#(bytes * int64_u)[@raw_ptr]) -> unit = "b" "n"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The "[@raw_ptr]" attribute may only be used on "[@@noalloc]" primitives.
|}]

(* Invalid: no separate native name. *)
external bad_no_native : (#(bytes * int64_u)[@raw_ptr]) -> unit = "b" [@@noalloc]
[%%expect{|
Line 1, characters 0-81:
1 | external bad_no_native : (#(bytes * int64_u)[@raw_ptr]) -> unit = "b" [@@noalloc]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       when attributes "[@untagged]" or "[@unboxed]" are present.
|}]

(* Invalid: combined with another representation attribute. *)
external bad_multi :
  (#(bytes * int64_u)[@raw_ptr] [@unpacked]) -> unit = "b" "n" [@@noalloc]
[%%expect{|
Line 2, characters 34-42:
2 |   (#(bytes * int64_u)[@raw_ptr] [@unpacked]) -> unit = "b" "n" [@@noalloc]
                                      ^^^^^^^^
Error: Too many "[@@unboxed]"/"[@@untagged]"/"[@@unpacked]"/"[@@raw_ptr]" attributes
|}]
