(* TEST
 flags = "-extension small_numbers";
 expect;
*)

(* [nativeint#], [int32#], [int64#], and [float32#] were removed in favor of
   the standalone types [nativeint_u], [int32_u], [int64_u], and
   [float32_u]: the boxed versions are custom blocks, so they should not have
   unboxed versions. *)

type t = nativeint#
[%%expect{|
Line 1, characters 9-19:
1 | type t = nativeint#
             ^^^^^^^^^^
Error: The type "nativeint" has no unboxed version.
Hint: Did you mean "nativeint_u"?
|}]

type t = int32#
[%%expect{|
Line 1, characters 9-15:
1 | type t = int32#
             ^^^^^^
Error: The type "int32" has no unboxed version.
Hint: Did you mean "int32_u"?
|}]

type t = int64#
[%%expect{|
Line 1, characters 9-15:
1 | type t = int64#
             ^^^^^^
Error: The type "int64" has no unboxed version.
Hint: Did you mean "int64_u"?
|}]

type t = float32#
[%%expect{|
Line 1, characters 9-17:
1 | type t = float32#
             ^^^^^^^^
Error: The type "float32" has no unboxed version.
Hint: Did you mean "float32_u"?
|}]

type nativeint_u_kind : word = nativeint_u
type int32_u_kind : bits32 = int32_u
type int64_u_kind : bits64 = int64_u
type float32_u_kind : float32 = float32_u
[%%expect{|
type nativeint_u_kind = nativeint_u
type int32_u_kind = int32_u
type int64_u_kind = int64_u
type float32_u_kind = float32_u
|}]
