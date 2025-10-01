(* TEST
 flags += "-ikinds";
 expect;
*)
type constant =
    Const_int of int
  | Const_char of char
  | Const_untagged_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_float32 of string
  | Const_unboxed_float of string
  | Const_unboxed_float32 of string
  | Const_int8 of int
  | Const_int16 of int
  | Const_int32 of int32
  | Const_int64 of int64
  (* CR mshinwell: This should use [Targetint.t] not [nativeint] *)
  | Const_nativeint of nativeint
  | Const_untagged_int of int
  | Const_untagged_int8 of int
  | Const_untagged_int16 of int
  | Const_unboxed_int32 of int32
  | Const_unboxed_int64 of int64
  | Const_unboxed_nativeint of nativeint
[%%expect{|
type constant =
    Const_int of int
  | Const_char of char
  | Const_untagged_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_float32 of string
  | Const_unboxed_float of string
  | Const_unboxed_float32 of string
  | Const_int8 of int
  | Const_int16 of int
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
  | Const_untagged_int of int
  | Const_untagged_int8 of int
  | Const_untagged_int16 of int
  | Const_unboxed_int32 of int32
  | Const_unboxed_int64 of int64
  | Const_unboxed_nativeint of nativeint
|}]

type 'a eq_int = Eq : int eq_int
[%%expect{|
type 'a eq_int = Eq : int eq_int
|}]

module M : sig
  type t
  val is_int : t eq_int
end = struct
  type t = int
  let is_int : t eq_int = Eq
end
[%%expect{|
module M : sig type t val is_int : t eq_int end
|}]

type q = Foo of M.t | Bar of int
[%%expect{|
type q = Foo of M.t | Bar of int
|}]

let takes_only_immutable (x : ('a : immutable_data)) : unit = ()
[%%expect{|
val takes_only_immutable : ('a : immutable_data). 'a -> unit = <fun>
|}]

let foo (x : q) =
  match M.is_int with
  | Eq -> takes_only_immutable x
[%%expect{|
val foo : q -> unit = <fun>
|}]

let bar (x : q) =
  takes_only_immutable x
[%%expect{|
Line 2, characters 23-24:
2 |   takes_only_immutable x
                           ^
Error: This expression has type "q" but an expression was expected of type
         "('a : immutable_data)"
       The kind of q is immutable_data with M.t
         because of the definition of q at line 1, characters 0-32.
       But the kind of q must be a subkind of immutable_data
         because of the definition of takes_only_immutable at line 1, characters 25-64.
|}]
