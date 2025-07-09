(* TEST
 expect;
*)

type ('a : value) t : value_or_null = 'a or_null [@@or_null_reexport]

[%%expect{|
type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport]
|}]

let to_option (x : 'a or_null) =
  match x with
  | Null -> None
  | This x -> Some x

[%%expect{|
val to_option : 'a or_null -> 'a option = <fun>
|}]

let of_option (x : 'a option) =
  match x with
  | None -> Null
  | Some x -> This x

[%%expect{|
val of_option : 'a option -> 'a t = <fun>
|}]

let pi = This 3.14

[%%expect{|
val pi : float t = This 3.14
|}]

let pi' =
  let value = This 3.14 in
  value

[%%expect{|
val pi' : float t = This 3.14
|}]

type myrec = { x : int; y : int or_null }

[%%expect{|
type myrec = { x : int; y : int or_null; }
|}]

let fst { x; y } = x

[%%expect{|
val fst : myrec -> int = <fun>
|}]

let snd { x; y } = y

[%%expect{|
val snd : myrec -> int or_null = <fun>
|}]

let snd' (a : myrec) = a.y

[%%expect{|
val snd' : myrec -> int or_null = <fun>
|}]

let mk n = { x = n; y = This n }

[%%expect{|
val mk : int -> myrec = <fun>
|}]

let test =
  let a = mk 4 in
  let a' = { a with y = Null } in
  a'.y

[%%expect{|
val test : int or_null = Null
|}]

let mytup = (4, This 5)

[%%expect{|
val mytup : int * int t = (4, This 5)
|}]

type mytup' = int * int t

[%%expect{|
type mytup' = int * int t
|}]

type nested = int or_null or_null

[%%expect{|
Line 1, characters 14-25:
1 | type nested = int or_null or_null
                  ^^^^^^^^^^^
Error: This type "int or_null" should be an instance of type
         "('a : value_or_null mod non_null)"
       The kind of int or_null is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of int or_null must be a subkind of
           value_or_null mod non_null
         because the type argument of or_null has kind value.
|}]

let should_fail = This (This 5)

[%%expect{|
Line 1, characters 23-31:
1 | let should_fail = This (This 5)
                           ^^^^^^^^
Error: This expression has type "'a t" = "'a or_null"
       but an expression was expected of type "('b : value)"
       The kind of 'a t is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of 'a t must be a subkind of value
         because of the definition of t at line 1, characters 0-69.
|}]

let should_also_fail = This Null

[%%expect{|
Line 1, characters 28-32:
1 | let should_also_fail = This Null
                                ^^^^
Error: This expression has type "'a t" = "'a or_null"
       but an expression was expected of type "('b : value)"
       The kind of 'a t is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of 'a t must be a subkind of value
         because of the definition of t at line 1, characters 0-69.
|}]

let mk' n = `Foo (This n)

[%%expect{|
val mk' : 'a -> [> `Foo of 'a t ] = <fun>
|}]

module type S = sig
  type a = float or_null

  val x : a
  val f : float -> a
  val g : a -> float
end

[%%expect{|
module type S =
  sig
    type a = float or_null
    val x : a
    val f : float -> a
    val g : a -> float
  end
|}]

module M : S with type a = float t = struct
  type a = float or_null

  let x = This 3.14
  let f x = This x
  let g = function
    | This x -> x
    | Null -> 0.
end

[%%expect{|
module M :
  sig type a = float t val x : a val f : float -> a val g : a -> float end
|}]

external this : 'a -> 'a or_null = "%identity"

[%%expect{|
external this : 'a -> 'a or_null = "%identity"
|}]

external unsafe_get : 'a or_null -> 'a = "%identity"

[%%expect{|
external unsafe_get : 'a or_null -> 'a = "%identity"
|}]

let should_fail = [| Null; This 5 |]

[%%expect{|
Line 1, characters 21-25:
1 | let should_fail = [| Null; This 5 |]
                         ^^^^
Error: This expression has type "'a t" = "'a or_null"
       but an expression was expected of type "('b : value)"
       The kind of 'a t is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of 'a t must be a subkind of value
         because it's the type of an array element,
         chosen to have kind value.
|}]

type should_fail = float or_null array

[%%expect{|
Line 1, characters 19-32:
1 | type should_fail = float or_null array
                       ^^^^^^^^^^^^^
Error: This type "float or_null" should be an instance of type
         "('a : any_non_null)"
       The kind of float or_null is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of float or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

let null_list = [ Null; This 5 ]

[%%expect{|
val null_list : int t list = [Null; This 5]
|}]

type null_list = float or_null list

[%%expect{|
type null_list = float or_null list
|}]

(* Immutable arrays should work the same as mutable: *)

let should_fail = [: Null; This 5 :]

[%%expect{|
Line 1, characters 21-25:
1 | let should_fail = [: Null; This 5 :]
                         ^^^^
Error: This expression has type "'a t" = "'a or_null"
       but an expression was expected of type "('b : value)"
       The kind of 'a t is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of 'a t must be a subkind of value
         because it's the type of an array element,
         chosen to have kind value.
|}]

type should_fail = float or_null array

[%%expect{|
Line 1, characters 19-32:
1 | type should_fail = float or_null array
                       ^^^^^^^^^^^^^
Error: This type "float or_null" should be an instance of type
         "('a : any_non_null)"
       The kind of float or_null is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of float or_null must be a subkind of any_non_null
         because it's the type argument to the array type.
|}]

(* CR layouts v3: object fields should accept null, but it's low priority. *)
type object_with_null = < x : int or_null; .. >

[%%expect{|
Line 1, characters 26-42:
1 | type object_with_null = < x : int or_null; .. >
                              ^^^^^^^^^^^^^^^^
Error: Object field types must have layout value.
       The kind of "int or_null" is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of "int or_null" must be a subkind of value
         because it's the type of an object field.
|}]

(* CR layouts v3: instance variables should accept null, but it's low priority. *)
class a_with_null =
  object
    val x = Null
  end

[%%expect{|
Line 3, characters 8-9:
3 |     val x = Null
            ^
Error: Variables bound in a class must have layout value.
       The kind of x is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of x must be a subkind of value
         because it's the type of a class field.
|}]

(* just checking printing *)
type t_any_non_null : any_non_null

[%%expect{|
type t_any_non_null : any_non_null
|}]

(* [or_null] in unboxed types *)

type unboxed_rec = { field : int or_null } [@@unboxed]

[%%expect{|
type unboxed_rec = { field : int or_null; } [@@unboxed]
|}]

let unboxed_null = { field = Null }

[%%expect{|
val unboxed_null : unboxed_rec = {field = Null}
|}]

let unboxed_some = { field = This 42 }

[%%expect{|
val unboxed_some : unboxed_rec = {field = This 42}
|}]

let get_field (r : unboxed_rec) = r.field

[%%expect{|
val get_field : unboxed_rec -> int or_null = <fun>
|}]

type unboxed_var = Wrap of int or_null [@@unboxed]

[%%expect{|
type unboxed_var = Wrap of int or_null [@@unboxed]
|}]

let var_null = Wrap Null

[%%expect{|
val var_null : unboxed_var = <unknown constructor>
|}]

let var_some = Wrap (This 99)

[%%expect{|
val var_some : unboxed_var = <unknown constructor>
|}]

let unwrap = function
  | Wrap x -> x

[%%expect{|
val unwrap : unboxed_var -> int or_null = <fun>
|}]

type (_, _) fail = Fail : 'a or_null -> ('a, 'a or_null) fail [@@unboxed]
[%%expect{|
Line 1, characters 45-55:
1 | type (_, _) fail = Fail : 'a or_null -> ('a, 'a or_null) fail [@@unboxed]
                                                 ^^^^^^^^^^
Error: This type "'a or_null" should be an instance of type "('b : value)"
       The kind of 'a or_null is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of 'a or_null must be a subkind of value
         because it instantiates an unannotated type parameter of fail,
         chosen to have kind value.
|}]

type (_, _ : value_or_null) gadt = Gadt : 'a or_null -> ('a, 'a or_null) gadt [@@unboxed]

[%%expect{|
type (_, _) gadt = Gadt : 'a or_null -> ('a, 'a or_null) gadt [@@unboxed]
|}]

let gadt_null = Gadt Null

[%%expect{|
val gadt_null : ('a, 'a or_null) gadt = <unknown constructor>
|}]

let gadt_some = Gadt (This 42)

[%%expect{|
val gadt_some : (int, int or_null) gadt = <unknown constructor>
|}]

let unwrap_gadt : type a. (a, a or_null) gadt -> a or_null = function
  | Gadt x -> x

[%%expect{|
val unwrap_gadt : ('a, 'a or_null) gadt -> 'a or_null = <fun>
|}]

let should_fail_unboxed_rec = This { field = Null }

[%%expect{|
Line 1, characters 35-51:
1 | let should_fail_unboxed_rec = This { field = Null }
                                       ^^^^^^^^^^^^^^^^
Error: This expression has type "unboxed_rec"
       but an expression was expected of type "('a : value)"
       The kind of unboxed_rec is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of unboxed_rec must be a subkind of value
         because of the definition of t at line 1, characters 0-69.
|}]

let should_fail_unboxed_var = This (Wrap Null)

[%%expect{|
Line 1, characters 35-46:
1 | let should_fail_unboxed_var = This (Wrap Null)
                                       ^^^^^^^^^^^
Error: This expression has type "unboxed_var"
       but an expression was expected of type "('a : value)"
       The kind of unboxed_var is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of unboxed_var must be a subkind of value
         because of the definition of t at line 1, characters 0-69.
|}]

let should_fail_unboxed_gadt = This (Gadt Null)

[%%expect{|
Line 1, characters 36-47:
1 | let should_fail_unboxed_gadt = This (Gadt Null)
                                        ^^^^^^^^^^^
Error: This expression has type "('a, 'a or_null) gadt"
       but an expression was expected of type "('b : value)"
       The kind of ('a, 'a or_null) gadt is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of ('a, 'a or_null) gadt must be a subkind of value
         because of the definition of t at line 1, characters 0-69.
|}]
