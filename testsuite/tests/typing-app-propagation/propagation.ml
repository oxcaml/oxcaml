(* TEST
 expect;
*)

(* Tests for propagation of the expected type into function applications
   (imported from https://github.com/ocaml/ocaml/pull/285). *)

(* Constructor disambiguation through a polymorphic function. *)

type t = A | B
type s = A | C

let id x = x

let _ = (id A : t), (id A : s)
[%%expect{|
type t = A | B
type s = A | C
val id : 'a -> 'a = <fun>
- : t * s = (A, A)
|}, Principal{|
type t = A | B
type s = A | C
val id : 'a -> 'a = <fun>
Line 6, characters 9-13:
6 | let _ = (id A : t), (id A : s)
             ^^^^
Error: This expression has type "s" but an expression was expected of type "t"
|}]

(* Constructor disambiguation into the argument of a higher-order
   function. *)

type bar = Bar of int
type baz = Bar of string

let bars (xs : int list) : bar list = List.map (fun x -> Bar x) xs
[%%expect{|
type bar = Bar of int
type baz = Bar of string
val bars : int list -> bar list = <fun>
|}, Principal{|
type bar = Bar of int
type baz = Bar of string
Line 4, characters 64-66:
4 | let bars (xs : int list) : bar list = List.map (fun x -> Bar x) xs
                                                                    ^^
Error: The value "xs" has type "int list" but an expression was expected of type
         "string list"
       Type "int" is not compatible with type "string"
|}]

(* The same, through [|>] and [@@]. *)

let bars_rev_app (xs : int list) : bar list =
  xs |> List.map (fun x -> Bar x)
[%%expect{|
val bars_rev_app : int list -> bar list = <fun>
|}, Principal{|
Line 2, characters 2-4:
2 |   xs |> List.map (fun x -> Bar x)
      ^^
Error: The value "xs" has type "int list" but an expression was expected of type
         "string list"
       Type "int" is not compatible with type "string"
|}]

let bars_app (xs : int list) : bar list =
  List.map (fun x -> Bar x) @@ xs
[%%expect{|
val bars_app : int list -> bar list = <fun>
|}, Principal{|
Line 2, characters 31-33:
2 |   List.map (fun x -> Bar x) @@ xs
                                   ^^
Error: The value "xs" has type "int list" but an expression was expected of type
         "string list"
       Type "int" is not compatible with type "string"
|}]

(* Record field disambiguation: arguments are still typed left to right, so
   [r.x] is resolved before [l] is seen and the propagated [int] can only
   move the error onto the field access. *)

type t1 = {x: int}
type t2 = {x: bool}

let f (l : t1 list) : int list = List.map (fun r -> r.x) l
[%%expect{|
type t1 = { x : int; }
type t2 = { x : bool; }
Line 4, characters 52-55:
4 | let f (l : t1 list) : int list = List.map (fun r -> r.x) l
                                                        ^^^
Error: The field access "r.x" has type "bool"
       but an expression was expected of type "int"
|}, Principal{|
type t1 = { x : int; }
type t2 = { x : bool; }
Line 4, characters 57-58:
4 | let f (l : t1 list) : int list = List.map (fun r -> r.x) l
                                                             ^
Error: The value "l" has type "t1 list" but an expression was expected of type
         "t2 list"
       Type "t1" is not compatible with type "t2"
|}]

(* Record literal disambiguated by the expected result type. *)

let recs (xs : int list) : t1 list = List.map (fun x -> {x}) xs
[%%expect{|
val recs : int list -> t1 list = <fun>
|}, Principal{|
Line 1, characters 61-63:
1 | let recs (xs : int list) : t1 list = List.map (fun x -> {x}) xs
                                                                 ^^
Error: The value "xs" has type "int list" but an expression was expected of type
         "bool list"
       Type "int" is not compatible with type "bool"
|}]

(* Propagation into a partial application: the expected type includes
   the arrows for the omitted arguments. *)

let const x ~y:_ = x

let g : y:unit -> t = const A
[%%expect{|
val const : 'a -> y:'b -> 'a = <fun>
val g : y:unit -> t = <fun>
|}, Principal{|
val const : 'a -> y:'b -> 'a = <fun>
Line 3, characters 22-29:
3 | let g : y:unit -> t = const A
                          ^^^^^^^
Error: This expression has type "y:unit -> s"
       but an expression was expected of type "y:unit -> t"
       Type "s" is not compatible with type "t"
|}]

(* Propagation from a function's result type is not principal: [k] admits
   both ['a -> 'a -> 'a] and ['a -> 'b -> 'a]
   (garrigue's example from the upstream discussion). *)

let f =
  let k x _ = x in
  fun a b -> (k {x=a} {x=b} : t1)
[%%expect{|
val f : int -> bool -> t1 = <fun>
|}, Principal{|
Line 3, characters 14-27:
3 |   fun a b -> (k {x=a} {x=b} : t1)
                  ^^^^^^^^^^^^^
Error: This expression has type "t2" but an expression was expected of type "t1"
|}]

(* Object-typed arguments (let-def's js_of_ocaml-style example): the method
   is resolved through the propagated type, and when it is missing the error
   should point at the argument rather than the whole application. *)

type 'a signal = Signal of 'a
let signal a = Signal a

class type showable = object
  method show : string
end

class type container = object
  method on_update : (showable -> unit) signal -> unit
end

let f (c : container) =
  c#on_update (signal (fun x -> print_endline x#show))
[%%expect{|
type 'a signal = Signal of 'a
val signal : 'a -> 'a signal = <fun>
class type showable = object method show : string end
class type container =
  object method on_update : (showable -> unit) signal -> unit end
val f : container -> unit = <fun>
|}]

let f (c : container) =
  c#on_update (signal (fun x -> print_endline x#to_string))
[%%expect{|
Line 2, characters 46-47:
2 |   c#on_update (signal (fun x -> print_endline x#to_string))
                                                  ^
Error: This expression has type "showable"
       It has no method "to_string"
|}, Principal{|
Line 2, characters 14-59:
2 |   c#on_update (signal (fun x -> print_endline x#to_string))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(< to_string : string; .. > -> unit) signal"
       but an expression was expected of type "(showable -> unit) signal"
       Type "< to_string : string; .. >" is not compatible with type
         "showable" = "< show : string >"
       The second object type has no method "to_string"
|}]

(* Check the constructor result's portability after [x] fixes its payload. *)

type 'a box = Box of 'a
let require_portable : ('a : value mod portable). 'a -> unit = fun _ -> ()

let f (x : int) = require_portable (id (Box x))
[%%expect{|
type 'a box = Box of 'a
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
val f : int -> unit = <fun>
|}, Principal{|
type 'a box = Box of 'a
val require_portable : ('a : value mod portable). 'a -> unit = <fun>
Line 4, characters 35-47:
4 | let f (x : int) = require_portable (id (Box x))
                                       ^^^^^^^^^^^^
Error: This expression has type "int box"
       but an expression was expected of type "('a : value mod portable)"
       The kind of int box is immutable_data with int
         because of the definition of box at line 1, characters 0-23.
       But the kind of int box must be a subkind of value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

(* The expected variable can also be hidden behind a type abbreviation. *)

type 'a identity = 'a
let require_portable_alias : ('a : value mod portable).
    'a identity -> unit = fun _ -> ()

let f (x : int) = require_portable_alias (id (Box x))
[%%expect{|
type 'a identity = 'a
val require_portable_alias : ('a : value mod portable). 'a identity -> unit =
  <fun>
val f : int -> unit = <fun>
|}, Principal{|
type 'a identity = 'a
val require_portable_alias : ('a : value mod portable). 'a identity -> unit =
  <fun>
Line 5, characters 41-53:
5 | let f (x : int) = require_portable_alias (id (Box x))
                                             ^^^^^^^^^^^^
Error: This expression has type "int box"
       but an expression was expected of type
         "'a identity" = "('a : value mod portable)"
       The kind of int box is immutable_data with int
         because of the definition of box at line 1, characters 0-23.
       But the kind of int box must be a subkind of value mod portable
         because of the definition of require_portable_alias at line 2, characters 4-26.
|}]

(* The same obligation can occur inside an injective type constructor. *)

let require_portable_list : ('a : value mod portable).
    'a list -> unit = fun _ -> ()

let f (x : int) = require_portable_list (id [Box x])
[%%expect{|
val require_portable_list : ('a : value mod portable). 'a list -> unit =
  <fun>
val f : int -> unit = <fun>
|}, Principal{|
val require_portable_list : ('a : value mod portable). 'a list -> unit =
  <fun>
Line 4, characters 40-52:
4 | let f (x : int) = require_portable_list (id [Box x])
                                            ^^^^^^^^^^^^
Error: This expression has type "int box list"
       but an expression was expected of type "'a list"
       The kind of int box is immutable_data with int
         because of the definition of box at line 1, characters 0-23.
       But the kind of int box must be a subkind of value mod portable
         because of the definition of require_portable_list at line 1, characters 4-25.
|}]

let require_portable_fst : ('a : value mod portable). 'a * 'b -> unit = fun _ -> ()

let f (x : int) = require_portable_fst (id (Box x, ()))
[%%expect{|
val require_portable_fst : ('a : value mod portable) 'b. 'a * 'b -> unit =
  <fun>
val f : int -> unit = <fun>
|}, Principal{|
val require_portable_fst : ('a : value mod portable) 'b. 'a * 'b -> unit =
  <fun>
Line 3, characters 39-55:
3 | let f (x : int) = require_portable_fst (id (Box x, ()))
                                           ^^^^^^^^^^^^^^^^
Error: This expression has type "int box * unit"
       but an expression was expected of type "'a * 'b"
       The kind of int box is immutable_data with int
         because of the definition of box at line 1, characters 0-23.
       But the kind of int box must be a subkind of value mod portable
         because of the definition of require_portable_fst at line 1, characters 4-24.
|}]

(* Propagation must not let a local GADT equation escape. *)

type _ g = Int : int g
let ky x y = ignore (x = y); x

let test : type a. a g -> _ = function Int -> ky (1 : a) 1
[%%expect{|
type _ g = Int : int g
val ky : 'a -> 'a -> 'a = <fun>
Line 4, characters 57-58:
4 | let test : type a. a g -> _ = function Int -> ky (1 : a) 1
                                                             ^
Error: The constant "1" has type "int" but an expression was expected of type
         "a" = "int"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
|}, Principal{|
type _ g = Int : int g
val ky : 'a -> 'a -> 'a = <fun>
Line 4, characters 46-58:
4 | let test : type a. a g -> _ = function Int -> ky (1 : a) 1
                                                  ^^^^^^^^^^^^
Error: This expression has type "a" = "int"
       but an expression was expected of type "'a"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
|}]

let test2 : type a. a g -> _ = function Int -> if true then (1 : a) else 1
[%%expect{|
Line 1, characters 73-74:
1 | let test2 : type a. a g -> _ = function Int -> if true then (1 : a) else 1
                                                                             ^
Error: The constant "1" has type "int" but an expression was expected of type
         "a" = "int"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
|}]


(* A payload can resolve variables inside an abstract injective with-bound. *)

module Hidden : sig
  type !'a t : immutable_data with 'a
  val ints : int t
end = struct
  type 'a t = 'a list
  let ints = [1]
end

type 'a wrapped = Wrap of 'a Hidden.t
[%%expect{|
module Hidden : sig type !'a t : immutable_data with 'a val ints : int t end
type 'a wrapped = Wrap of 'a Hidden.t
|}]

let direct = require_portable (Wrap Hidden.ints)
[%%expect{|
val direct : unit = ()
|}, Principal{|
Line 1, characters 30-48:
1 | let direct = require_portable (Wrap Hidden.ints)
                                  ^^^^^^^^^^^^^^^^^^
Error:
       The kind of int wrapped is immutable_data with int Hidden.t
         because of the definition of wrapped at line 9, characters 0-37.
       But the kind of int wrapped must be a subkind of value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

let through_id = require_portable (id (Wrap Hidden.ints))
[%%expect{|
val through_id : unit = ()
|}, Principal{|
Line 1, characters 34-57:
1 | let through_id = require_portable (id (Wrap Hidden.ints))
                                      ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int wrapped"
       but an expression was expected of type "('a : value mod portable)"
       The kind of int wrapped is immutable_data with int Hidden.t
         because of the definition of wrapped at line 9, characters 0-37.
       But the kind of int wrapped must be a subkind of value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

(* Delaying a check must not discard it or use equations local to a payload. *)

let nonportable (x : (int -> int) Hidden.t) = require_portable (id (Wrap x))
[%%expect{|
Line 1, characters 67-75:
1 | let nonportable (x : (int -> int) Hidden.t) = require_portable (id (Wrap x))
                                                                       ^^^^^^^^
Error:
       The kind of (int -> int) wrapped is
           immutable_data with (int -> int) Hidden.t
         because of the definition of wrapped at line 9, characters 0-37.
       But the kind of (int -> int) wrapped must be a subkind of
           value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}, Principal{|
Line 1, characters 63-76:
1 | let nonportable (x : (int -> int) Hidden.t) = require_portable (id (Wrap x))
                                                                   ^^^^^^^^^^^^^
Error: This expression has type "(int -> int) wrapped"
       but an expression was expected of type "('a : value mod portable)"
       The kind of (int -> int) wrapped is
           immutable_data with (int -> int) Hidden.t
         because of the definition of wrapped at line 9, characters 0-37.
       But the kind of (int -> int) wrapped must be a subkind of
           value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

let unresolved x = require_portable (id (Wrap x))
[%%expect{|
Line 1, characters 40-48:
1 | let unresolved x = require_portable (id (Wrap x))
                                            ^^^^^^^^
Error:
       The kind of 'a wrapped is immutable_data with 'a Hidden.t
         because of the definition of wrapped at line 9, characters 0-37.
       But the kind of 'a wrapped must be a subkind of value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}, Principal{|
Line 1, characters 36-49:
1 | let unresolved x = require_portable (id (Wrap x))
                                        ^^^^^^^^^^^^^
Error: This expression has type "'a wrapped"
       but an expression was expected of type "('b : value mod portable)"
       The kind of 'a wrapped is immutable_data with 'a Hidden.t
         because of the definition of wrapped at line 9, characters 0-37.
       But the kind of 'a wrapped must be a subkind of value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

type _ witness = Int : int witness | Function : (int -> int) witness

let local_equations : type a. a witness -> a Hidden.t -> unit =
  fun witness x ->
    require_portable
      (Wrap (match witness with Int -> x | Function -> x))
[%%expect{|
type _ witness = Int : int witness | Function : (int -> int) witness
Line 6, characters 6-58:
6 |       (Wrap (match witness with Int -> x | Function -> x))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The kind of a wrapped is immutable_data with a Hidden.t
         because of the definition of wrapped at line 9, characters 0-37.
       But the kind of a wrapped must be a subkind of value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

(* A failed check must not affect later expressions. *)

let after_failure = require_portable (id (Wrap Hidden.ints))
[%%expect{|
val after_failure : unit = ()
|}, Principal{|
Line 1, characters 37-60:
1 | let after_failure = require_portable (id (Wrap Hidden.ints))
                                         ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int wrapped"
       but an expression was expected of type "('a : value mod portable)"
       The kind of int wrapped is immutable_data with int Hidden.t
         because of the definition of wrapped at line 9, characters 0-37.
       But the kind of int wrapped must be a subkind of value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

(* Tuples and arrays also check their result kinds after their operands. *)

let tuple (x : int) = require_portable (id (x, x))
[%%expect{|
val tuple : int -> unit = <fun>
|}, Principal{|
Line 1, characters 39-50:
1 | let tuple (x : int) = require_portable (id (x, x))
                                           ^^^^^^^^^^^
Error: This expression has type "int * int"
       but an expression was expected of type "('a : value mod portable)"
       The kind of int * int is immutable_data with int
         because it's a tuple type.
       But the kind of int * int must be a subkind of value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

let mutable_array (x : int) = require_portable (id [|x|])
[%%expect{|
val mutable_array : int -> unit = <fun>
|}, Principal{|
Line 1, characters 47-57:
1 | let mutable_array (x : int) = require_portable (id [|x|])
                                                   ^^^^^^^^^^
Error: This expression has type "int array"
       but an expression was expected of type "('a : value mod portable)"
       The kind of int array is mutable_data with int
         because it is the primitive value type array.
       But the kind of int array must be a subkind of value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

let immutable_array (x : int) = require_portable (id [:x:])
[%%expect{|
val immutable_array : int -> unit = <fun>
|}, Principal{|
Line 1, characters 49-59:
1 | let immutable_array (x : int) = require_portable (id [:x:])
                                                     ^^^^^^^^^^
Error: This expression has type "int iarray"
       but an expression was expected of type "('a : value mod portable)"
       The kind of int iarray is immutable_data with int
         because it is the primitive value type iarray.
       But the kind of int iarray must be a subkind of value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

let id_product : ('a : value & value). 'a -> 'a = fun x -> x
let require_portable_product : ('a : value & value mod portable).
  'a -> unit = fun _ -> ()
let unboxed_tuple (x : int) = require_portable_product (id_product #(x, x))
[%%expect{|
val id_product : ('a : value & value). 'a -> 'a = <fun>
val require_portable_product :
  ('a : value mod portable & value mod portable). 'a -> unit = <fun>
val unboxed_tuple : int -> unit = <fun>
|}]

let nonportable_tuple (f : int -> int) = require_portable (id (1, f))
[%%expect{|
Line 1, characters 62-68:
1 | let nonportable_tuple (f : int -> int) = require_portable (id (1, f))
                                                                  ^^^^^^
Error:
       The kind of int * (int -> int) is value non_float mod immutable
         because it's a tuple type.
       But the kind of int * (int -> int) must be a subkind of
           value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}, Principal{|
Line 1, characters 58-69:
1 | let nonportable_tuple (f : int -> int) = require_portable (id (1, f))
                                                              ^^^^^^^^^^^
Error: This expression has type "int * (int -> int)"
       but an expression was expected of type "('a : value mod portable)"
       The kind of int * (int -> int) is
           value non_float mod immutable with int
         because it's a tuple type.
       But the kind of int * (int -> int) must be a subkind of
           value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

let nonportable_array (f : int -> int) = require_portable (id [|f|])
[%%expect{|
Line 1, characters 62-67:
1 | let nonportable_array (f : int -> int) = require_portable (id [|f|])
                                                                  ^^^^^
Error:
       The kind of (int -> int) array is value non_float
         because it is the primitive value type array.
       But the kind of (int -> int) array must be a subkind of
           value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}, Principal{|
Line 1, characters 58-68:
1 | let nonportable_array (f : int -> int) = require_portable (id [|f|])
                                                              ^^^^^^^^^^
Error: This expression has type "(int -> int) array"
       but an expression was expected of type "('a : value mod portable)"
       The kind of (int -> int) array is mutable_data with int -> int
         because it is the primitive value type array.
       But the kind of (int -> int) array must be a subkind of
           value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

let nonportable_iarray (f : int -> int) = require_portable (id [:f:])
[%%expect{|
Line 1, characters 63-68:
1 | let nonportable_iarray (f : int -> int) = require_portable (id [:f:])
                                                                   ^^^^^
Error:
       The kind of (int -> int) iarray is value non_float mod immutable
         because it is the primitive value type iarray.
       But the kind of (int -> int) iarray must be a subkind of
           value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}, Principal{|
Line 1, characters 59-69:
1 | let nonportable_iarray (f : int -> int) = require_portable (id [:f:])
                                                               ^^^^^^^^^^
Error: This expression has type "(int -> int) iarray"
       but an expression was expected of type "('a : value mod portable)"
       The kind of (int -> int) iarray is immutable_data with int -> int
         because it is the primitive value type iarray.
       But the kind of (int -> int) iarray must be a subkind of
           value mod portable
         because of the definition of require_portable at line 2, characters 4-20.
|}]

let nonportable_product (f : int -> int) =
  require_portable_product (id_product #(1, f))
[%%expect{|
Line 2, characters 39-46:
2 |   require_portable_product (id_product #(1, f))
                                           ^^^^^^^
Error:
       The kind of #(int * (int -> int)) is
           value non_pointer mod aliased immutable
           & value non_float mod aliased immutable
         because it is an unboxed tuple.
       But the kind of #(int * (int -> int)) must be a subkind of
           value mod portable & value mod portable
         because of the definition of require_portable_product at line 2, characters 4-28.
|}, Principal{|
Line 2, characters 27-47:
2 |   require_portable_product (id_product #(1, f))
                               ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "#(int * (int -> int))"
       but an expression was expected of type
         "('a : value mod portable & value mod portable)"
       The kind of #(int * (int -> int)) is
           immediate mod dynamic with int with int -> int
           & value mod everything
               non_float
               mod dynamic
               with int
               with int -> int
         because it is an unboxed tuple.
       But the kind of #(int * (int -> int)) must be a subkind of
           value mod portable & value mod portable
         because of the definition of require_portable_product at line 2, characters 4-28.
|}]
