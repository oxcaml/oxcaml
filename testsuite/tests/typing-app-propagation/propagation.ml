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

(* Record field disambiguation. *)

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

(* Object-typed arguments (let-def's js_of_ocaml-style example): the error
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
  c#on_update (signal (fun x -> print_endline x#to_string))
[%%expect{|
type 'a signal = Signal of 'a
val signal : 'a -> 'a signal = <fun>
class type showable = object method show : string end
class type container =
  object method on_update : (showable -> unit) signal -> unit end
Line 13, characters 46-47:
13 |   c#on_update (signal (fun x -> print_endline x#to_string))
                                                   ^
Error: This expression has type "showable"
       It has no method "to_string"
|}, Principal{|
type 'a signal = Signal of 'a
val signal : 'a -> 'a signal = <fun>
class type showable = object method show : string end
class type container =
  object method on_update : (showable -> unit) signal -> unit end
Line 13, characters 14-59:
13 |   c#on_update (signal (fun x -> print_endline x#to_string))
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(< to_string : string; .. > -> unit) signal"
       but an expression was expected of type "(showable -> unit) signal"
       Type "< to_string : string; .. >" is not compatible with type
         "showable" = "< show : string >"
       The second object type has no method "to_string"
|}]

(* GADT equation scoping (trefis's example): error message comparison. *)

type _ g = Int : int g
let ky x y = ignore (x = y); x

let test : type a. a g -> _ = function Int -> ky (1 : a) 1
[%%expect{|
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
