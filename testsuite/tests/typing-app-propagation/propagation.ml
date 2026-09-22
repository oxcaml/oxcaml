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
Line 1, characters 61-63:
1 | let recs (xs : int list) : t1 list = List.map (fun x -> {x}) xs
                                                                 ^^
Error: The value "xs" has type "int list" but an expression was expected of type
         "bool list"
       Type "int" is not compatible with type "bool"
|}]

(* Propagation into a partial application: the expected type includes
   the arrows for the remaining arguments. *)

let const x ~y:_ = x

let g : y:unit -> t = const A
[%%expect{|
val const : 'a -> y:'b -> 'a = <fun>
Line 3, characters 22-29:
3 | let g : y:unit -> t = const A
                          ^^^^^^^
Error: This expression has type "y:unit -> s"
       but an expression was expected of type "y:unit -> t"
       Type "s" is not compatible with type "t"
|}]

(* Omit a labelled argument before a supplied argument. *)

let const_omitted ~y:_ x = x
let g : y:unit -> t = const_omitted A
[%%expect{|
val const_omitted : y:'a -> 'b -> 'b = <fun>
Line 2, characters 22-37:
2 | let g : y:unit -> t = const_omitted A
                          ^^^^^^^^^^^^^^^
Error: This expression has type "y:unit -> s"
       but an expression was expected of type "y:unit -> t"
       Type "s" is not compatible with type "t"
|}]

(* An expected unlabelled arrow still takes the inference path, which must
   allow optional-argument elimination before checking the result type. *)

let const_unlabelled x () = x
let h : unit -> t = const_unlabelled A
[%%expect{|
val const_unlabelled : 'a -> unit -> 'a = <fun>
Line 2, characters 20-38:
2 | let h : unit -> t = const_unlabelled A
                        ^^^^^^^^^^^^^^^^^^
Error: This expression has type "unit -> s"
       but an expression was expected of type "unit -> t"
       Type "s" is not compatible with type "t"
|}]

let with_optional x ?(opt = 0) () = x + opt
let h : unit -> int = with_optional 1
let () = assert (h () = 1)
[%%expect{|
val with_optional : int -> ?opt:int -> unit -> int = <fun>
val h : unit -> int = <fun>
|}]

(* An omitted optional parameter is part of the propagated result type. *)

let const_optional ?x ~y () = y
let h : ?x:int -> unit -> t = const_optional ~y:A
[%%expect{|
val const_optional : ?x:'a -> y:'b -> unit -> 'b = <fun>
Line 2, characters 30-49:
2 | let h : ?x:int -> unit -> t = const_optional ~y:A
                                  ^^^^^^^^^^^^^^^^^^^
Error: This expression has type "?x:int -> unit -> s"
       but an expression was expected of type "?x:int -> unit -> t"
       Type "s" is not compatible with type "t"
|}]

(* Propagating through a pair can expose an expected arrow for an argument,
   allowing the usual optional-argument coercion. *)

let with_default ?(opt = 0) x = opt + x
[%%expect{|
val with_default : ?opt:int -> int -> int = <fun>
|}]

let h : (int -> int) * int = (fun x -> x, 0) with_default
let () = assert (fst h 2 = 2)
[%%expect{|
Line 1, characters 29-57:
1 | let h : (int -> int) * int = (fun x -> x, 0) with_default
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(?opt:int -> int -> int) * int"
       but an expression was expected of type "(int -> int) * int"
       The first argument is labeled "?opt",
       but an unlabeled argument was expected
Hint: This function application is partial, maybe some arguments are missing.
|}]

(* Propagation from a function's result type is not principal: [k] admits
   both ['a -> 'a -> 'a] and ['a -> 'b -> 'a]
   (garrigue's example from the upstream discussion). *)

let f =
  let k x _ = x in
  fun a b -> (k {x=a} {x=b} : t1)
[%%expect{|
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
Line 2, characters 14-59:
2 |   c#on_update (signal (fun x -> print_endline x#to_string))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(< to_string : string; .. > -> unit) signal"
       but an expression was expected of type "(showable -> unit) signal"
       Type "< to_string : string; .. >" is not compatible with type
         "showable" = "< show : string >"
       The second object type has no method "to_string"
|}]

(* Kind constraints propagate through the result before typing [Box x]. *)

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

(* Propagation also reaches constraints nested under lists and tuples. *)

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

(* GADT equation scoping (trefis's example): the result must not retain
   the local equation introduced by matching [Int]. *)

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

(* A phantom alias exported by an abstract functor can discard the existential
   parameter of an argument. A result that depends on it must still be rejected. *)

module type Field = sig type 'a t end
module type Data = sig type 'a t end
module type Map = sig
  module Make (F : Field) (D : Data) : sig
    type t
    module Data : Data with type 'a t = 'a D.t
    val find : t -> 'a F.t -> 'a Data.t
  end
end
[%%expect{|
module type Field = sig type 'a t end
module type Data = sig type 'a t end
module type Map =
  sig
    module Make :
      functor (F : Field) (D : Data) ->
        sig
          type t
          module Data : sig type 'a t = 'a D.t end
          val find : t -> 'a F.t -> 'a Data.t
        end
  end
|}]

module Phantom_result (M : Map) = struct
  module Field = struct
    type _ t = Int : int t
    type packed = T : 'a t -> packed
  end
  module Sql = M.Make (Field) (struct type 'a t = string end)
  let select columns packed =
    let premium c = Sql.find columns c in
    List.map (fun (Field.T col) -> premium col) packed
end
[%%expect{|
module Phantom_result :
  functor (M : Map) ->
    sig
      module Field :
        sig type _ t = Int : int t type packed = T : 'a t -> packed end
      module Sql :
        sig
          type t
          module Data : sig type 'a t = string end
          val find : t -> 'a Field.t -> 'a Data.t
        end
      val select : Sql.t -> Field.packed list -> string list
    end
|}]

module Escape (M : Map) = struct
  module Field = struct
    type _ t = Int : int t
    type packed = T : 'a t -> packed
  end
  module Sql = M.Make (Field) (struct type 'a t = 'a end)
  let select columns packed =
    let premium c = Sql.find columns c in
    List.map (fun (Field.T col) -> premium col) packed
end
[%%expect{|
Line 9, characters 35-46:
9 |     List.map (fun (Field.T col) -> premium col) packed
                                       ^^^^^^^^^^^
Error: This expression has type "$a" but an expression was expected of type "'a"
       The type constructor "$a" would escape its scope
       Hint: "$a" is an existential type bound by the constructor "T".
|}]

let fold : init:'a -> f:('a -> 'a) -> 'a = fun ~init ~f -> f init
let portable : ('a : value mod portable contended). (unit -> 'a) -> unit =
  fun _ -> ()
[%%expect{|
val fold : init:'a -> f:('a -> 'a) -> 'a = <fun>
val portable : ('a : value mod portable contended). (unit -> 'a) -> unit =
  <fun>
|}]

let fold_portable () =
  portable (fun () ->
    fold ~init:(Ok []) ~f:(fun _ -> if true then Ok [1] else Error 0))
[%%expect{|
val fold_portable : unit -> unit = <fun>
|}, Principal{|
Line 3, characters 4-69:
3 |     fold ~init:(Ok []) ~f:(fun _ -> if true then Ok [1] else Error 0))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(int list, int) result"
       but an expression was expected of type
         "('a : value mod portable contended)"
       The kind of (int list, int) result is
           immutable_data with int with int list.
       But the kind of (int list, int) result must be a subkind of
           value mod portable contended
         because of the definition of portable at line 2, characters 4-12.
|}]

let fold_nonportable (f : int -> int) =
  portable (fun () ->
    fold ~init:(Ok []) ~f:(fun _ -> if true then Ok [f] else Error 0))
[%%expect{|
Line 3, characters 4-69:
3 |     fold ~init:(Ok []) ~f:(fun _ -> if true then Ok [f] else Error 0))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "((int -> int) list, int) result"
       but an expression was expected of type
         "('a : value mod portable contended)"
       The kind of ((int -> int) list, int) result is
           value non_float mod immutable.
       But the kind of ((int -> int) list, int) result must be a subkind of
           value mod portable contended
         because of the definition of portable at line 2, characters 4-12.
|}, Principal{|
Line 3, characters 4-69:
3 |     fold ~init:(Ok []) ~f:(fun _ -> if true then Ok [f] else Error 0))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "((int -> int) list, int) result"
       but an expression was expected of type
         "('a : value mod portable contended)"
       The kind of ((int -> int) list, int) result is
           immutable_data with (int -> int) list with int.
       But the kind of ((int -> int) list, int) result must be a subkind of
           value mod portable contended
         because of the definition of portable at line 2, characters 4-12.
|}]


(* Argument order and a nested expected result exercise the same constraints. *)

let fold_reversed : f:('a -> 'a) -> init:'a -> 'a =
  fun ~f ~init -> f init
let portable_pair : ('a : value mod portable contended).
  (unit -> 'a * unit) -> unit = fun _ -> ()

let fold_reversed_portable () =
  portable (fun () ->
    fold_reversed ~f:(fun _ -> if true then Ok [1] else Error 0) ~init:(Ok []))

let fold_nested_portable () =
  portable_pair (fun () ->
    fold ~init:(Ok [], ())
      ~f:(fun _ -> ((if true then Ok [1] else Error 0), ())))
[%%expect{|
val fold_reversed : f:('a -> 'a) -> init:'a -> 'a = <fun>
val portable_pair :
  ('a : value mod portable contended). (unit -> 'a * unit) -> unit = <fun>
val fold_reversed_portable : unit -> unit = <fun>
val fold_nested_portable : unit -> unit = <fun>
|}, Principal{|
val fold_reversed : f:('a -> 'a) -> init:'a -> 'a = <fun>
val portable_pair :
  ('a : value mod portable contended). (unit -> 'a * unit) -> unit = <fun>
Line 8, characters 4-78:
8 |     fold_reversed ~f:(fun _ -> if true then Ok [1] else Error 0) ~init:(Ok []))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(int list, int) result"
       but an expression was expected of type
         "('a : value mod portable contended)"
       The kind of (int list, int) result is
           immutable_data with int with int list.
       But the kind of (int list, int) result must be a subkind of
           value mod portable contended
         because of the definition of portable at line 2, characters 4-12.
|}]
