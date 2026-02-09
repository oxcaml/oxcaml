(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* Test that variables can be given evals-to constraints. *)


(** Basics and examples **)


(* [eval] stub *)
open (struct
  let eval a = Obj.magic a
end : sig
  val eval : 'a expr -> 'a eval
end)
[%%expect{|
val eval : 'a expr -> 'a eval = <fun>
|}]


(* We can type an unannotated eval *)

let print_int (_ : int) = ()

let () = print_int (eval <[ 2 + 2 ]>)
[%%expect{|
val print_int : int -> unit = <fun>
|}]

let () = print_int (eval <[ "foo" ^ "bar" ]>)  (* type error: int ~/~ string *)
[%%expect{|
Line 1, characters 19-45:
1 | let () = print_int (eval <[ "foo" ^ "bar" ]>)  (* type error: int ~/~ string *)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "<[string]> eval" = "string"
       but an expression was expected of type "int"
|}]


(* Identity function. Types are constrained to the image of eval *)

let id () = eval <[ fun x -> x ]>
[%%expect{|
val id : unit -> <[$('a) -> $('a)]> eval = <fun>
|}]

let id' (type a) () : a eval -> a eval = id () (* really polymorphic *)
[%%expect{|
val id' : unit -> 'a eval -> 'a eval = <fun>
|}]


(* Cross-stage type scheme *)

let value_or_eval x e = function
  | true -> x
  | false -> eval e
[%%expect{|
val value_or_eval : 'a eval -> 'a expr -> bool -> 'a eval = <fun>
|}]


(* Evaluating a simple polymorphic function *)
let map () = eval <[
    let rec map ~f =
      function
      | [] -> []
      | x :: xs -> f x :: map ~f xs
    in map
  ]>
[%%expect{|
val map : unit -> <[f:($('a) -> $('b)) -> $('a) list -> $('b) list]> eval =
  <fun>
|}]

let map_int () : f:(int -> int) -> int list -> int list = map ()
[%%expect{|
Line 1, characters 58-64:
1 | let map_int () : f:(int -> int) -> int list -> int list = map ()
                                                              ^^^^^^
Error: This expression has type
         "<[f:($('a) -> $('b)) -> $('a) list -> $('b) list]> eval" =
           "f:<[($('a) -> $('b)) eval]> -> <[($('a) list -> $('b) list) eval]>"
       but an expression was expected of type
         "f:(int -> int) -> int list -> int list"
       Type "<[($('a) -> $('b)) eval]>" = "<[$('a) eval]> -> <[$('b) eval]>"
       is not compatible with type "int -> int"
       Type "<[$('a) eval]>" = "'a eval" is not compatible with type "int"
|}]

let map_int2 ()
  :  f:(int * int -> int * int * int)
  -> (int * int) list
  -> (int * int * int) list
  = map ()
[%%expect{|
Line 5, characters 4-10:
5 |   = map ()
        ^^^^^^
Error: This expression has type
         "<[f:($('a) -> $('b)) -> $('a) list -> $('b) list]> eval" =
           "f:<[($('a) -> $('b)) eval]> -> <[($('a) list -> $('b) list) eval]>"
       but an expression was expected of type
         "f:(int * int -> int * int * int) ->
         (int * int) list -> (int * int * int) list"
       Type "<[($('a) -> $('b)) eval]>" = "<[$('a) eval]> -> <[$('b) eval]>"
       is not compatible with type "int * int -> int * int * int"
       Type "<[$('a) eval]>" = "'a eval" is not compatible with type "int * int"
|}]

let map_int2'
    ~(f : int * int -> int * int * int)
    (x : (int * int) list)
  : (int * int * int) list
  = map () ~f x
[%%expect{|
Line 5, characters 12-13:
5 |   = map () ~f x
                ^
Error: This expression has type "int * int -> int * int * int"
       but an expression was expected of type
         "<[$('a) eval]> -> <[$('b) eval]>"
       Type "int * int" is not compatible with type "<[$('a) eval]>" = "'a eval"
|}]

let map_int2'' ~f x : (int * int * int) list = map ~f x
[%%expect{|
Line 1, characters 47-55:
1 | let map_int2'' ~f x : (int * int * int) list = map ~f x
                                                   ^^^^^^^^
Error: This expression has type
         "<[($('a) list -> $('b) list) eval]>" =
           "<[$('a) list eval]> -> <[$('b) list eval]>"
       but an expression was expected of type "(int * int * int) list"
  Hint: This function application is partial,
  maybe some arguments are missing.
|}]


(* format bug *)

let must_evals_to_unit e = if true then eval e else ()
[%%expect{|
Line 1, characters 52-54:
1 | let must_evals_to_unit e = if true then eval e else ()
                                                        ^^
Error: This expression has type "unit" but an expression was expected of type
         "'a eval"
|}]

let () =
  let f x : <[ _ format6 ]> expr = x in
  must_unit
    <[let p = $(f <[""]>) in
      Format.printf p ]>
[%%expect{|
Line 3, characters 2-11:
3 |   must_unit
      ^^^^^^^^^
Error: Unbound value "must_unit"
|}]


(** Iterating eval **)


let i00 x y = function true -> x | false -> y
[%%expect{|
val i00 : 'a -> 'a -> bool -> 'a = <fun>
|}]


(* 1 eval *)

let i01 x y = function true -> x | false -> eval y
[%%expect{|
val i01 : 'a eval -> 'a expr -> bool -> 'a eval = <fun>
|}]
let i10 x y = function true -> eval x | false -> y
[%%expect{|
val i10 : 'a expr -> 'a eval -> bool -> 'a eval = <fun>
|}]
let i11 x y = function true -> eval x | false -> eval y
[%%expect{|
val i11 : 'a expr -> 'a expr -> bool -> 'a eval = <fun>
|}]

(* 2 evals *)
let i02 x y = function true -> x | false -> eval (eval y)
[%%expect{|
Line 1, characters 49-57:
1 | let i02 x y = function true -> x | false -> eval (eval y)
                                                     ^^^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "'b expr"
|}]
let i20 x y = function true -> eval (eval x) | false -> y
[%%expect{|
Line 1, characters 36-44:
1 | let i20 x y = function true -> eval (eval x) | false -> y
                                        ^^^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "'b expr"
|}]
let i12 x y = function true -> eval x | false -> eval (eval y)
[%%expect{|
Line 1, characters 54-62:
1 | let i12 x y = function true -> eval x | false -> eval (eval y)
                                                          ^^^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "'b expr"
|}]
let i21 x y = function true -> eval (eval x) | false -> eval y
[%%expect{|
Line 1, characters 36-44:
1 | let i21 x y = function true -> eval (eval x) | false -> eval y
                                        ^^^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "'b expr"
|}]
let i22 x y = function true -> eval (eval x) | false -> eval (eval y)
[%%expect{|
Line 1, characters 36-44:
1 | let i22 x y = function true -> eval (eval x) | false -> eval (eval y)
                                        ^^^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "'b expr"
|}]

(* 3 evals *)
let i03 x y = function true -> x | false -> eval (eval (eval y))
[%%expect{|
Line 1, characters 55-63:
1 | let i03 x y = function true -> x | false -> eval (eval (eval y))
                                                           ^^^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "'b expr"
|}]
let i33 x y = function true -> eval (eval (eval x)) | false -> eval (eval (eval y))
[%%expect{|
Line 1, characters 42-50:
1 | let i33 x y = function true -> eval (eval (eval x)) | false -> eval (eval (eval y))
                                              ^^^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "'b expr"
|}]


(** Inclusion checks **)


(* Image of eval *)

module M : sig
  val id : unit -> 'a eval -> 'a eval  (* in image *)
end = struct
  let id () = eval <[ fun x -> x ]>
end
[%%expect{|
module M : sig val id : unit -> 'a eval -> 'a eval end
|}]
module M : sig
  val id : unit -> 'a -> 'a  (* outside image *)
end = struct
  let id () = eval <[ fun x -> x ]>
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let id () = eval <[ fun x -> x ]>
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val id : unit -> <[$('a) -> $('a)]> eval end
       is not included in
         sig val id : unit -> 'a -> 'a end
       Values do not match:
         val id : unit -> <[$('a) -> $('a)]> eval
       is not included in
         val id : unit -> 'a -> 'a
       The type "unit -> <[$('a) -> $('a)]> eval"
       is not compatible with the type "unit -> 'b -> 'b"
       Type "<[$('a) -> $('a)]> eval" = "<[$('a) eval]> -> <[$('a) eval]>"
       is not compatible with type "'b -> 'b"
       Type "<[$('a) eval]>" = "'a eval" is not compatible with type "'b"
|}]


(* Concretising variables with evals-to constraints *)

module M : sig
  val f : unit -> int
end = struct
  let f () = eval <[ 42 ]>
end
[%%expect{|
module M : sig val f : unit -> int end
|}]

module M : sig
  val f : unit -> string
end = struct
  let f () = eval <[ 42 ]>
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f () = eval <[ 42 ]>
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : unit -> <[int]> eval end
       is not included in
         sig val f : unit -> string end
       Values do not match:
         val f : unit -> <[int]> eval
       is not included in
         val f : unit -> string
       The type "unit -> <[int]> eval" is not compatible with the type
         "unit -> string"
       Type "<[int]> eval" = "int" is not compatible with type "string"
|}]

module M : sig
  val f : unit -> 'a  (* obviously unsound *)
end = struct
  let f () = eval <[ 42 ]>
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f () = eval <[ 42 ]>
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : unit -> <[int]> eval end
       is not included in
         sig val f : unit -> 'a end
       Values do not match:
         val f : unit -> <[int]> eval
       is not included in
         val f : unit -> 'a
       The type "unit -> <[int]> eval" is not compatible with the type
         "unit -> 'a"
       Type "<[int]> eval" = "int" is not compatible with type "'a"
       Hint: Did you forget to wrap the expression using "fun () ->"?
|}]

(* Underlying eval : 'a expr -> 'a eval is constrained to 'a eval = int *)

module M = struct
  let f e = function true -> eval e | false -> 42
end
[%%expect{|
Line 2, characters 47-49:
2 |   let f e = function true -> eval e | false -> 42
                                                   ^^
Error: This expression has type "int" but an expression was expected of type
         "'a eval"
|}]
module M : sig
  val f : <[int]> expr -> int
end = struct
  let f e = function true -> eval e | false -> 42
end
[%%expect{|
Line 4, characters 47-49:
4 |   let f e = function true -> eval e | false -> 42
                                                   ^^
Error: This expression has type "int" but an expression was expected of type
         "'a eval"
|}]
module M : sig
  val f : <[int]> expr -> <[int]> eval
end = struct
  let f e = function true -> eval e | false -> 42
end
[%%expect{|
Line 4, characters 47-49:
4 |   let f e = function true -> eval e | false -> 42
                                                   ^^
Error: This expression has type "int" but an expression was expected of type
         "'a eval"
|}]

module M : sig
  val f : <[string]> expr -> string
end = struct
  let f e = function true -> eval e | false -> 42
end
[%%expect{|
Line 4, characters 47-49:
4 |   let f e = function true -> eval e | false -> 42
                                                   ^^
Error: This expression has type "int" but an expression was expected of type
         "'a eval"
|}]
module M : sig
  val f : <[string]> expr -> <[string]> eval
end = struct
  let f e = function true -> eval e | false -> 42
end
[%%expect{|
Line 4, characters 47-49:
4 |   let f e = function true -> eval e | false -> 42
                                                   ^^
Error: This expression has type "int" but an expression was expected of type
         "'a eval"
|}]
