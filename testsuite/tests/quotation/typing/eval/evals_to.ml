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

(* unit-returning stubs *)
let print_int (_ : int) = ()
let print_string (_ : string) = ()
[%%expect{|
val print_int : int -> unit = <fun>
val print_string : string -> unit = <fun>
|}]

(* We can type an unannotated eval *)

let () = print_int (eval <[ 2 + 2 ]>)
[%%expect{|
|}]

let () = print_int (eval <[ "foo" ^ "bar" ]>)  (* type error: int ~/~ string *)
[%%expect{|
Line 1, characters 19-45:
1 | let () = print_int (eval <[ "foo" ^ "bar" ]>)  (* type error: int ~/~ string *)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "string" but an expression was expected of type
         "int"
|}]


(* Identity function. Types are constrained to the image of eval *)

let id () = eval <[ fun x -> x ]>
[%%expect{|
val id : unit -> 'a eval -> 'a eval = <fun>
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
val map : unit -> f:('a eval -> 'b eval) -> 'a eval list -> 'b eval list =
  <fun>
|}]

let map_int () : f:(int -> int) -> int list -> int list = map ()
[%%expect{|
val map_int : unit -> f:(int -> int) -> int list -> int list = <fun>
|}]

let map_int2 ()
  :  f:(int * int -> int * int * int)
  -> (int * int) list
  -> (int * int * int) list
  = map ()
[%%expect{|
val map_int2 :
  unit ->
  f:(int * int -> int * int * int) ->
  (int * int) list -> (int * int * int) list = <fun>
|}]

let map_int2'
    ~(f : int * int -> int * int * int)
    (x : (int * int) list)
  : (int * int * int) list
  = map () ~f x
[%%expect{|
val map_int2' :
  f:(int * int -> int * int * int) ->
  (int * int) list -> (int * int * int) list = <fun>
|}]

let map_int2'' ~f x : (int * int * int) list = map () ~f x
[%%expect{|
val map_int2'' :
  'a ('b constraint 'b eval = int * int * int).
    f:('a eval -> 'b eval) -> 'a eval list -> (int * int * int) list =
  <fun>
|}]

(* Composition *)

let eval1 e = eval e
[%%expect{|
val eval1 : 'a expr -> 'a eval = <fun>
|}]

let eval2 e = eval1 (eval1 e)
[%%expect{|
val eval2 : 'b ('a constraint 'a eval = 'b expr). 'a expr -> 'b eval = <fun>
|}]

let eval3  e = eval1 (eval2 e)
let eval3' e = eval2 (eval1 e)
[%%expect{|
val eval3 :
  'b ('c constraint 'c eval = 'b expr) ('a constraint 'a eval = 'c expr).
    'a expr -> 'b eval =
  <fun>
val eval3' :
  'b ('c constraint 'c eval = 'b expr) ('a constraint 'a eval = 'c expr).
    'a expr -> 'b eval =
  <fun>
|}]

let eval4   e = eval3 (eval1 e)
let eval4'  e = eval1 (eval3 e)
let eval4'' e = eval2 (eval2 e)
[%%expect{|
val eval4 :
  'b ('c constraint 'c eval = 'b expr) ('d constraint 'd eval = 'c expr) ('a
    constraint 'a eval = 'd expr). 'a expr -> 'b eval =
  <fun>
val eval4' :
  'b ('c constraint 'c eval = 'b expr) ('d constraint 'd eval = 'c expr) ('a
    constraint 'a eval = 'd expr). 'a expr -> 'b eval =
  <fun>
val eval4'' :
  'b ('c constraint 'c eval = 'b expr) ('d constraint 'd eval = 'c expr) ('a
    constraint 'a eval = 'd expr). 'a expr -> 'b eval =
  <fun>
|}]

(** Evals-to consistency **)


let must_eval_to_unit e : unit = if true then eval e else ()
[%%expect{|
val must_eval_to_unit : ('a constraint 'a eval = unit). 'a expr -> unit =
  <fun>
|}]

let eval1_same' e : bool -> int = function
  | true  -> e |> eval
  | false -> e |> eval
[%%expect{|
val eval1_same' : ('a constraint 'a eval = int). 'a expr -> bool -> int =
  <fun>
|}]

let eval1_same e = function
  | true  -> e |> eval |> print_int
  | false -> e |> eval |> print_int
[%%expect{|
val eval1_same : ('a constraint 'a eval = int). 'a expr -> bool -> unit =
  <fun>
|}]

let eval1_diff e = function
  | true  -> e |> eval |> print_int
  | false -> e |> eval |> print_string
[%%expect{|
Line 3, characters 13-22:
3 |   | false -> e |> eval |> print_string
                 ^^^^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "string"
       The constraints "<[<['a]>]> eval eval = <[string]>" and
       "<[<['a]>]> eval eval = <[int]>" have to be compatible.
|}]

let eval2_same e = function
  | true  -> e |> eval |> eval |> print_int
  | false -> e |> eval |> eval |> print_int
[%%expect{|
val eval2_same :
  ('b constraint 'b eval = int) ('a constraint 'a eval = 'b expr).
    'a expr -> bool -> unit =
  <fun>
|}]

let eval2_diff e = function
  | true  -> e |> eval |> eval |> print_int
  | false -> e |> eval |> eval |> print_string
[%%expect{|
Line 3, characters 13-30:
3 |   | false -> e |> eval |> eval |> print_string
                 ^^^^^^^^^^^^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "string"
       The constraints "<[<['a]>]> eval eval = <[string]>" and
       "<[<['a]>]> eval eval = <[int]>" have to be compatible.
|}]

let () =
  let f x : <[ _ format6 ]> expr = x in
  must_eval_to_unit
    <[let p = $(f <[""]>) in
      Format.printf p ]>
[%%expect{|
|}]

let f () : ('c. 'c eval -> 'b eval -> 'c eval)
        -> 'a eval -> 'b eval -> 'a eval * 'b eval =
  eval <[ fun (f : 'd. 'd -> _ -> 'd) x y -> (f x y, y) ]>
[%%expect{|
Uncaught exception: Stack overflow

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
(*
let i11 x y = function true -> eval x | false -> eval y
[%%expect{|
val i11 :
  ('b constraint 'b eval = 'a eval) ('a constraint 'a eval = $('b) eval).
    'a expr -> $('b) expr -> bool -> 'a eval =
  <fun>
|}]

(* 2 evals *)
let i02 x y = function true -> x | false -> eval (eval y)
[%%expect{|
val i02 :
  'a ('b constraint 'b eval = 'a expr). 'a eval -> 'b expr -> bool -> 'a eval =
  <fun>
|}]
let i20 x y = function true -> eval (eval x) | false -> y
[%%expect{|
val i20 :
  'b ('a constraint 'a eval = $('b) expr).
    'a expr -> $('b) eval -> bool -> $('b) eval =
  <fun>
|}]
let i12 x y = function true -> eval x | false -> eval (eval y)
[%%expect{|
val i12 :
  ('c constraint 'c eval = 'a eval) ('a constraint 'a eval = $('c) eval) ('b
    constraint 'b eval = $('c) expr). 'a expr -> 'b expr -> bool -> 'a eval =
  <fun>
|}]
let i21 x y = function true -> eval (eval x) | false -> eval y
[%%expect{|
val i21 :
  ('b constraint 'b eval = $('c) eval) ('c constraint 'c eval = 'b eval) ('a
    constraint 'a eval = $('c) expr).
    'a expr -> 'b expr -> bool -> $('c) eval =
  <fun>
|}]
let i22 x y = function true -> eval (eval x) | false -> eval (eval y)
[%%expect{|
Uncaught exception: Stack overflow

|}]

(* 3 evals *)
let i03 x y = function true -> x | false -> eval (eval (eval y))
[%%expect{|
val i03 :
  ('b constraint 'b eval = '_weak1 expr) ('a constraint 'a eval =
    $('b) expr). '_weak1 eval -> 'a expr -> bool -> '_weak1 eval =
  <fun>
|}, Principal{|
val i03 :
  ('c constraint 'c eval = 'a eval) ('a constraint 'a eval = 'c eval) ('d
    constraint 'd eval = 'a expr) ('b constraint 'b eval = 'd expr).
    'a eval -> 'b expr -> bool -> 'a eval =
  <fun>
|}]
let i33 x y =
  function true -> eval (eval (eval x)) | false -> eval (eval (eval y))
[%%expect{|
>> Fatal error: Evals-to constraint lost on linked variable
Uncaught exception: Misc.Fatal_error

|}]
*)


(** Inclusion checks **)

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
  val f : unit -> string (* type error: int ~/~ string *)
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
         sig val f : unit -> int end
       is not included in
         sig val f : unit -> string end
       Values do not match:
         val f : unit -> int
       is not included in
         val f : unit -> string
       The type "unit -> int" is not compatible with the type "unit -> string"
       Type "int" is not compatible with type "string"
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
         sig val f : unit -> int end
       is not included in
         sig val f : unit -> 'a end
       Values do not match:
         val f : unit -> int
       is not included in
         val f : unit -> 'a
       The type "unit -> int" is not compatible with the type "unit -> 'a"
       Type "int" is not compatible with type "'a"
       Hint: Did you forget to wrap the expression using "fun () ->"?
|}]

(* Underlying eval : 'a expr -> 'a eval is constrained to 'a eval = int *)

module M = struct
  let f e = function true -> eval e | false -> 42
end
[%%expect{|
Line 1:
Error: In module "M":
       Modules do not match:
         sig
           val f : ('a constraint 'a eval = int). 'a expr -> bool -> 'a eval
         end
       is not included in
         sig
           val f : ('a constraint 'a eval = int). 'a expr -> bool -> 'a eval
         end
       In module "M":
       Values do not match:
         val f : ('a constraint 'a eval = int). 'a expr -> bool -> 'a eval
       is not included in
         val f : ('a constraint 'a eval = int). 'a expr -> bool -> 'a eval
       The type "'_weak1 expr -> bool -> '_weak1 eval"
       is not compatible with the type "'_weak1 expr -> bool -> '_weak1 eval"
       Type "'_weak1 eval" is not compatible with type "'_weak1 eval"
       Type "<['_weak1]> eval" is not compatible with type "<[int]>"
|}]

(* compatible *)
module M : sig
  val f : <[int]> expr -> bool -> int
end = struct
  let f e = function true -> eval e | false -> 42
end
[%%expect{|
module M : sig val f : <[int]> expr -> bool -> int end
|}]

(* incompatible, type error: evals-to int ~/~ evals-to string *)
module M : sig
  val f : <[string]> expr -> bool -> string
end = struct
  let f e = function true -> eval e | false -> 42
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f e = function true -> eval e | false -> 42
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val f : ('a constraint 'a eval = int). 'a expr -> bool -> 'a eval
         end
       is not included in
         sig val f : <[string]> expr -> bool -> string end
       Values do not match:
         val f : ('a constraint 'a eval = int). 'a expr -> bool -> 'a eval
       is not included in
         val f : <[string]> expr -> bool -> string
       The type "'a expr -> bool -> 'a eval" is not compatible with the type
         "<[string]> expr -> bool -> string"
       Type "<[int]>" is not compatible with type "<[string]>"
       The constraint "<['a]> eval = <[int]>" has to be satisfied for "'a =
       <[string]>".
|}]

(* incompatible, unconstrained does not subsume constrained *)
module M : sig
  val id : 'b eval -> 'b eval
end = struct
  let id : 'a eval -> 'a eval = fun x -> if true then x else ()
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let id : 'a eval -> 'a eval = fun x -> if true then x else ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val id : ('a constraint 'a eval = unit). 'a eval -> 'a eval end
       is not included in
         sig val id : 'b eval -> 'b eval end
       Values do not match:
         val id : ('a constraint 'a eval = unit). 'a eval -> 'a eval
       is not included in
         val id : 'b eval -> 'b eval
       The type "'a eval -> 'a eval" is not compatible with the type
         "'b eval -> 'b eval"
       Type "'a eval" is not compatible with type "'b eval"
       The constraints "<[<['a]>]> eval eval = <['b]> eval" and
       "<[<['a]>]> eval eval = <[unit]>" have to be compatible.
|}]

(* perhaps surprisingly, compatible both ways due to surjectivity of eval *)
module M : sig
  val id : 'b -> 'b
end = struct
  let id : 'a eval -> 'a eval = fun x -> x
end
[%%expect{|
module M : sig val id : 'b -> 'b end
|}]
module M : sig
  val id : 'b eval -> 'b eval
end = struct
  let id : 'a -> 'a = fun x -> x
end
[%%expect{|
module M : sig val id : 'b eval -> 'b eval end
|}]
(* ...but not once constrained *)
module M : sig
  val id : 'b -> 'b
end = struct
  let id : 'a eval -> 'a eval = fun x -> if true then x else ()
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let id : 'a eval -> 'a eval = fun x -> if true then x else ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val id : ('a constraint 'a eval = unit). 'a eval -> 'a eval end
       is not included in
         sig val id : 'b -> 'b end
       Values do not match:
         val id : ('a constraint 'a eval = unit). 'a eval -> 'a eval
       is not included in
         val id : 'b -> 'b
       The type "'a eval -> 'a eval" is not compatible with the type "'b -> 'b"
       Type "'a eval" is not compatible with type "'b"
       The constraints "<[<['a]>]> eval eval = <['b]>" and
       "<[<['a]>]> eval eval = <[unit]>" have to be compatible.
|}]
