(* TEST
 flags = "-extension runtime_metaprogramming -no-locs";
 expect;
*)

#syntax quotations on

(* avoid actually using Camlinternaleval, since we are just testing [Teval] *)
open (struct
  let eval a = Obj.magic a
end : sig
  val eval : 'a expr -> 'a eval
end)

[%%expect{|
val eval : 'a expr -> 'a eval = <fun>
|}]


(* we need introduce some unit parameters to dodge the value restriction *)


let id () = eval <[ fun x -> x ]>

[%%expect{|
val id : unit -> <[$('a) -> $('a)]> eval = <fun>
|}]

let id' (type a) () : a eval -> a eval = id ()

[%%expect{|
val id' : unit -> 'a eval -> 'a eval = <fun>
|}]


let an_int : unit -> int = eval <[ fun () -> 42 + 0 ]>

[%%expect{|
val an_int : unit -> int = <fun>
|}]

let not_an_int : unit -> int = eval <[ fun () -> "42" ^ "" ]>

[%%expect{|
Line _, characters _-_:
 | let not_an_int : unit -> int = eval <[ fun () -> "42" ^ "" ]>
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type
         "<[unit -> string]> eval" = "<[unit]> eval -> <[string]> eval"
       but an expression was expected of type "<[unit]> eval -> int"
       Type "<[string]> eval" = "string" is not compatible with type "int"
|}]


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

let map_int : f:(int -> int) -> int list -> int list = map ()

[%%expect{|
val map_int : f:(int -> int) -> int list -> int list = <fun>
|}]

let map_int2
  :  f:(int * int -> int * int * int)
  -> (int * int) list
  -> (int * int * int) list
  = map ()

[%%expect{|
val map_int2 :
  f:(int * int -> int * int * int) ->
  (int * int) list -> (int * int * int) list = <fun>
|}]

let map_int2' ()
    ~(f : int * int -> int * int * int)
    (x : (int * int) list)
  : (int * int * int) list
  = map () ~f x

[%%expect{|
val map_int2' :
  unit ->
  f:(int * int -> int * int * int) ->
  (int * int) list -> (int * int * int) list = <fun>
|}]


let foo = fun x e p -> if p then x else eval e
let foo' : 'a. 'a eval -> 'a expr -> bool -> 'a eval = foo

[%%expect{|
val foo : 'a eval -> 'a expr -> bool -> 'a eval = <fun>
val foo' : 'a eval -> 'a expr -> bool -> 'a eval = <fun>
|}]

(* FIXME: The type scheme should show that 'a eval = unit. *)
let must_evals_to_unit e = if true then eval e else ()

[%%expect{|
val must_evals_to_unit : ('a [@evals_to]) expr -> ('a [@evals_to]) eval =
  <fun>
|}]

(* [evals_to_unit] is already generalised:
   its return value might not be unit. *)
let must_unit e : unit = must_evals_to_unit e

[%%expect{|
val must_unit : ('a [@evals_to]) expr -> unit = <fun>
|}]


(* FIXME: I'm fairly certain this should work, and it's almost like the above.
   The error message sure is confusing, so this might be an expansion bug. *)
let () =
  let f x : <[ _ format6 ]> expr = x in
  must_unit
    <[let p = $(f <[""]>) in
      Format.printf p ]>

[%%expect{|
Line _, characters _-_:
 |       Format.printf p ]>
         ^^^^^^^^^^^^^^^
Error: This expression has type "unit" = "unit"
       but an expression was expected of type "('a [@evals_to])"
       Type "unit" is not compatible with type "<[unit]> eval"
|}]


open (struct
  let inject a = Obj.magic a
end : sig
  val inject : 'a eval -> 'a expr
end)

let injected_ints () : int =
  eval <[ $(inject 2) + $(inject 2) ]>

[%%expect{|
val inject : 'a eval -> 'a expr = <fun>
val injected_ints : unit -> int = <fun>
|}]

let not_injected_ints () : int =
  eval <[ $(inject 2) + $(inject "2") ]>

[%%expect{|
Line _, characters _-_:
 |   eval <[ $(inject 2) + $(inject "2") ]>
                            ^^^^^^^^^^^^
Error: This expression has type "('a [@evals_to]) expr"
       but an expression was expected of type "<[int]> expr"
       Type "string" is not compatible with type "<[int]> eval" = "int"
|}]

(* inclusion checks for eval constraints *)

module M : sig
  type t
end = struct
  type t constraint int = t eval
end

[%%expect{|
module M : sig type t end
|}]

module M : sig
  type t constraint int = t eval
end = struct
  type t constraint int = t eval
end

[%%expect{|
module M : sig type t [@@evals_to] end
|}]

module M : sig
  type t constraint int = t eval
end = struct
  type t
end

[%%expect{|
module M : sig type t [@@evals_to] end
|}]

(* We can hide a concrete type with one that evaluates to it
   eval is reducible on concrete types -- t = <[int]> implies t eval = int *)
module M : sig
  type t constraint int = t eval
end = struct
  type t = <[int]>
end

[%%expect{|
module M : sig type t [@@evals_to] end
|}]

(* eval is not injective -- t eval = int does not imply t = <[int]> *)
module M : sig
  type t = <[int]>
end = struct
  type t constraint int = t eval
end

[%%expect{|
Lines _-_, characters _-_:
 | ......struct
 |   type t constraint int = t eval
 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t [@@evals_to] end
       is not included in
         sig type t = <[int]> end
       Type declarations do not match:
         type t
       [@@evals_to]
       is not included in
         type t = <[int]>
       The type "t" is not equal to the type "<[int]>"
|}]

(* the image of eval is not necessarily the space of all types *)
module M : sig
  val id : unit -> 'a -> 'a
end = struct
  let id () = eval <[ fun x -> x ]>
end

[%%expect{|
Lines _-_, characters _-_:
 | ......struct
 |   let id () = eval <[ fun x -> x ]>
 | end
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
       Type "<[$('a) -> $('a)]> eval" = "'a eval -> 'a eval"
       is not compatible with type "'b -> 'b"
       Type "'a eval" is not compatible with type "'b"
|}]

(* we can concretise a type that is known to evaluate to something *)
module M : sig
  val x : unit -> int
end = struct
  let x () = eval <[ 42 ]>
end

[%%expect{|
module M : sig val x : unit -> int end
|}]

(* we can abstract over a type that is known to evaluate to something *)
(* FIXME jbachurski: moregen *)
module M : sig
  type t constraint int = t eval
  val x : unit -> t
end = struct
  type t constraint int = t eval
  let x () = eval <[ 42 ]>
end

[%%expect{|
Lines _-_, characters _-_:
 | ......struct
 |   type t constraint int = t eval
 |   let x () = eval <[ 42 ]>
 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t [@@evals_to] val x : unit -> <[int]> eval end
       is not included in
         sig type t [@@evals_to] val x : unit -> t end
       Values do not match:
         val x : unit -> <[int]> eval
       is not included in
         val x : unit -> t
       The type "unit -> <[int]> eval" is not compatible with the type
         "unit -> t"
       Type "<[int]> eval" = "int" is not compatible with type "t"
|}]
