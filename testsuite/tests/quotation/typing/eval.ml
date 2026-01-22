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
Line _, characters _-_:
 | let map_int : f:(int -> int) -> int list -> int list = map ()
                                                          ^^^^^^
Error: This expression has type
         "<[f:($('a) -> $('b)) -> $('a) list -> $('b) list]> eval" =
           "f:<[$('a) -> $('b)]> eval -> <[$('a) list -> $('b) list]> eval"
       but an expression was expected of type
         "f:(int -> int) -> int list -> int list"
       Type "<[$('a) -> $('b)]> eval" = "'a eval -> 'b eval"
       is not compatible with type "int -> int"
       Type "'a eval" is not compatible with type "int"
|}]

let map_int2
  :  f:(int * int -> int * int * int)
  -> (int * int) list
  -> (int * int * int) list
  = map ()

[%%expect{|
Line _, characters _-_:
 |   = map ()
       ^^^^^^
Error: This expression has type
         "<[f:($('a) -> $('b)) -> $('a) list -> $('b) list]> eval" =
           "f:<[$('a) -> $('b)]> eval -> <[$('a) list -> $('b) list]> eval"
       but an expression was expected of type
         "f:(int * int -> int * int * int) ->
         (int * int) list -> (int * int * int) list"
       Type "<[$('a) -> $('b)]> eval" = "'a eval -> 'b eval"
       is not compatible with type "int * int -> int * int * int"
       Type "'a eval" is not compatible with type "int * int"
|}]

let map_int2'
    ~(f : int * int -> int * int * int)
    (x : (int * int) list)
  : (int * int * int) list
  = map () ~f x

[%%expect{|
Line _, characters _-_:
 |   = map () ~f x
               ^
Error: This expression has type "int * int -> int * int * int"
       but an expression was expected of type "'a eval -> 'b eval"
       Type "int * int" is not compatible with type "'a eval"
|}]


let foo = fun x e p -> if p then x else eval e
let foo' : 'a. 'a eval -> 'a expr -> bool -> 'a eval = foo

[%%expect{|
val foo : 'a eval -> 'a expr -> bool -> 'a eval = <fun>
val foo' : 'a eval -> 'a expr -> bool -> 'a eval = <fun>
|}]


open (struct
  let inject a = Obj.magic a
end : sig
  val inject : 'a eval -> 'a expr
end)

let injected_ints : unit -> int =
  eval <[ fun () -> $(inject 2) + $(inject 2) ]>

[%%expect{|
val inject : 'a eval -> 'a expr = <fun>
Line _, characters _-_:
 |   eval <[ fun () -> $(inject 2) + $(inject 2) ]>
                                ^
Error: This expression has type "int" but an expression was expected of type
         "'a eval"
|}]

let not_injected_ints : unit -> int =
  eval <[ fun () -> $(inject 2) + $(inject "2") ]>

[%%expect{|
Line _, characters _-_:
 |   eval <[ fun () -> $(inject 2) + $(inject "2") ]>
                                ^
Error: This expression has type "int" but an expression was expected of type
         "'a eval"
|}]

(* We can hide a concrete type with one that evaluates to it
   eval is reducible on concrete types -- t = <[int]> implies t eval = int *)
module M : sig
  type t constraint int = t eval
end = struct
  type t = <[int]>
end

[%%expect{|
Line _, characters _-_:
 |   type t constraint int = t eval
                       ^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "int" is not compatible with type "t eval"
|}]

(* eval is not injective -- t eval = int does not imply t = <[int]> *)
module M : sig
  type t = <[int]>
end = struct
  type t constraint int = t eval
end

[%%expect{|
Line _, characters _-_:
 |   type t constraint int = t eval
                       ^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "int" is not compatible with type "t eval"
|}]
