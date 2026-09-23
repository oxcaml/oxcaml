(* TEST
 flags = "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(* Examples on types for sanity *)

type ('a, 'b) t = 'a * 'c -> 'b
  constraint 'a = 'c -> 'd;;
[%%expect{|
type ('a, 'b) t = 'a * 'c -> 'b constraint 'a = 'c -> 'd
|}];;

let f (x : ('a, 'b) t) : _ -> _ = x
[%%expect{|
val f :
  ('a -> 'c, 'b) t @ [< 'm mod aliased contended immutable] ->
  (('a -> 'c) * 'a -> 'b) @ [> 'm] = <fun>
|}];;

type ('a, 'b) t = 'a * 'c -> 'b
  constraint 'a = 'b;;
[%%expect{|
Line 1, characters 23-25:
1 | type ('a, 'b) t = 'a * 'c -> 'b
                           ^^
Error: The type variable "'c" is unbound in this type declaration.
|}];;

(* ['m] is unbound *)
type 'a t = 'a @ [< 'm] -> 'a @ [> 'm]
[%%expect{|
Line 1, characters 20-22:
1 | type 'a t = 'a @ [< 'm] -> 'a @ [> 'm]
                        ^^
Error: The mode variable "'m" is unbound in this type declaration.
|}];;

(* ['m] is bound in a [constraint] *)
type ghost

type ('a, 'nonsense) t = 'a @ [< 'm] -> 'a @ [> 'm]
  constraint 'nonsense = ghost @ 'm -> unit;;

let f (x : ('a, ghost @ 'm -> unit) t) : 'a @ [< 'm] -> 'a @ [> 'm] = x;;
[%%expect{|
type ghost
type ('a, 'b) t = 'a @ [< 'o & 'n] -> 'a @ [> 'o | 'm]
  constraint 'b = ghost @ [< 'm > 'n] -> unit
val f :
  ('a, ghost @ [< 'm > 'n] -> unit) t @ [< 'o mod aliased contended immutable] ->
  ('a @ [< 'p & 'n] -> 'a @ [> 'p | 'm]) @ [> 'o] = <fun>
|}];;

let f (x : ('a, ghost @ local -> unit) t) : 'a @ local -> 'a @ local = x;;
[%%expect{|
val f :
  ('a, ghost @ local -> unit) t @ [< 'm mod aliased contended immutable] ->
  ('a @ local -> 'a @ local) @ [> 'm] = <fun>
|}];;

let f (x : ('a, ghost @ unique -> unit) t) : 'a @ unique -> 'a @ unique = x;;
[%%expect{|
val f :
  ('a, ghost @ unique -> unit) t @ [< 'm mod aliased contended immutable] ->
  ('a @ unique -> 'a @ unique) @ [> 'm] = <fun>
|}];;

let f (x : ('a, ghost @ local -> unit) t) (x @ local) = x;;
[%%expect{|
val f :
  ('a, ghost @ local -> unit) t @ [< past('m) & global] ->
  ('b @ [< 'n > local] -> 'b @ [> 'n | local]) @ [> past('m)] = <fun>
|}];;

let f (x : ('a, ghost @ 'm -> unit) t) : ('a, ghost @ 'm -> unit) t = x;;
[%%expect{|
val f :
  ('a, ghost @ 'm -> unit) t @ [< 'n mod aliased contended immutable] ->
  ('a, ghost @ 'o -> unit) t @ [> 'n] = <fun>
|}];;

module M : sig
  val f : 'a @ [< 'm & local] -> 'a @ [> 'm | local]
end = struct
  let f : ('a, ghost @ local -> unit) t = fun x -> x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f : ('a, ghost @ local -> unit) t = fun x -> x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : ('a, ghost @ local -> unit) t end
       is not included in
         sig val f : 'a @ [< 'm] -> 'a @ [> 'm | local] end
       Values do not match:
         val f : ('a, ghost @ local -> unit) t
       is not included in
         val f : 'a @ [< 'm] -> 'a @ [> 'm | local]
       The type
         "('a, ghost @ local -> unit) t" =
           "'a @ [< many read_write] ->
           'a @ [> local aliased stateful dynamic]"
       is not compatible with the type "'a @ [< 'm] -> 'a @ [> 'm | local]"
|}]

module M : sig
  val f : 'a @ [< local] -> 'a @ [> local]
end = struct
  let f : ('a, ghost @ local -> unit) t = fun x -> x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f : ('a, ghost @ local -> unit) t = fun x -> x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : ('a, ghost @ local -> unit) t end
       is not included in
         sig val f : 'a @ 'm -> 'a @ [> local] end
       Values do not match:
         val f : ('a, ghost @ local -> unit) t
       is not included in
         val f : 'a @ 'm -> 'a @ [> local]
       The type
         "('a, ghost @ local -> unit) t" =
           "'a @ [< many read_write] ->
           'a @ [> local aliased stateful dynamic]"
       is not compatible with the type "'a @ 'm -> 'a @ [> local]"
|}]

module M : sig
  val f : 'a @ local -> 'a @ local
end = struct
  let f : ('a, ghost @ local -> unit) t = fun x -> x
end
[%%expect{|
module M : sig val f : 'a @ local -> 'a @ local end
|}]

module M : sig
  val f : 'a -> 'a
end = struct
  let f : ('a, ghost @ local -> unit) t = fun x -> x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f : ('a, ghost @ local -> unit) t = fun x -> x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : ('a, ghost @ local -> unit) t end
       is not included in
         sig val f : 'a -> 'a end
       Values do not match:
         val f : ('a, ghost @ local -> unit) t
       is not included in
         val f : 'a -> 'a
       The type
         "('a, ghost @ local -> unit) t" =
           "'a @ [< many read_write > aliased stateful dynamic] ->
           'a @ [> local aliased stateful dynamic]"
       is not compatible with the type "'a -> 'a"
|}]
