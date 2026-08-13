(* TEST
   expect;
*)

(* We must put this in a module to stop the top-level from squeezing out
   unconstrained mode variables *)

module M = struct
  let f = fun _ -> ()

  let _ = (f :> 'a -> unit)

  let _ = f (local_ 42.0)
end

[%%expect{|
module M : sig val f : 'a @ local -> unit end
|}]

(* Mode crossing in coercions should treat argument and return positions
   uniformly on all axes except locality. *)

let subtype_arg : 'a 'b. ('a -> 'b) -> ('a @ portable -> 'b) =
  fun f -> (f : _ @ portable -> _)
[%%expect{|
val subtype_arg : ('a -> 'b) -> 'a @ portable -> 'b = <fun>
|}]

let cross_arg :
  type (a : value mod portable) b. (a @ portable -> b) -> (a -> b) =
  fun f -> (f :> a -> _)
[%%expect{|
val cross_arg :
  ('a : value mod portable) 'b. ('a @ portable -> 'b) -> 'a -> 'b = <fun>
|}]

let subtype_ret : 'a 'b. ('a -> 'b @ portable) -> ('a -> 'b) =
  fun f -> (f : _ -> _)
[%%expect{|
val subtype_ret : ('a -> 'b @ portable) -> 'a -> 'b = <fun>
|}]

let cross_ret :
  type a (b : value mod portable). (a -> b) -> (a -> b @ portable) =
  fun f -> (f :> _ -> b @ portable)
[%%expect{|
val cross_ret :
  'a ('b : value mod portable). ('a -> 'b) -> 'a -> 'b @ portable = <fun>
|}]

(* Same, with a closed coercion (exercises the ground subtyping path rather
   than [enlarge_type]) *)
let cross_ret_closed :
  type a (b : value mod portable). (a -> b) -> (a -> b @ portable) =
  fun f -> (f : a -> b :> a -> b @ portable)
[%%expect{|
val cross_ret_closed :
  'a ('b : value mod portable). ('a -> 'b) -> 'a -> 'b @ portable = <fun>
|}]

(* Locality on the return position must not cross, even for immediates: the
   function might allocate in the caller's region. *)
let cross_ret_local (f : int -> int @ local) = (f :> int -> int)
[%%expect{|
Line 1, characters 47-64:
1 | let cross_ret_local (f : int -> int @ local) = (f :> int -> int)
                                                   ^^^^^^^^^^^^^^^^^
Error: Type "int -> int @ local" is not a subtype of "int -> int"
|}]

let cross_ret_local' (f : int -> int @ local) = (f :> _ -> int)
[%%expect{|
Line 1, characters 49-50:
1 | let cross_ret_local' (f : int -> int @ local) = (f :> _ -> int)
                                                     ^
Error: This expression cannot be coerced to type ""'a -> int""; it has type
         "int -> int @ local"
       but is here used with type "'a -> int"
|}]

(* The same coercions with the arrow nested in a contravariant position. *)

let cross_ret_nested :
  type a (b : value mod portable) c.
    ((a -> b @ portable) -> c) -> ((a -> b) -> c) =
  fun f -> (f :> (_ -> b) -> _)
[%%expect{|
val cross_ret_nested :
  'a ('b : value mod portable) 'c.
    (('a -> 'b @ portable) -> 'c) -> ('a -> 'b) -> 'c =
  <fun>
|}]

let cross_ret_nested_closed :
  type a (b : value mod portable) c.
    ((a -> b @ portable) -> c) -> ((a -> b) -> c) =
  fun f -> (f : (a -> b @ portable) -> c :> (a -> b) -> c)
[%%expect{|
val cross_ret_nested_closed :
  'a ('b : value mod portable) 'c.
    (('a -> 'b @ portable) -> 'c) -> ('a -> 'b) -> 'c =
  <fun>
|}]

(* Locality on the nested return position must still not cross. *)
let cross_ret_nested_local (f : (int -> int) -> int) =
  (f :> (int -> int @ local) -> int)
[%%expect{|
Line 2, characters 2-36:
2 |   (f :> (int -> int @ local) -> int)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(int -> int) -> int" is not a subtype of
         "(int -> int @ local) -> int"
       Type "int -> int @ local" is not a subtype of "int -> int"
|}]

let cross_ret_nested_local' (f : (int -> int) -> int) =
  (f :> (_ -> int @ local) -> _)
[%%expect{|
Line 2, characters 3-4:
2 |   (f :> (_ -> int @ local) -> _)
       ^
Error: This expression cannot be coerced to type ""('a -> int @ local) -> 'b"";
       it has type "(int -> int) -> int" but is here used with type
         "('a -> int @ local) -> 'b"
       Type "int -> int" is not compatible with type "'a -> int @ local"
|}]
