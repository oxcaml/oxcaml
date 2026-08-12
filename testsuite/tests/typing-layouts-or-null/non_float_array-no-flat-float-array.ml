(* TEST
 flags = "-dlambda -dno-unique-ids -extension-universe upstream_compatible";
 no-flat-float-array;
 expect;
*)

(* With the flat float array optimization disabled, a polymorphic value
   array ['a array] is represented as an [addrarray]: there is no need
   to check whether its elements are floats. *)

let mk_gen (x : 'a) = [| x |]
[%%expect{|
(let (mk_gen = (function {nlocal = 0} x? : addrarray (makearray[addr] x)))
  (apply (field_imm 1 (global Toploop!)) "mk_gen" mk_gen))
val mk_gen : ('a : value_maybe_null). 'a -> 'a array = <fun>
|}]

let get_gen (xs : 'a array) i = xs.(i)
[%%expect{|
(let
  (get_gen =
     (function {nlocal = 0} xs[value<addrarray>] i[value<int>]
       (array.get[addr indexed by int] xs i)))
  (apply (field_imm 1 (global Toploop!)) "get_gen" get_gen))
val get_gen : ('a : value_maybe_null). 'a array -> int -> 'a = <fun>
|}]

let set_gen (xs : 'a array) x i = xs.(i) <- x
[%%expect{|
(let
  (set_gen =
     (function {nlocal = 0} xs[value<addrarray>] x? i[value<int>] : int
       (array.set[addr indexed by int] xs i x)))
  (apply (field_imm 1 (global Toploop!)) "set_gen" set_gen))
val set_gen : ('a : value_maybe_null). 'a array -> 'a -> int -> unit = <fun>
|}]
