(* TEST
 flags = "-dlambda -dno-unique-ids -extension-universe upstream_compatible";
 flat-float-array;
 expect;
*)

(* Normal arrays are [genarray]s. Due to the float array optimization,
   they must check whether their elements are float or non-float on
   creation, [get] and [set].

   We can see what kind of array we are getting by looking at Lambda. *)

let mk_gen (x : 'a) = [| x |]
[%%expect{|
(let (mk_gen = (function {nlocal = 0} x? : genarray (makearray[gen] x)))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "mk_gen" mk_gen))
val mk_gen : ('a : value_maybe_null). 'a -> 'a array = <fun>
|}]

let get_gen (xs : 'a array) i = xs.(i)
[%%expect{|
(let
  (get_gen =
     (function {nlocal = 0} xs[value<genarray>] i[value<int>]
       (array.get[gen indexed by int] xs i)))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "get_gen" get_gen))
val get_gen : ('a : value_maybe_null). 'a array -> int -> 'a = <fun>
|}]

let set_gen (xs : 'a array) x i = xs.(i) <- x
[%%expect{|
(let
  (set_gen =
     (function {nlocal = 0} xs[value<genarray>] x? i[value<int>] : int
       (array.set[gen indexed by int] xs i x)))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "set_gen" set_gen))
val set_gen : ('a : value_maybe_null). 'a array -> 'a -> int -> unit = <fun>
|}]
