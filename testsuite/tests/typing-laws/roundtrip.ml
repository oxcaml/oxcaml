(* TEST
 setup-ocamlopt.opt-build-env;
 script = "sh ${test_source_directory}/roundtrip.sh ${ocamlsrcdir}";
 script;
*)

(* The interface that [-i] prints for this file is compiled, and this file
   is compiled against it. Inclusion then compares each printed law with
   the original: the parameter types must be equal and the clauses
   alpha-equivalent. *)

let f x = x
let g ~a c = a + c
let ( +! ) x y = x + y
module Ops = struct
  let ( + ) = ( + )
  let add = ( + )
end

law? prefix (x : int) : f (f x) = f x
law? infix (x : int) (y : int) : x + y >= x && x +! y = y +! x
law? qualified (x : int) : Ops.( + ) x 1 = Ops.add x 1
law? negative (x : int) : f (-1) = x ===> x = -1
law? labelled (c : int) : g ~a:1 c = g c ~a:1
law? stdlib (xs : 'a list) (n : int) : List.length xs >= n
law? kinded (x : ('a : immutable_data)) (y : 'a) : f x = f y
law? \#type (\#method : int) : \#method = \#method
law? assumptions (x : int) (y : int) : x > 0 ===> y > 0 ===> x + y > 0

(* Laws in a submodule and in a module type. *)

module Nested = struct
  let h x = x
  law? nested (x : int) : h x = x
  module type S = sig
    val k : int -> int
    law? in_module_type (x : int) : k x = x
  end
end

(* A law with function parameter. *)

law? parameter (f : int -> int -> int) : f 1 2 = f 2 1

(* Shadowed names of [Stdlib] are printed qualified; local ones are not. *)

module List = struct let length _ = 0 end
let max _ _ = 0
law? shadowed_module (xs : int list) : Stdlib.List.length xs >= 0
law? shadowed_value (x : int) : Stdlib.max x 0 >= 0
law? local_module (xs : int list) : List.length xs = 0
law? local_value (x : int) : max x 0 = 0
