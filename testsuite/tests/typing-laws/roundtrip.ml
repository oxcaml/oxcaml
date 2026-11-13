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

law_? prefix (x : int) : f (f x) = f x
law_? infix (x : int) (y : int) : x + y >= x && x +! y = y +! x
law_? qualified (x : int) : Ops.( + ) x 1 = Ops.add x 1
law_? negative (x : int) : f (-1) = x implies_ x = -1
law_? labelled (c : int) : g ~a:1 c = g c ~a:1
law_? stdlib (xs : 'a list) : List.length xs >= 0
law_? kinded (x : ('a : immutable_data)) (y : 'a) : f x = f y
law_? \#type (\#method : int) : \#method = \#method
law_? assumptions (x : int) (y : int) : x > 0 implies_ y > 0 implies_ x + y > 0

(* Laws in a submodule and in a module type. *)

module Nested = struct
  let h x = x
  law_? nested (x : int) : h x = x
  module type S = sig
    val k : int -> int
    law_? in_module_type (x : int) : k x = x
  end
end

(* A law with function parameter. *)

law_? parameter (f : int -> int -> int) : f 1 2 = f 2 1

(* Shadowed names of [Stdlib] are printed qualified; local ones are not. *)

module List = struct let length _ = 0 end
let max _ _ = 0
law_? shadowed_module (xs : int list) : Stdlib.List.length xs >= 0
law_? shadowed_value (x : int) : Stdlib.max x 0 >= 0
law_? local_module (xs : int list) : List.length xs = 0
law_? local_value (x : int) : max x 0 = 0
