(* TEST
 flags = "-stop-after parsing -dsource";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The syntax of laws, in every position and shape, as printed back by
   [-dsource]. *)

(* Positions *)

law? in_structure : true

module type S = sig
  val f : int -> int
  law? in_signature (x : int) : f x = x
end

module M = struct
  law? in_module (x : int) : x = x
  module N = struct
    law? in_nested_module : true
  end
  module type T = sig
    law? in_nested_module_type : true
  end
end

module F (X : sig law? in_functor_parameter : true end) = struct
  law? in_functor_body : true
end

module type FT = functor (X : S) -> sig
  law? in_functor_result (x : int) : X.f x = x
end

module G (X : S) : sig law? in_functor_result_constraint : true end = struct
  law? in_functor_result_constraint : true
end

module type FC = sig law? in_first_class_module : true end
let first_class = (module M : FC)

module Inc = struct
  include (struct law? in_included_structure : true end)
end

(* Parameters *)

law? no_params : true
law? one_param (x : int) : x = x
law? several_params (x : int) (y : int) (z : int) : x + y + z = z + y + x
law? polymorphic_params (xs : 'a list) (f : 'a -> 'b) :
  List.length (List.map f xs) = List.length xs
law? unannotated_params x y z : x + y + z = z + y + x
law? mixed_params x (y : int) z (t : 'a list) : x + y = z && t = []
law? complex_param_types
  (p : int * string) (f : int -> int -> int) (o : int option)
  (r : < m : int >) (v : [ `A | `B of int ]) (t : (int, string) Hashtbl.t) :
  true

(* Implications *)

law? no_assumption : true
law? one_assumption (x : int) : x > 0 ===> x <> 0
law? two_assumptions (x : int) (y : int) : x > 0 ===> y > 0 ===> x + y > 0
law? three_assumptions (x : int) (y : int) (z : int) :
  x > 0 ===> y > 0 ===> z > 0 ===> x * y * z > 0

(* Attributes *)

law? attributed : true [@@attr]
law? several_attributes (x : int) : x = x [@@attr1] [@@attr2 payload]
law? attributed_clauses (x : int) :
  (x > 0) [@attr_assumption] ===> (x <> 0) [@attr_conclusion]
law? attributed_param_type (x : int [@attr]) : true
[@@@floating]

(* Docstrings are attributes. *)

(** Documented. *)
law? documented : true
