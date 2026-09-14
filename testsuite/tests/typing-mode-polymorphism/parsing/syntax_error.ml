(* TEST
 flags = "-extension mode_polymorphism_alpha";
 toplevel;
*)

(* Upper bounds only have meets *)

module type Bad = sig
  val bad : 'a @ [< 'm | portable] -> 'a @ [> 'm]
end;;

(* Lower bounds only have joins *)

module type Bad = sig
  val bad : 'a @ [> 'm & dynamic] -> 'a @ [< 'm]
end;;

(* Bounds cannot be empty *)

module type Bad = sig
  val bad : 'a @ [<] -> 'a @ [>]
end;;

(* Mode variables must occur before the constant bound *)

module type Bad = sig
  val bad : 'a @ [< portable & 'm] -> 'a @ [> 'm]
end;;

(* At most one constant bound, at the end *)

module type Bad = sig
  val bad : 'a @ [< 'm & portable & local] -> 'a @ [> 'm]
end;;

(* In combined bounds, the upper bound comes first *)

module type Bad = sig
  val bad : 'a @ [> 'n < 'm] -> 'a @ [> 'm]
end;;

(* Morphisms are only allowed inside bounds *)

module type Bad = sig
  val bad : 'a @ past('m) -> 'a @ [> 'm]
end;;

module type Bad = sig
  val bad : 'a @ 'm mod portable -> 'a @ [> 'm]
end;;
