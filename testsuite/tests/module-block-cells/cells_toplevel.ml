(* TEST
 readonly_files = "cells_toplevel_lib.ml";
 flambda2;
 setup-ocamlopt.byte-build-env;
 module = "cells_toplevel_lib.ml";
 ocamlopt.byte;
 module = "";
 flags = "-shared";
 program = "cells_toplevel_lib.cmxs";
 all_modules = "cells_toplevel_lib.cmx";
 ocamlopt.byte;
 flags = "";
 all_modules = "cells_toplevel.ml";
 toplevel.opt;
*)

(* The native toplevel reads library fields from cells: phrases compiled
   against a loaded unit, [#install_printer] (which evaluates a value path
   outside compiled code), and a whole-block use in a phrase.  The Stdlib
   is a cells library too. *)

#directory "../ocamlopt.byte";;
#load "cells_toplevel_lib.cmxs";;

external box_float : float# -> float = "%box_float";;

Cells_toplevel_lib.f 1;;
Cells_toplevel_lib.n;;
box_float Cells_toplevel_lib.u;;

#install_printer Cells_toplevel_lib.print_int;;
5;;
#remove_printer Cells_toplevel_lib.print_int;;
5;;

module type S = sig
  val f : int -> int
  val n : int
  val u : float#
  val print_int : Format.formatter -> int -> unit
end;;
module W = (val (module Cells_toplevel_lib : S));;
W.f W.n;;
box_float W.u;;

List.length [1; 2; 3];;
#install_printer Format.pp_print_int;;
6;;
#remove_printer Format.pp_print_int;;
