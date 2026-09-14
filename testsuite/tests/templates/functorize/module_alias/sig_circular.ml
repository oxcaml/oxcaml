(* Prints the bundle's inferred module structure via [ocamlc -i].
   [include] forces expansion instead of the printer just referring to
   the applicative [Bundle_circular.Intf(P_int).S]. *)
include Bundle_circular.Make (P_int) ()
