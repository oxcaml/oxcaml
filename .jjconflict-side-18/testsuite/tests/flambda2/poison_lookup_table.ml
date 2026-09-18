(* TEST
   compile_only = "true";
   flambda2;
   ocamlopt_flags = "-O3";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
 *)

type t = A | B | C

(* The simplification of switch will try to create a lookup table for the first
   field of [Some], but that would require storing a [poison] value in the
   static const array (coming from variant unboxing for the "C" branch) which
   is not supported at the time of writing. 
   
   Make sure that we don't crash and do something sensible (like storing a
   dummy value instead of the poison in the lookup table, or preventing the
   transformation into lookup tables) in that situation and don't crash! *)

let f t =
  match (match t with A -> Some 1. | B -> Some 2. | C -> None) with
  | None -> 0.
  | Some x -> x +. 1.
