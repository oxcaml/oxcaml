(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: a record with one value field and two unboxed
   flat-suffix fields builds a mixed block and reads each field at its correct
   field kind; a load from a locally-built immutable mixed block folds.
   Rules: P.Variadic.MakeBlock.Mixed, P.Unary.BlockLoad.Mixed,
   P.MixedShape.FieldKinds, WF.Prim.MakeBlockMixed.
   Case study: middle_end/flambda2/docs/formalism/14-validation/mixed-01-record.md
   Phenomenon: Make_block mixed shape [0].[float, int32]; per-field loads at the
   correct element kind; roundtrip folds the flat-suffix load to the argument. *)

type t = { x1 : string; x2 : float#; x3 : int32# }

let build (s : string) (f : float#) (i : int32#) = { x1 = s; x2 = f; x3 = i }

let get_x1 (t : t) = t.x1
let get_x2 (t : t) = t.x2
let get_x3 (t : t) = t.x3

let roundtrip (s : string) (f : float#) (i : int32#) =
  let t = { x1 = s; x2 = f; x3 = i } in
  get_x2 t
