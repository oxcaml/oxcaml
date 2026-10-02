(* -*- tuareg -*- *)

open StdLabels
open Printf

let nl () = printf "\n"

let supported_versions =
  [
    ("402", "4.02");
    ("403", "4.03");
    ("404", "4.04");
    ("405", "4.05");
    ("406", "4.06");
    ("407", "4.07");
    ("408", "4.08");
    ("409", "4.09");
    ("410", "4.10");
    ("411", "4.11");
    ("412", "4.12");
    ("413", "4.13");
    ("414", "4.14");
    ("500", "5.00");
    ("501", "5.01");
  ]

let foreach_version f =
  nl ();
  List.iter supported_versions ~f:(fun (suffix, version) -> f suffix version)

let foreach_version_pair f =
  nl ();
  let rec aux = function
    | (x, _) :: ((y, _) :: _ as tail) ->
        f x y;
        aux tail
    | [ _ ] | [] -> ()
  in
  aux supported_versions

(* Just for 4.14 <-> 5.00, mostly used by [ast_cinaps_helpers] *)

let qualified_types =
  [
    ( "Parsetree",
      [
        "structure";
        "signature";
        "toplevel_phrase";
        "core_type";
        "expression";
        "pattern";
        "case";
        "type_declaration";
        "type_extension";
        "extension_constructor";
        "class_expr";
        "class_field";
        "class_type";
        "class_signature";
        "class_type_field";
        "module_expr";
        "module_type";
        "signature_item";
        "structure_item";
      ] );
  ]

let foreach_module f =
  nl ();
  List.iter qualified_types ~f:(fun (m, types) -> f m types)

let foreach_type f = foreach_module (fun m -> List.iter ~f:(f m))
