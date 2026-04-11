open Ppxlib

let loc = Location.none

let ast =
  let vbs =
    let pat = [%pat? f] in
    let expr = [%expr fun _ -> ()] in
    let constraint_ =
      Pvc_constraint
        {
          locally_abstract_univars = [];
          typ =
            Ast_builder.Default.ptyp_poly ~loc
              [ Loc.make ~loc "a" ]
              [%type: 'a -> unit];
        }
    in
    [ Ast_builder.Default.Latest.value_binding ~loc ~pat ~expr ~constraint_ () ]
  in
  Ast_builder.Default.pstr_value ~loc Nonrecursive vbs

let print_source () = Format.printf "%a\n" Pprintast.structure_item ast
let print_ast () = Format.printf "%a\n" Pp_ast.Default.structure_item ast

let () =
  match Sys.argv with
  | [| _exec |] -> print_source ()
  | [| _exec; _flag |] ->
      print_ast ();
      Format.printf "------- PRINTED AS -------\n";
      print_source ()
  | _ ->
      Printf.eprintf "Invalid usage!";
      exit 1
