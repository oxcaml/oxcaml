open Ppxlib

let loc = Location.none

let ast =
  let vbs =
    let pat =
      Ast_builder.Default.ppat_constraint ~loc
        [%pat? f]
        (Ast_builder.Default.ptyp_poly ~loc
           [ Loc.make ~loc "a" ]
           [%type: 'a -> unit])
    in
    let expr = [%expr fun _ -> ()] in
    (* It is important that this is either built using [Latest.value_binding]
       or assembled manually as [Ast_builder.Defaut.value_binding] will
       generate a pvb_constraint, entirely defeatin the test's purpose. *)
    [ Ast_builder.Default.Latest.value_binding ~loc ~pat ~expr () ]
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
