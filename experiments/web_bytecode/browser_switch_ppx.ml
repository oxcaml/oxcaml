let expand_structure ast =
  Ppxlib_ast.Selected_ast.Of_ocaml.copy_structure ast
  |> Ppxlib.Driver.map_structure
  |> Ppxlib_ast.Selected_ast.To_ocaml.copy_structure
