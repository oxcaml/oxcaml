let ghost_mapper =
  let open Ast_mapper in
  let ghost loc = { loc with Location.loc_ghost = true } in
  { default_mapper with
    location = (fun _mapper loc -> ghost loc) }

let ast =
  let ast =
    (Pparse.parse_implementation ~tool_name:"w27_test" "w27_source.ml")
      .Pparse.ast
  in
  ghost_mapper.structure ghost_mapper ast

let () = Pparse.write_ast Pparse.Structure "w27_source.marshalled.ml" ast
