open Model

let values_var_name module_name = "values_" ^ Name.snake_case module_name

let const_module_name module_name = module_name ^ ".Const"

let field_module_name (field : field) ~in_op =
  let opposite = if in_op then not field.declared_opposite else field.declared_opposite in
  if opposite then Name.op_module_name field.lattice_name else field.lattice_name

let axis_ctor_name field_name = String.capitalize_ascii field_name

type exported_alias =
  { alias_name : string;
    module_name : string;
    morphism : morphism
  }

let morphism_of_slot (embedding : embedding) slot =
  if slot = "embed"
  then embedding.embed
  else if String.length slot >= 4 && String.sub slot 0 4 = "left"
  then
    List.nth embedding.left_chain
      (int_of_string (String.sub slot 4 (String.length slot - 4)) - 1)
  else
    List.nth embedding.right_chain
      (int_of_string (String.sub slot 5 (String.length slot - 5)) - 1)

let collect_exported_aliases model =
  let seen = Hashtbl.create 16 in
  let add seen alias =
    if Hashtbl.mem seen alias.alias_name
    then failwith ("duplicate top-level alias export: " ^ alias.alias_name);
    Hashtbl.add seen alias.alias_name ()
  in
  List.fold_left
    (fun acc ->
      function
      | Lattice _ -> acc
      | Embedding embedding ->
        List.fold_left
          (fun acc (alias_name, slot) ->
            let alias =
              { alias_name;
                module_name = embedding.module_name;
                morphism = morphism_of_slot embedding slot
              }
            in
            add seen alias;
            acc @ [ alias ])
          acc
          embedding.aliases)
    []
    model.items
