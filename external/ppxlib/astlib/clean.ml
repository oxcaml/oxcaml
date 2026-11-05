let remove_ppxlib_migration_from_ast_mapper =
  let should_keep_attribute (a : Parsetree.attribute) =
    not (Stdlib0.String.is_prefix ~prefix:"ppxlib.migration" a.attr_name.txt)
  in
  let mapper =
    {
      Ast_mapper.default_mapper with
      attributes = (fun _ -> List.filter should_keep_attribute);
    }
  in
  mapper

let remove_migration_attributes_from_str =
  remove_ppxlib_migration_from_ast_mapper.structure
    remove_ppxlib_migration_from_ast_mapper

let remove_migration_attributes_from_sig =
  remove_ppxlib_migration_from_ast_mapper.signature
    remove_ppxlib_migration_from_ast_mapper
