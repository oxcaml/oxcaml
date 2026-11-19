open! Import

let remove_migration_attributes =
  object
    inherit Ast_traverse.map

    method! attributes vs =
      match vs with
      | [] -> []
      | vs ->
          let should_keep_attribute a =
            not (String.is_prefix ~prefix:"ppxlib.migration" a.attr_name.txt)
          in
          List.filter ~f:should_keep_attribute vs
  end
