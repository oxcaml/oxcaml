(* Jane Street: We use the compiler's implementation of pprintast, not ppxlib's. *)

module Janestreet = Ocaml_common.Pprintast

(* When the AST used by Ppxlib changes, update [From] and add the appropriate
   [Migrate_XXX_to_YYY] calls to the [copy] functions; when we update the compiler, update
   [To] and remove the appropriate [Migrate_XXX_to_YYY] calls. *)
module To_janestreet = struct
  module From = Ast_414.Parsetree
  module To = Ast_999.Parsetree

  let copy_structure (x : From.structure) : To.structure =
    Migrate_500_999.copy_structure x

  let copy_signature (x : From.signature) : To.signature =
    Migrate_500_999.copy_signature x

  let copy_toplevel_phrase (x : From.toplevel_phrase) : To.toplevel_phrase =
      Migrate_500_999.copy_toplevel_phrase x

  let copy_core_type (x : From.core_type) : To.core_type =
      Migrate_500_999.copy_core_type x

  let copy_expression (x : From.expression) : To.expression =
      Migrate_500_999.copy_expression x

  let copy_pattern (x : From.pattern) : To.pattern =
      Migrate_500_999.copy_pattern x

  let copy_type_declaration (x : From.type_declaration) : To.type_declaration =
    Migrate_500_999.copy_type_declaration x

  let copy_class_expr (x : From.class_expr) : To.class_expr =
    Migrate_500_999.copy_class_expr x

  let copy_class_field (x : From.class_field) : To.class_field =
    Migrate_500_999.copy_class_field x

  let copy_class_type (x : From.class_type) : To.class_type =
    Migrate_500_999.copy_class_type x

  let copy_class_signature (x : From.class_signature) : (To.class_signature) =
    Migrate_500_999.copy_class_signature x

  let copy_class_type_field (x : From.class_type_field) : To.class_type_field =
    Migrate_500_999.copy_class_type_field x

  let copy_module_expr (x : From.module_expr) : To.module_expr =
    Migrate_500_999.copy_module_expr x

  let copy_module_type (x : From.module_type) : To.module_type =
    Migrate_500_999.copy_module_type x

  let copy_signature_item (x : From.signature_item) : To.signature_item =
    Migrate_500_999.copy_signature_item x

  let copy_structure_item (x : From.structure_item) : To.structure_item =
    Migrate_500_999.copy_structure_item x
end

type space_formatter = (unit, Format.formatter, unit) format

let toplevel_phrase fmt phrase =
  Janestreet.toplevel_phrase fmt (To_janestreet.copy_toplevel_phrase phrase)

let expression fmt expr =
  Janestreet.expression fmt (To_janestreet.copy_expression expr)

let string_of_expression expr =
  Janestreet.string_of_expression (To_janestreet.copy_expression expr)

let top_phrase fmt phrase =
  Janestreet.top_phrase fmt (To_janestreet.copy_toplevel_phrase phrase)

let core_type fmt typ =
  Janestreet.core_type fmt (To_janestreet.copy_core_type typ)

let pattern fmt pat =
  Janestreet.pattern fmt (To_janestreet.copy_pattern pat)

let signature fmt sg =
  Janestreet.signature fmt (To_janestreet.copy_signature sg)

let structure fmt st =
  Janestreet.structure fmt (To_janestreet.copy_structure st)

let string_of_structure st =
  Janestreet.string_of_structure (To_janestreet.copy_structure st)

let class_expr fmt clexpr =
  Janestreet.class_expr fmt (To_janestreet.copy_class_expr clexpr)

let class_field fmt clfield =
  Janestreet.class_field fmt (To_janestreet.copy_class_field clfield)

let class_type fmt clty =
  Janestreet.class_type fmt (To_janestreet.copy_class_type clty)

let class_signature fmt clsig =
  Janestreet.class_signature fmt (To_janestreet.copy_class_signature clsig)

let class_type_field fmt ctf =
  Janestreet.class_type_field fmt (To_janestreet.copy_class_type_field ctf)

let module_expr fmt mod_expr =
  Janestreet.module_expr fmt (To_janestreet.copy_module_expr mod_expr)

let module_type fmt mod_ty =
  Janestreet.module_type fmt (To_janestreet.copy_module_type mod_ty)

let signature_item fmt sig_item =
  Janestreet.signature_item fmt (To_janestreet.copy_signature_item sig_item)

let signature_items fmt items =
  List.iteri (fun i item ->
      if i > 0 then Format.fprintf fmt "\n";
      signature_item fmt item)
    items

let structure_item fmt str_item =
  Janestreet.structure_item fmt (To_janestreet.copy_structure_item str_item)

let type_declaration fmt tydecl =
  Janestreet.type_declaration fmt (To_janestreet.copy_type_declaration tydecl)
