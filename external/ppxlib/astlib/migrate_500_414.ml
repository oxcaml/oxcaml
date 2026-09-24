module From = Ast_500
module To = Ast_414

(*$ open Astlib_cinaps_helpers $*)

(*$ foreach_type (fun _ s ->
      Printf.printf
        "let copy_%s\n\
        \  : Ast_500.Parsetree.%s -> Ast_414.Parsetree.%s\n\
        \  = fun x -> x\n\n"
        s s s
  )
*)
let copy_structure
  : Ast_500.Parsetree.structure -> Ast_414.Parsetree.structure
  = fun x -> x

let copy_signature
  : Ast_500.Parsetree.signature -> Ast_414.Parsetree.signature
  = fun x -> x

let copy_toplevel_phrase
  : Ast_500.Parsetree.toplevel_phrase -> Ast_414.Parsetree.toplevel_phrase
  = fun x -> x

let copy_core_type
  : Ast_500.Parsetree.core_type -> Ast_414.Parsetree.core_type
  = fun x -> x

let copy_expression
  : Ast_500.Parsetree.expression -> Ast_414.Parsetree.expression
  = fun x -> x

let copy_pattern
  : Ast_500.Parsetree.pattern -> Ast_414.Parsetree.pattern
  = fun x -> x

let copy_case
  : Ast_500.Parsetree.case -> Ast_414.Parsetree.case
  = fun x -> x

let copy_type_declaration
  : Ast_500.Parsetree.type_declaration -> Ast_414.Parsetree.type_declaration
  = fun x -> x

let copy_type_extension
  : Ast_500.Parsetree.type_extension -> Ast_414.Parsetree.type_extension
  = fun x -> x

let copy_extension_constructor
  : Ast_500.Parsetree.extension_constructor -> Ast_414.Parsetree.extension_constructor
  = fun x -> x

let copy_class_expr
  : Ast_500.Parsetree.class_expr -> Ast_414.Parsetree.class_expr
  = fun x -> x

let copy_class_field
  : Ast_500.Parsetree.class_field -> Ast_414.Parsetree.class_field
  = fun x -> x

let copy_class_type
  : Ast_500.Parsetree.class_type -> Ast_414.Parsetree.class_type
  = fun x -> x

let copy_class_signature
  : Ast_500.Parsetree.class_signature -> Ast_414.Parsetree.class_signature
  = fun x -> x

let copy_class_type_field
  : Ast_500.Parsetree.class_type_field -> Ast_414.Parsetree.class_type_field
  = fun x -> x

let copy_module_expr
  : Ast_500.Parsetree.module_expr -> Ast_414.Parsetree.module_expr
  = fun x -> x

let copy_module_type
  : Ast_500.Parsetree.module_type -> Ast_414.Parsetree.module_type
  = fun x -> x

let copy_signature_item
  : Ast_500.Parsetree.signature_item -> Ast_414.Parsetree.signature_item
  = fun x -> x

let copy_structure_item
  : Ast_500.Parsetree.structure_item -> Ast_414.Parsetree.structure_item
  = fun x -> x

(*$*)
