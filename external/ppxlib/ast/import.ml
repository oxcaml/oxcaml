(* This file is used to control what we use from the current compiler and what is embed in
   this library.

   It must be opened in all modules, especially the ones coming from the compiler.
*)

(*$ open Ast_cinaps_helpers $*)

module Js = Versions.OCaml_502
module Ocaml = Versions.OCaml_current

module Select_ast (Ocaml : Versions.OCaml_version) = struct
  include Js

  module Type = struct
    type ('js, 'ocaml) t =
      (*$ foreach_type (fun _ s ->
            printf
              "      | %s\n\
              \          : ( Js.Ast.Parsetree.%s,\n\
              \              Ocaml.Ast.Parsetree.%s )\n\
              \            t\n"
              (capitalize_ascii s) s s
          )
      *)
      | Structure
          : ( Js.Ast.Parsetree.structure,
              Ocaml.Ast.Parsetree.structure )
            t
      | Signature
          : ( Js.Ast.Parsetree.signature,
              Ocaml.Ast.Parsetree.signature )
            t
      | Toplevel_phrase
          : ( Js.Ast.Parsetree.toplevel_phrase,
              Ocaml.Ast.Parsetree.toplevel_phrase )
            t
      | Core_type
          : ( Js.Ast.Parsetree.core_type,
              Ocaml.Ast.Parsetree.core_type )
            t
      | Expression
          : ( Js.Ast.Parsetree.expression,
              Ocaml.Ast.Parsetree.expression )
            t
      | Pattern
          : ( Js.Ast.Parsetree.pattern,
              Ocaml.Ast.Parsetree.pattern )
            t
      | Case
          : ( Js.Ast.Parsetree.case,
              Ocaml.Ast.Parsetree.case )
            t
      | Type_declaration
          : ( Js.Ast.Parsetree.type_declaration,
              Ocaml.Ast.Parsetree.type_declaration )
            t
      | Type_extension
          : ( Js.Ast.Parsetree.type_extension,
              Ocaml.Ast.Parsetree.type_extension )
            t
      | Extension_constructor
          : ( Js.Ast.Parsetree.extension_constructor,
              Ocaml.Ast.Parsetree.extension_constructor )
            t
      | Class_expr
          : ( Js.Ast.Parsetree.class_expr,
              Ocaml.Ast.Parsetree.class_expr )
            t
      | Class_field
          : ( Js.Ast.Parsetree.class_field,
              Ocaml.Ast.Parsetree.class_field )
            t
      | Class_type
          : ( Js.Ast.Parsetree.class_type,
              Ocaml.Ast.Parsetree.class_type )
            t
      | Class_signature
          : ( Js.Ast.Parsetree.class_signature,
              Ocaml.Ast.Parsetree.class_signature )
            t
      | Class_type_field
          : ( Js.Ast.Parsetree.class_type_field,
              Ocaml.Ast.Parsetree.class_type_field )
            t
      | Module_expr
          : ( Js.Ast.Parsetree.module_expr,
              Ocaml.Ast.Parsetree.module_expr )
            t
      | Module_type
          : ( Js.Ast.Parsetree.module_type,
              Ocaml.Ast.Parsetree.module_type )
            t
      | Signature_item
          : ( Js.Ast.Parsetree.signature_item,
              Ocaml.Ast.Parsetree.signature_item )
            t
      | Structure_item
          : ( Js.Ast.Parsetree.structure_item,
              Ocaml.Ast.Parsetree.structure_item )
            t
(*$*)
      | List : ('a, 'b) t -> ('a list, 'b list) t
      | Pair : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  end

  open Type
  module Of_ocaml = Versions.Convert (Ocaml) (Js)
  module To_ocaml = Versions.Convert (Js) (Ocaml)

  let rec of_ocaml : type ocaml js. (js, ocaml) Type.t -> ocaml -> js =
    let open Of_ocaml in
    fun node ->
      match node with
      (*$ foreach_type (fun _ s ->
            printf
              "      | %s -> copy_%s\n"
              (capitalize_ascii s) s
          )
      *)
      | Structure -> copy_structure
      | Signature -> copy_signature
      | Toplevel_phrase -> copy_toplevel_phrase
      | Core_type -> copy_core_type
      | Expression -> copy_expression
      | Pattern -> copy_pattern
      | Case -> copy_case
      | Type_declaration -> copy_type_declaration
      | Type_extension -> copy_type_extension
      | Extension_constructor -> copy_extension_constructor
      | Class_expr -> copy_class_expr
      | Class_field -> copy_class_field
      | Class_type -> copy_class_type
      | Class_signature -> copy_class_signature
      | Class_type_field -> copy_class_type_field
      | Module_expr -> copy_module_expr
      | Module_type -> copy_module_type
      | Signature_item -> copy_signature_item
      | Structure_item -> copy_structure_item
(*$*)
      | List t -> List.map (of_ocaml t)
      | Pair (a, b) ->
          let f = of_ocaml a in
          let g = of_ocaml b in
          fun (x, y) -> (f x, g y)

  let rec to_ocaml : type ocaml js. (js, ocaml) Type.t -> js -> ocaml =
    let open To_ocaml in
    fun node ->
      match node with
      (*$ foreach_type (fun _ s ->
            printf
              "      | %s -> copy_%s\n"
              (capitalize_ascii s) s
          )
      *)
      | Structure -> copy_structure
      | Signature -> copy_signature
      | Toplevel_phrase -> copy_toplevel_phrase
      | Core_type -> copy_core_type
      | Expression -> copy_expression
      | Pattern -> copy_pattern
      | Case -> copy_case
      | Type_declaration -> copy_type_declaration
      | Type_extension -> copy_type_extension
      | Extension_constructor -> copy_extension_constructor
      | Class_expr -> copy_class_expr
      | Class_field -> copy_class_field
      | Class_type -> copy_class_type
      | Class_signature -> copy_class_signature
      | Class_type_field -> copy_class_type_field
      | Module_expr -> copy_module_expr
      | Module_type -> copy_module_type
      | Signature_item -> copy_signature_item
      | Structure_item -> copy_structure_item
(*$*)
      | List t -> List.map (to_ocaml t)
      | Pair (a, b) ->
          let f = to_ocaml a in
          let g = to_ocaml b in
          fun (x, y) -> (f x, g y)

  let of_ocaml_mapper item f ctxt x = to_ocaml item x |> f ctxt |> of_ocaml item
  let to_ocaml_mapper item f ctxt x = of_ocaml item x |> f ctxt |> to_ocaml item
end

module Selected_ast = Select_ast (Ocaml)
module Ast_helper = Ast_helper_lite

(* Modules from Ast_<n> of Astlib, where <n> is the compiler version the ppxlib driver is compiled with *)
module Parsetree = Selected_ast.Ast.Parsetree
module Asttypes = Selected_ast.Ast.Asttypes

(* Other Astlib modules *)
module Location = Astlib.Location
module Longident = Astlib.Longident

module Parse = struct
  include Astlib.Parse
  module Of_ocaml = Versions.Convert (Ocaml) (Js)

  let implementation lexbuf = implementation lexbuf |> Of_ocaml.copy_structure
  let interface lexbuf = interface lexbuf |> Of_ocaml.copy_signature

  let toplevel_phrase lexbuf =
    toplevel_phrase lexbuf |> Of_ocaml.copy_toplevel_phrase

  let use_file lexbuf =
    use_file lexbuf |> List.map Of_ocaml.copy_toplevel_phrase

  let core_type lexbuf = core_type lexbuf |> Of_ocaml.copy_core_type
  let expression lexbuf = expression lexbuf |> Of_ocaml.copy_expression
  let pattern lexbuf = pattern lexbuf |> Of_ocaml.copy_pattern
end
