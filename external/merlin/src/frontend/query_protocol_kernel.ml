(** This file declares some types for Query_protocol in a js_of_ocaml-compatible
    library. This enabled jsoo programs to share these type definitions with
    Merlin. *)

module Locate_types_result = struct
  module Tree = struct
    type node_data =
      | Arrow
      | Tuple
      | Unboxed_tuple
      | Object
      | Poly_variant
      | Type_ref of
          { type_ : string
          ; result :
              [ `Found of string option * Lexing.position
              | `Builtin of string
              | `Not_in_env of string
              | `File_not_found of string
              | `Not_found of string * string option
              ]
          }
      | Other of string

    type t =
      { data : node_data
      ; children : t list
      }
  end

  type t =
    | Success of Tree.t
    | Invalid_context
end

module Compl = struct
  type 'desc raw_entry =
    { name : string
    ; kind :
        [ `Value
        | `Constructor
        | `Variant
        | `Label
        | `Module
        | `Modtype
        | `Type
        | `MethodCall
        | `Keyword
        ]
    ; desc : 'desc
    ; info : 'desc
    ; deprecated : bool
    ; ppx_template_generated : bool
    }
end

module Locate_context = struct
  type t =
    | Expr
    | Module_path
    | Module_type
    | Patt
    | Type
    | Constant
    | Constructor
    | Label
    | Unknown
end
