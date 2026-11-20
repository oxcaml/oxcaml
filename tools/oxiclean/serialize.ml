(* Serialize a Lean constructor to JSON by tagging its constructor name. *)
let tag (lean_ctor_name : string) (contents : Json.t) : Json.t =
  Json.Object [(lean_ctor_name, contents)]

let string (s : string) : Json.t = Json.String s

let array (f : 'a -> Json.t) (l : 'a list) : Json.t =
  Json.Array (List.map f l)

let option (f : 'a -> Json.t) : 'a option -> Json.t = function
  | None -> Json.Null
  | Some a -> f a

let global_module_name (name : Global_module.Name.t) : Json.t =
  name |> Global_module.Name.to_string |> string

let rec impl : Model.Expr.t -> Json.t = function
  | Apply { func; arg } ->
    Json.Object
      [ "func", impl func
      ; "arg", impl arg
      ]
    |> tag "apply"
  | Block { tag = i; fields } ->
    Json.Object
      [ "tag", Json.int i
      ; "fields", array impl fields
      ]
    |> tag "block"
  | Field { within; index } ->
    Json.Object
      [ "indexInto", impl within
      ; "index", Json.int index
      ]
    |> tag "field"
  | Function { body } -> impl body |> tag "body" |> tag "function"
  | Let_binding { bound; body } ->
    Json.Object [ "bound", impl bound; "body", impl body ] |> tag "letBinding"
  | Physical_equality { lhs; rhs } ->
    Json.Object [ "lhs", impl lhs; "rhs", impl rhs ]
    |> tag "physicalEquality"
  | Scalar_operation { operation } ->
    scalar_operation operation |> tag "operation" |> tag "scalarOperation"
  | Switch { scrutinee; n_consts; n_blocks; consts; blocks; failaction } ->
    Json.Object
      [ "scrutinee", impl scrutinee
      ; "nImmediates", Json.int n_consts
      ; "nBlocks", Json.int n_blocks
      ; "immediates", array immediate_switch_case consts
      ; "blocks", array block_switch_case blocks
      ; "failAction", option impl failaction
      ]
    |> tag "switch"
  | Compilation_unit { name } ->
    global_module_name name
    |> tag "name"
    |> tag "compilationUnit"
  | Value { value = v } ->
    value v |> tag "value" |> tag "val"
  | Var { de_bruijn } ->
    Json.int de_bruijn |> tag "deBruijn" |> tag "var"

and value : Model.Value.t -> Json.t = function
  | Immediate { bits_above_lsb } ->
    Json.int bits_above_lsb
    |> tag "bitsAboveLsb"
    |> tag "imm"
    |> tag "immediate"
  | Indirection { indirection } ->
    begin match indirection with
    | Immutable { block } ->
      begin match block with
      | { tag = i; contents } ->
        let contents =
          begin match contents with
          | Values { values } ->
            array value values
            |> tag "fields"
            |> tag "values"
          | Bytes { bytes } ->
            array Json.int bytes
            |> tag "data"
            |> tag "bytes"
          | Closure { body } -> impl body |> tag "body" |> tag "closure"
          end
        in
        Json.Object
          [ "tag", Json.int i
          ; "contents", contents
          ]
      end
      |> tag "block"
      |> tag "noMutableFields"
    end
    |> tag "ptr"
    |> tag "indirection"

and immediate_switch_case
    ({ case; body_if_matched } : Model.Expr.switch_case) : Json.t =
  Json.Object
    [ "immediate", Json.int case |> tag "bitsAboveLsb"
    ; "body", impl body_if_matched
    ]

and block_switch_case
    ({ case; body_if_matched } : Model.Expr.switch_case) : Json.t =
  Json.Object
    [ "tag", Json.int case
    ; "body", impl body_if_matched
    ]

and scalar_operation : Model.Expr.Scalar_operation.t -> Json.t = function
  | Binary { operation; lhs; rhs } ->
    Json.Object
      [ "operation", binary_scalar_operation operation
      ; "lhs", impl lhs
      ; "rhs", impl rhs
      ]
    |> tag "binary"

and binary_scalar_operation
    : Model.Expr.Scalar_operation.Binary.t -> Json.t =
  function
  | Integral operation ->
    integral_binary_scalar_operation operation
    |> tag "integral"
    |> tag "integral"

and integral_binary_scalar_operation
    : Model.Expr.Scalar_operation.Binary.Integral.t -> Json.t =
  function
  | Mod -> Json.String "modulo"

let predef : Model.predef -> Json.t = function
  | Bool -> Json.String "bool"
  | Int -> Json.String "int"
  | Option -> Json.String "option"

let module_statics_field (within : Json.t) (index : int) : Json.t =
  Json.Object [ "within", within; "index", Json.int index ] |> tag "field"

let rec module_statics : Model.module_statics -> Json.t = function
  | Compilation_unit { name } ->
    global_module_name name
    |> tag "name"
    |> tag "compilationUnit"
  | Module_var { de_bruijn } ->
    Json.int de_bruijn |> tag "deBruijn" |> tag "var"
  | Module_field { within; index } ->
    module_statics_field (module_statics within) index

let type_constr : Model.type_constr -> Json.t = function
  | Type_constr_field { within; index } ->
    module_statics_field (module_statics within) index
  | Predef p -> predef p |> tag "predef" |> tag "predef"

let rec module_type ({ absent; present } : Model.module_type) : Json.t =
  Json.Object
    [ "absent", array module_item_absent_at_runtime absent
    ; "present", array module_item_present_at_runtime present
    ]

and type_decl ({ manifest; arity } : Model.type_decl) : Json.t =
  Json.Object
    [ "manifest", option val_type manifest
    ; "arity", Json.int arity
    ]

and module_item_absent_at_runtime
    : Model.module_item_absent_at_runtime -> Json.t = function
  | Module_type m ->
    option module_type m
    |> tag "moduleTypeDecl"
    |> tag "moduleType"
  | Val_type t -> type_decl t |> tag "typeDecl" |> tag "valType"

and module_item_present_at_runtime
    : Model.module_item_present_at_runtime -> Json.t = function
  | Module m -> module_type m |> tag "moduleType" |> tag "mod"
  | Val v -> val_type v |> tag "valType" |> tag "val"

and val_type : Model.val_type -> Json.t = function
  | Arrow { domain; codomain } ->
    Json.Object
      [ "domain", val_type domain
      ; "codomain", val_type codomain
      ]
    |> tag "arrow"
  | Constr { constr; args } ->
    Json.Object
      [ "constructor", type_constr constr
      ; "args", array val_type args
      ]
    |> tag "constr"
  | First_class_module { signature } ->
    module_type signature |> tag "signature" |> tag "firstClassModule"
  | Poly { body } -> val_type body |> tag "body" |> tag "poly"
  | Type_var { de_bruijn } -> Json.int de_bruijn |> tag "deBruijn" |> tag "var"

let intf = module_type
