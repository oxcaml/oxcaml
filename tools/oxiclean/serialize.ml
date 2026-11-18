(* Serialize a Lean constructor to JSON by tagging its constructor name. *)
let tag (lean_ctor_name : string) (contents : Json.t) : Json.t =
  Json.Object [(lean_ctor_name, contents)]

let string (s : string) : Json.t = Json.String s

let array (f : 'a -> Json.t) (l : 'a list) : Json.t =
  Json.Array (List.map f l)

let option (f : 'a -> Json.t) : 'a option -> Json.t = function
  | None -> Json.Null
  | Some a -> f a

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
  | Field { block; index } ->
    Json.Object
      [ "indexInto", impl block
      ; "index", Json.int index
      ]
    |> tag "field"
  | Function { body } -> impl body |> tag "body" |> tag "function"
  | LetBinding { bound; body } ->
    Json.Object [ "bound", impl bound; "body", impl body ] |> tag "letBinding"
  | PhysicalEquality { lhs; rhs } ->
    Json.Object [ "lhs", impl lhs; "rhs", impl rhs ]
    |> tag "physicalEquality"
  | ScalarOperation { operation } ->
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
  | UpstreamCompilationUnit { name } ->
    string name |> tag "name" |> tag "upstreamCompilationUnit"
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

let type_predef : Model.Type.predef -> Json.t = function
  | Bool -> Json.String "bool"
  | Int -> Json.String "int"
  | Option -> Json.String "option"

let type_constr : Model.Type.constr -> Json.t = function
  | Predef p -> type_predef p |> tag "predef" |> tag "predef"
  | Var { de_bruijn } ->
    Json.int de_bruijn |> tag "deBruijn" |> tag "var"

let rec type_expr : Model.Type.expr -> Json.t = function
  | Arrow { domain; codomain } ->
    Json.Object
      [ "domain", type_expr domain
      ; "codomain", type_expr codomain
      ]
    |> tag "arrow"
  | Constr { constr; args } ->
    Json.Object
      [ "constructor", type_constr constr
      ; "args", array type_expr args
      ]
    |> tag "constr"
  | Poly { body } ->
    type_expr body
    |> tag "body"
    |> tag "poly"
  | TypeVar { de_bruijn } ->
    Json.int de_bruijn
    |> tag "deBruijn"
    |> tag "var"

let type_kind : Model.Type.kind -> Json.t = function
  | Abstract -> Json.String "abstract"

let rec intf : Model.Type.t -> Json.t = function
  | EmptyModule -> Json.String "empty"
  | Module { ty; etc } ->
    Json.Object
      [ "moduleType", intf ty
      ; "etc", intf etc
      ]
    |> tag "mod"
  | Type { kind; etc } ->
    Json.Object
      [ "kind", type_kind kind
      ; "etc", intf etc
      ]
    |> tag "ty"
  | Val { ty; etc } ->
    Json.Object
      [ "exprType", type_expr ty
      ; "etc", intf etc
      ]
    |> tag "val"
