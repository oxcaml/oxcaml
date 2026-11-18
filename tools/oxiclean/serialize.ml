(* Serialize a Lean constructor to JSON by tagging its constructor name. *)
let tag (lean_ctor_name : string) (contents : Json.t) : Json.t =
  Json.Object [(lean_ctor_name, contents)]

let string (s : string) : Json.t = Json.String s

let bool (b : bool) : Json.t = if b then Json.True else Json.False

let array (f : 'a -> Json.t) (l : 'a list) : Json.t =
  Json.Array (List.map f l)

let option (f : 'a -> Json.t) : 'a option -> Json.t = function
  | None -> Json.Null
  | Some a -> f a

let intf () = Json.Null

let rec impl : Model.Expression.t -> Json.t = function
  | Apply { func; args } ->
    Json.Object
      [ "func", impl func
      ; "args", array impl args
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
      [ "block", impl block
      ; "index", Json.int index
      ]
    |> tag "field"
  | Function c -> closure c |> tag "function"
  | LetBinding { bound; body } ->
    Json.Object [ "bound", impl bound; "body", impl body ] |> tag "letBinding"
  | PhysicalEquality { negated; lhs; rhs } ->
    Json.Object [ "negated", bool negated; "lhs", impl lhs; "rhs", impl rhs ]
    |> tag "physicalEquality"
  | ScalarOperation { operation } ->
    scalar_operation operation |> tag "operation" |> tag "scalarOperation"
  | Switch { scrutinee; n_consts; n_blocks; consts; blocks; failaction } ->
    Json.Object
      [ "scrutinee", impl scrutinee
      ; "nConsts", Json.int n_consts
      ; "nBlocks", Json.int n_blocks
      ; "consts", array switch_case consts
      ; "blocks", array switch_case blocks
      ; "failaction", option impl failaction
      ]
    |> tag "switch"
  | UpstreamCompilationUnit { path } ->
    array string path |> tag "path" |> tag "upstreamCompilationUnit"
  | Value { value = v } ->
    value v |> tag "value" |> tag "value"
  | Var { de_bruijn } ->
    Json.int de_bruijn |> tag "deBruijn" |> tag "var"

and closure ({ n_params; body } : Model.Expression.closure) : Json.t =
  Json.Object
    [ "nParams", Json.int n_params
    ; "body", impl body
    ]

and value : Model.Value.t -> Json.t = function
  | Immediate { bits_above_lsb } ->
    Json.int bits_above_lsb |> tag "bitsAboveLsb" |> tag "immediate"
  | Indirection { indirection } ->
    begin match indirection with
    | Immutable { block } ->
      begin match block with
      | { tag = i; contents } ->
        let contents =
          begin match contents with
          | Values { values } ->
            array value values
            |> tag "values"
            |> tag "values"
          | Bytes { bytes } ->
            array Json.int bytes
            |> tag "bytes"
            |> tag "bytes"
          | Closure c -> closure c |> tag "closure"
          end
        in
        Json.Object
          [ "tag", Json.int i
          ; "contents", contents
          ]
      end
      |> tag "block"
      |> tag "immutable"
    end
    |> tag "indirection"
    |> tag "indirection"

and switch_case ({ case; body_if_matched } : Model.Expression.switch_case) : Json.t =
  Json.Object
    [ "case", Json.int case
    ; "bodyIfMatched", impl body_if_matched
    ]

and scalar_operation : Model.Expression.Scalar_operation.t -> Json.t = function
  | Binary { operation; lhs; rhs } ->
    Json.Object
      [ "operation", binary_scalar_operation operation
      ; "lhs", impl lhs
      ; "rhs", impl rhs
      ]
    |> tag "binary"

and binary_scalar_operation
    : Model.Expression.Scalar_operation.Binary.t -> Json.t =
  function
  | Integral operation ->
    integral_binary_scalar_operation operation
    |> tag "operation"
    |> tag "integral"

and integral_binary_scalar_operation
    : Model.Expression.Scalar_operation.Binary.Integral.t -> Json.t =
  function
  | Mod -> Json.String "mod"
