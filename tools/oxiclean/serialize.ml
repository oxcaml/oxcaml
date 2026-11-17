(* Serialize a Lean constructor to JSON by tagging its constructor name. *)
let tag (lean_ctor_name : string) (contents : Json.t) : Json.t =
  Json.Object [(lean_ctor_name, contents)]

let string (s : string) : Json.t = Json.String s

let array (f : 'a -> Json.t) (l : 'a list) : Json.t =
  Json.Array (List.map f l)

(*
(* Serialize an [option]. *)
let serialize_option (f : 'a -> Json.t) : 'a option -> Json.t = function
  | None -> Json.Null
  | Some a -> f a
*)

let intf () = Json.Null

let rec impl : Model.Impl.t -> Json.t = function
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
  | LetBinding { bound; body } ->
    Json.Object [ "bound", impl bound; "body", impl body ] |> tag "letBinding"
  | UpstreamCompilationUnit { path } ->
    array string path |> tag "path" |> tag "upstreamCompilationUnit"
  | Var { de_bruijn } ->
    Json.int de_bruijn |> tag "deBruijn" |> tag "var"
