type t =
  | Assoc of (string * t) list
  | List of t list
  | StringLit of string
  | IntLit of string
  | FloatLit of string
  | Bool of bool
  | Null

let rec to_basic = function
  | Assoc l -> `Assoc (List.map (fun (name, obj) -> (name, to_basic obj)) l)
  | List l -> `List (List.map to_basic l)
  | StringLit s -> `String s
  | FloatLit s -> `Float (float_of_string s)
  | IntLit s -> `Int (int_of_string s)
  | Bool b -> `Bool b
  | Null -> `Null

let rec to_safe = function
  | Assoc l -> `Assoc (List.map (fun (name, obj) -> (name, to_safe obj)) l)
  | List l -> `List (List.map to_safe l)
  | StringLit s -> `String s
  | FloatLit s -> `Float (float_of_string s)
  | IntLit s -> (
      match int_of_string_opt s with Some i -> `Int i | None -> `Intlit s)
  | Bool b -> `Bool b
  | Null -> `Null
