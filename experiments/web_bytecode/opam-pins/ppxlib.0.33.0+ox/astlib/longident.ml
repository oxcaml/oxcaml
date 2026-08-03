type t =
  | Lident of string
  | Ldot of t * string
  | Lapply of t * t

let rec flatten = function
  | Lident name -> [name]
  | Ldot (prefix, name) -> flatten prefix @ [name]
  | Lapply _ -> invalid_arg "Longident.flatten"

let parse source =
  match String.split_on_char '.' source with
  | [] -> invalid_arg "Longident.parse"
  | head :: tail -> List.fold_left (fun path name -> Ldot (path, name)) (Lident head) tail
