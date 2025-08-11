type t =
  | Path
  | Directory_index
  | Timestamp
  | Size
  | MD5
  | Lo_user
  | Hi_user

let of_int_exn = function
  | 0x1 -> Path
  | 0x2 -> Directory_index
  | 0x3 -> Timestamp
  | 0x4 -> Size
  | 0x5 -> MD5
  | 0x2000->Lo_user
  | 0x3fff->Hi_user
  | w    -> failwith (Printf.sprintf "invalid content type 0x%x" w)
