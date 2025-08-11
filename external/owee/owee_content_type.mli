open Owee_buf

type t = private
  | Path
  | Directory_index
  | Timestamp
  | Size
  | MD5
  | Lo_user
  | Hi_user


val of_int_exn : u128 -> t
