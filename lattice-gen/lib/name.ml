let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let is_module_name name =
  let len = String.length name in
  len > 0
  &&
  match name.[0] with
  | 'A' .. 'Z' -> String.for_all is_ident_char name
  | _ -> false

let is_value_name name =
  let len = String.length name in
  len > 0
  &&
  match name.[0] with
  | 'a' .. 'z' | '_' -> String.for_all is_ident_char name
  | _ -> false

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false

let snake_case name =
  let buf = Buffer.create (String.length name * 2) in
  let len = String.length name in
  for i = 0 to len - 1 do
    let c = name.[i] in
    if is_uppercase c
    then (
      let needs_underscore =
        i > 0
        &&
        let prev = name.[i - 1] in
        is_lowercase prev
        || (is_uppercase prev
           && i + 1 < len
           && is_lowercase name.[i + 1])
        || (match prev with
           | '0' .. '9' -> true
           | _ -> false)
      in
      if needs_underscore then Buffer.add_char buf '_';
      Buffer.add_char buf (Char.lowercase_ascii c))
    else Buffer.add_char buf (Char.lowercase_ascii c)
  done;
  Buffer.contents buf

let op_module_name name = name ^ "_op"

let embedding_module_name small big = small ^ "_in_" ^ big
