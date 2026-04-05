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

let ocaml_keywords =
  [ "and";
    "as";
    "assert";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "lazy";
    "let";
    "match";
    "method";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with"
  ]

let escape_value_name name =
  if List.mem name ocaml_keywords then name ^ "_" else name

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

let snake_case_value_name name = escape_value_name (snake_case name)

let op_module_name name = name ^ "_op"

let embedding_module_name small big = small ^ "_in_" ^ big
