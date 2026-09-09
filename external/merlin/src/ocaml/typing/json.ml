exception Malformed of string

type t =
  | Null
  | Bool of bool
  | Number of string
  | String of string
  | Array of t list
  | Object of (string * t) list

let parse text =
  let length = String.length text in
  let position = ref 0 in
  let fail message =
    raise (Malformed (Printf.sprintf "at byte %d: %s" !position message))
  in
  let peek () = if !position < length then Some text.[!position] else None in
  let peek_ahead offset =
    let position = !position + offset in
    if position < length then Some text.[position] else None
  in
  let advance () = incr position in
  let rec skip_whitespace () =
    match peek () with
    | Some (' ' | '\t' | '\r' | '\n') ->
      advance ();
      skip_whitespace ()
    | Some _ | None -> ()
  in
  let expect character =
    match peek () with
    | Some found when Char.equal found character -> advance ()
    | Some found ->
      fail (Printf.sprintf "expected %c, found %c" character found)
    | None ->
      fail (Printf.sprintf "expected %c, found end of input" character)
  in
  let hexadecimal character =
    match character with
    | '0' .. '9' -> Char.code character - Char.code '0'
    | 'a' .. 'f' -> Char.code character - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code character - Char.code 'A' + 10
    | _ -> fail "expected a hexadecimal digit"
  in
  let code_unit () =
    let code = ref 0 in
    for _ = 1 to 4 do
      match peek () with
      | None -> fail "unterminated escape"
      | Some digit ->
        code := (!code * 16) + hexadecimal digit;
        advance ()
    done;
    !code
  in
  let is_leading_surrogate code = code >= 0xd800 && code <= 0xdbff in
  let is_trailing_surrogate code = code >= 0xdc00 && code <= 0xdfff in
  let scalar_value () =
    let code = code_unit () in
    if is_trailing_surrogate code then fail "lone trailing surrogate";
    if not (is_leading_surrogate code) then code
    else begin
      (match peek (), peek_ahead 1 with
      | Some '\\', Some 'u' -> position := !position + 2
      | _ -> fail "lone leading surrogate");
      let trailing = code_unit () in
      if not (is_trailing_surrogate trailing) then
        fail "expected a trailing surrogate";
      0x10000 + ((code - 0xd800) * 0x400) + (trailing - 0xdc00)
    end
  in
  let parse_string () =
    expect '"';
    let buffer = Buffer.create 32 in
    let rec loop () =
      match peek () with
      | None -> fail "unterminated string"
      | Some ('\000' .. '\031') ->
        fail "unescaped control character in string"
      | Some '"' ->
        advance ();
        Buffer.contents buffer
      | Some '\\' ->
        advance ();
        (match peek () with
        | None -> fail "unterminated escape"
        | Some 'u' ->
          advance ();
          Buffer.add_utf_8_uchar buffer (Uchar.of_int (scalar_value ()))
        | Some escaped ->
          advance ();
          Buffer.add_char buffer
            (match escaped with
            | 'n' -> '\n'
            | 't' -> '\t'
            | 'r' -> '\r'
            | 'b' -> '\b'
            | 'f' -> '\012'
            | '"' -> '"'
            | '\\' -> '\\'
            | '/' -> '/'
            | _ -> fail "unknown escape"));
        loop ()
      | Some character ->
        advance ();
        Buffer.add_char buffer character;
        loop ()
    in
    loop ()
  in
  let parse_literal spelling value =
    let width = String.length spelling in
    let stop = !position + width in
    if stop > length
       || not (String.equal (String.sub text !position width) spelling)
    then fail (Printf.sprintf "expected %s" spelling);
    position := stop;
    value
  in
  let parse_number () =
    let start = !position in
    let rec loop () =
      match peek () with
      | Some ('-' | '+' | '.' | 'e' | 'E' | '0' .. '9') ->
        advance ();
        loop ()
      | Some _ | None -> ()
    in
    loop ();
    if Int.equal !position start then fail "expected a number";
    Number (String.sub text start (!position - start))
  in
  let rec parse_value () =
    skip_whitespace ();
    match peek () with
    | None -> fail "expected a value"
    | Some '"' -> String (parse_string ())
    | Some '{' -> parse_object ()
    | Some '[' -> parse_array ()
    | Some 't' -> parse_literal "true" (Bool true)
    | Some 'f' -> parse_literal "false" (Bool false)
    | Some 'n' -> parse_literal "null" Null
    | Some _ -> parse_number ()
  and parse_object () =
    expect '{';
    skip_whitespace ();
    match peek () with
    | Some '}' ->
      advance ();
      Object []
    | Some _ | None ->
      let rec loop fields =
        skip_whitespace ();
        let name = parse_string () in
        skip_whitespace ();
        expect ':';
        let value = parse_value () in
        let fields = (name, value) :: fields in
        skip_whitespace ();
        match peek () with
        | Some ',' ->
          advance ();
          loop fields
        | Some '}' ->
          advance ();
          Object (List.rev fields)
        | Some _ | None -> fail "expected , or } in object"
      in
      loop []
  and parse_array () =
    expect '[';
    skip_whitespace ();
    match peek () with
    | Some ']' ->
      advance ();
      Array []
    | Some _ | None ->
      let rec loop items =
        let items = parse_value () :: items in
        skip_whitespace ();
        match peek () with
        | Some ',' ->
          advance ();
          loop items
        | Some ']' ->
          advance ();
          Array (List.rev items)
        | Some _ | None -> fail "expected , or ] in array"
      in
      loop []
  in
  let value = parse_value () in
  skip_whitespace ();
  if not (Int.equal !position length) then fail "unexpected trailing input";
  value

let add_escaped_string buffer value =
  let width = String.length value in
  let rec add index =
    if index < width then
      match String.get value index with
      | '"' ->
        Buffer.add_string buffer "\\\"";
        add (index + 1)
      | '\\' ->
        Buffer.add_string buffer "\\\\";
        add (index + 1)
      | '\b' ->
        Buffer.add_string buffer "\\b";
        add (index + 1)
      | '\012' ->
        Buffer.add_string buffer "\\f";
        add (index + 1)
      | '\n' ->
        Buffer.add_string buffer "\\n";
        add (index + 1)
      | '\r' ->
        Buffer.add_string buffer "\\r";
        add (index + 1)
      | '\t' ->
        Buffer.add_string buffer "\\t";
        add (index + 1)
      | '\000' .. '\031' as control ->
        Buffer.add_string buffer
          (Printf.sprintf "\\u%04x" (Char.code control));
        add (index + 1)
      | ' ' .. '\127' as ascii ->
        Buffer.add_char buffer ascii;
        add (index + 1)
      | _ ->
        let decoded = String.get_utf_8_uchar value index in
        let bytes = Uchar.utf_decode_length decoded in
        if Uchar.utf_decode_is_valid decoded then
          Buffer.add_substring buffer value index bytes
        else Buffer.add_utf_8_uchar buffer Uchar.rep;
        add (index + bytes)
  in
  Buffer.add_char buffer '"';
  add 0;
  Buffer.add_char buffer '"'

let to_string json =
  let buffer = Buffer.create 1024 in
  let rec add json =
    match json with
    | Null -> Buffer.add_string buffer "null"
    | Bool true -> Buffer.add_string buffer "true"
    | Bool false -> Buffer.add_string buffer "false"
    | Number number -> Buffer.add_string buffer number
    | String value -> add_escaped_string buffer value
    | Array items ->
      Buffer.add_char buffer '[';
      List.iteri
        (fun index item ->
          if index > 0 then Buffer.add_char buffer ',';
          add item)
        items;
      Buffer.add_char buffer ']'
    | Object fields ->
      Buffer.add_char buffer '{';
      List.iteri
        (fun index (name, value) ->
          if index > 0 then Buffer.add_char buffer ',';
          add_escaped_string buffer name;
          Buffer.add_char buffer ':';
          add value)
        fields;
      Buffer.add_char buffer '}'
  in
  add json;
  Buffer.contents buffer

let malformed format =
  Printf.ksprintf (fun message -> raise (Malformed message)) format

let object_fields = function
  | Object fields -> fields
  | _ -> malformed "expected an object"

let field name json =
  match List.assoc_opt name (object_fields json) with
  | Some value -> value
  | None -> malformed "missing field %S" name

let optional_field name json = List.assoc_opt name (object_fields json)

let string = function
  | String string -> string
  | _ -> malformed "expected a string"

let int = function
  | Number number -> (
    match int_of_string_opt number with
    | Some int -> int
    | None -> malformed "expected an integer, found %S" number)
  | _ -> malformed "expected an integer"

let array = function
  | Array values -> values
  | _ -> malformed "expected an array"
