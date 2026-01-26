type t =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of string
  | `Object of (string * t) list
  | `Array of t list
  ]

let copy_substring chan s ~start ~limit =
  if limit > start then output_substring chan s start (limit - start)

let rec output_escaped_chars chan s ~n ~start ~pos =
  if pos < n
  then
    match String.get s pos with
    | '\b' -> escape chan s ~n ~start ~pos ~e:"\\b"
    | '\t' -> escape chan s ~n ~start ~pos ~e:"\\t"
    | '\n' -> escape chan s ~n ~start ~pos ~e:"\\n"
    | '\012' -> escape chan s ~n ~start ~pos ~e:"\\f"
    | '\r' -> escape chan s ~n ~start ~pos ~e:"\\r"
    | '\\' -> escape chan s ~n ~start ~pos ~e:"\\\\"
    | '"' -> escape chan s ~n ~start ~pos ~e:"\\\""
    | '\000' .. '\031' as c ->
        escape chan s ~n ~start ~pos ~e:(Printf.sprintf "\\u%04x" (Char.code c))
    | '\032' .. '\127' -> output_escaped_chars chan s ~n ~start ~pos:(pos + 1)
    (* Deal with multibyte sequences. *)
    | _ ->
        let decoded = String.get_utf_8_uchar s pos in
        if Uchar.utf_decode_is_valid decoded
        then
          output_escaped_chars
            chan
            s
            ~n
            ~start
            ~pos:(pos + Uchar.utf_decode_length decoded)
        else
          (* Replace each bad byte by the Unicode replacement character (0xFFFD),
           encoded in UTF-8. *)
          escape chan s ~n ~start ~pos ~e:"\xef\xbf\xbd"
  else copy_substring chan s ~start ~limit:pos

and escape chan s ~n ~start ~pos ~e =
  copy_substring chan s ~start ~limit:pos;
  output_string chan e;
  output_escaped_chars chan s ~n ~start:(pos + 1) ~pos:(pos + 1)

let output_quoted_string chan s =
  output_char chan '"';
  output_escaped_chars chan s ~n:(String.length s) ~start:0 ~pos:0;
  output_char chan '"'

let rec output chan (t : t) =
  match t with
  | `Null -> output_string chan "null"
  | `String s -> output_quoted_string chan s
  | `Number f -> output_string chan f
  | `True -> output_string chan "true"
  | `False -> output_string chan "false"
  | `Array l ->
      output_char chan '[';
      output_array_body chan l;
      output_char chan ']'
  | `Object o ->
      output_char chan '{';
      output_object_body chan o;
      output_char chan '}'

and output_array_body chan = function
  | [] -> ()
  | [ x ] -> output chan x
  | x :: xs ->
      output chan x;
      output_char chan ',';
      output_array_body chan xs

and output_object_body chan = function
  | [] -> ()
  | [ (x, y) ] ->
      output_quoted_string chan x;
      output_char chan ':';
      output chan y
  | (x, y) :: xs ->
      output_quoted_string chan x;
      output_char chan ':';
      output chan y;
      output_char chan ',';
      output_object_body chan xs

let write t chan = output chan t
