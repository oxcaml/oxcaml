let () = set_binary_mode_out stdout true
let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]

let print_pos buf =
  let f { Lexing.pos_lnum; pos_bol; pos_cnum; _ } =
    Printf.sprintf "line=%d:bol=%d:cnum=%d" pos_lnum pos_bol pos_cnum
  in
  let f ~prefix (startp, endp) =
    Printf.printf "%s pos: [%s;%s]\n" prefix (f startp) (f endp)
  in
  f ~prefix:"code point" (Sedlexing.lexing_positions buf);
  f ~prefix:"bytes" (Sedlexing.lexing_bytes_positions buf)

let rec token buf =
  match%sedlex buf with
    | number ->
        print_pos buf;
        Printf.printf "Number %s\n" (Sedlexing.Utf8.lexeme buf);
        token buf
    | id_start, Star id_continue ->
        print_pos buf;
        Printf.printf "Ident %s\n" (Sedlexing.Utf8.lexeme buf);
        token buf
    | Plus xml_blank -> token buf
    | Plus (Chars "+*-/") ->
        print_pos buf;
        Printf.printf "Op %s\n" (Sedlexing.Utf8.lexeme buf);
        token buf
    | eof ->
        print_pos buf;
        print_endline "EOF"
    | any ->
        print_pos buf;
        Printf.printf "Any %s\n" (Sedlexing.Utf8.lexeme buf);
        token buf
    | _ -> assert false

let utf16_of_utf8 ?(endian = Sedlexing.Utf16.Big_endian) s =
  let b = Buffer.create (String.length s * 4) in
  let rec loop pos =
    if pos >= String.length s then ()
    else (
      let c = String.get_utf_8_uchar s pos in
      let u = Uchar.utf_decode_uchar c in
      (match endian with
        | Big_endian -> Buffer.add_utf_16be_uchar b u
        | Little_endian -> Buffer.add_utf_16le_uchar b u);
      loop (pos + Uchar.utf_decode_length c))
  in
  loop 0;
  Buffer.contents b

let remove_last s n = String.sub s 0 (String.length s - n)

let gen_from_string s =
  let i = ref 0 in
  fun () ->
    if !i >= String.length s then None
    else (
      let c = String.get s !i in
      incr i;
      Some c)

let channel_from_string s =
  let name, oc = Filename.open_temp_file "" "" in
  output_string oc s;
  close_out oc;
  open_in_bin name

let test_latin s f =
  print_endline "== from_string ==";
  (try f (Sedlexing.Latin1.from_string s)
   with Sedlexing.MalFormed -> print_endline "MalFormed");
  print_endline "== from_gen ==";
  (try f (Sedlexing.Latin1.from_gen (gen_from_string s))
   with Sedlexing.MalFormed -> print_endline "MalFormed");
  print_endline "== from_channel ==";
  try f (Sedlexing.Latin1.from_channel (channel_from_string s))
  with Sedlexing.MalFormed -> print_endline "MalFormed"

let test_utf8 s f =
  print_endline "== from_string ==";
  (try f (Sedlexing.Utf8.from_string s)
   with Sedlexing.MalFormed -> print_endline "MalFormed");
  print_endline "== from_gen ==";
  (try f (Sedlexing.Utf8.from_gen (gen_from_string s))
   with Sedlexing.MalFormed -> print_endline "MalFormed");
  print_endline "== from_channel ==";
  try f (Sedlexing.Utf8.from_channel (channel_from_string s))
  with Sedlexing.MalFormed -> print_endline "MalFormed"

let test_utf16 s bo f =
  print_endline "== from_string ==";
  (try f (Sedlexing.Utf16.from_string s bo)
   with Sedlexing.MalFormed -> print_endline "MalFormed");
  print_endline "== from_gen ==";
  (try f (Sedlexing.Utf16.from_gen (gen_from_string s) bo)
   with Sedlexing.MalFormed -> print_endline "MalFormed");
  print_endline "== from_channel ==";
  try f (Sedlexing.Utf16.from_channel (channel_from_string s) bo)
  with Sedlexing.MalFormed -> print_endline "MalFormed"

let%expect_test "latin1" =
  let s = "asas 123 + 2asd" in
  test_latin s (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    EOF |}];
  let s = "asas 123 + 2\129" in
  test_latin s (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    Any 
    code point pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=13]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    Any 
    code point pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=13]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    Any 
    code point pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=13]
    EOF |}]

let%expect_test "utf8" =
  let s = {|as🎉as 123 + 2asd|} in
  test_utf8 s (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    Ident as
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=3]
    bytes pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=6]
    Any 🎉
    code point pos: [line=1:bol=0:cnum=3;line=1:bol=0:cnum=5]
    bytes pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=8]
    Ident as
    code point pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=9]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=12]
    Number 123
    code point pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=11]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=14]
    Op +
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=16]
    Number 2
    code point pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=16]
    bytes pos: [line=1:bol=0:cnum=16;line=1:bol=0:cnum=19]
    Ident asd
    code point pos: [line=1:bol=0:cnum=16;line=1:bol=0:cnum=16]
    bytes pos: [line=1:bol=0:cnum=19;line=1:bol=0:cnum=19]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    Ident as
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=3]
    bytes pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=6]
    Any 🎉
    code point pos: [line=1:bol=0:cnum=3;line=1:bol=0:cnum=5]
    bytes pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=8]
    Ident as
    code point pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=9]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=12]
    Number 123
    code point pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=11]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=14]
    Op +
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=16]
    Number 2
    code point pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=16]
    bytes pos: [line=1:bol=0:cnum=16;line=1:bol=0:cnum=19]
    Ident asd
    code point pos: [line=1:bol=0:cnum=16;line=1:bol=0:cnum=16]
    bytes pos: [line=1:bol=0:cnum=19;line=1:bol=0:cnum=19]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    Ident as
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=3]
    bytes pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=6]
    Any 🎉
    code point pos: [line=1:bol=0:cnum=3;line=1:bol=0:cnum=5]
    bytes pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=8]
    Ident as
    code point pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=9]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=12]
    Number 123
    code point pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=11]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=14]
    Op +
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=13]
    bytes pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=16]
    Number 2
    code point pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=16]
    bytes pos: [line=1:bol=0:cnum=16;line=1:bol=0:cnum=19]
    Ident asd
    code point pos: [line=1:bol=0:cnum=16;line=1:bol=0:cnum=16]
    bytes pos: [line=1:bol=0:cnum=19;line=1:bol=0:cnum=19]
    EOF |}];
  let s = {|as🎉as 123 + 2|} ^ "\129" in
  test_utf8 s (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    Ident as
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=3]
    bytes pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=6]
    Any 🎉
    code point pos: [line=1:bol=0:cnum=3;line=1:bol=0:cnum=5]
    bytes pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=8]
    Ident as
    code point pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=9]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=12]
    Number 123
    code point pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=11]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=14]
    Op +
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    Ident as
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=3]
    bytes pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=6]
    Any 🎉
    code point pos: [line=1:bol=0:cnum=3;line=1:bol=0:cnum=5]
    bytes pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=8]
    Ident as
    code point pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=9]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=12]
    Number 123
    code point pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=11]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=14]
    Op +
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    Ident as
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=3]
    bytes pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=6]
    Any 🎉
    code point pos: [line=1:bol=0:cnum=3;line=1:bol=0:cnum=5]
    bytes pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=8]
    Ident as
    code point pos: [line=1:bol=0:cnum=6;line=1:bol=0:cnum=9]
    bytes pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=12]
    Number 123
    code point pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=11]
    bytes pos: [line=1:bol=0:cnum=13;line=1:bol=0:cnum=14]
    Op +
    MalFormed |}]

let%expect_test "utf16" =
  let bo = None in
  let s = utf16_of_utf8 "asas 123 + 2asd" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF |}];
  let s = utf16_of_utf8 "asas 123 + 2" ^ "a" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed |}];
  let s1 = "12asd12\u{1F6F3}" in
  let s = utf16_of_utf8 s1 in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF |}];
  test_utf16 (remove_last s 1) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 2) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 3) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 4) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF |}]

let%expect_test "utf16-be" =
  let endian = Sedlexing.Utf16.Big_endian in
  let utf16_of_utf8 = utf16_of_utf8 ~endian in
  let bo = Some endian in
  let s = utf16_of_utf8 "asas 123 + 2asd" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF |}];
  let s = utf16_of_utf8 "asas 123 + 2" ^ "a" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed |}];
  let s1 = "12asd12\u{1F6F3}" in
  let s = utf16_of_utf8 s1 in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF |}];
  test_utf16 (remove_last s 1) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 2) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 3) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 4) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF |}]

let%expect_test "utf16-le" =
  let endian = Sedlexing.Utf16.Little_endian in
  let utf16_of_utf8 = utf16_of_utf8 ~endian in
  let bo = Some endian in
  let s = utf16_of_utf8 "asas 123 + 2asd" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    code point pos: [line=1:bol=0:cnum=11;line=1:bol=0:cnum=12]
    bytes pos: [line=1:bol=0:cnum=22;line=1:bol=0:cnum=24]
    Number 2
    code point pos: [line=1:bol=0:cnum=12;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=24;line=1:bol=0:cnum=30]
    Ident asd
    code point pos: [line=1:bol=0:cnum=15;line=1:bol=0:cnum=15]
    bytes pos: [line=1:bol=0:cnum=30;line=1:bol=0:cnum=30]
    EOF |}];
  let s = utf16_of_utf8 "asas 123 + 2" ^ "a" in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=8]
    Ident asas
    code point pos: [line=1:bol=0:cnum=5;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=10;line=1:bol=0:cnum=16]
    Number 123
    code point pos: [line=1:bol=0:cnum=9;line=1:bol=0:cnum=10]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=20]
    Op +
    MalFormed |}];
  let s1 = "12asd12\u{1F6F3}" in
  let s = utf16_of_utf8 s1 in
  test_utf16 s bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=18]
    Any 🛳
    code point pos: [line=1:bol=0:cnum=8;line=1:bol=0:cnum=8]
    bytes pos: [line=1:bol=0:cnum=18;line=1:bol=0:cnum=18]
    EOF |}];
  test_utf16 (remove_last s 1) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 2) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 3) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    MalFormed |}];
  test_utf16 (remove_last s 4) bo (fun lb -> token lb);
  [%expect
    {|
    == from_string ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF
    == from_gen ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF
    == from_channel ==
    code point pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=2]
    bytes pos: [line=1:bol=0:cnum=0;line=1:bol=0:cnum=4]
    Number 12
    code point pos: [line=1:bol=0:cnum=2;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=4;line=1:bol=0:cnum=14]
    Ident asd12
    code point pos: [line=1:bol=0:cnum=7;line=1:bol=0:cnum=7]
    bytes pos: [line=1:bol=0:cnum=14;line=1:bol=0:cnum=14]
    EOF |}]
