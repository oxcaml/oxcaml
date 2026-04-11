let from_string () =
  Alcotest.(check Testable.yojson)
    __LOC__ Fixtures.json_value
    (Yojson.Safe.from_string Fixtures.json_string)

let from_crlf_string () =
  Alcotest.(check Testable.yojson)
    __LOC__ Fixtures.json_value
    (Yojson.Safe.from_string Fixtures.json_string_crlf)

let parse s () = s |> Yojson.Safe.from_string |> ignore
let parse_basic s () = s |> Yojson.Basic.from_string |> ignore

let from_string_fail_simple () =
  Alcotest.check_raises "Location of parsing failure is correct"
    (Yojson.Json_error "Line 1, bytes 0-5:\nInvalid token 'hello'")
    (parse "hello")

let from_string_fail_lines () =
  Alcotest.check_raises "Location of parsing failure has right line"
    (Yojson.Json_error "Line 3, bytes 0-1:\nExpected ':' but found '}'")
    (parse {|{
      hello
}|})

let from_string_fail_bytes () =
  Alcotest.check_raises "Location has right line and bytes"
    (Yojson.Json_error
       "Line 2, bytes 6-9:\nExpected string or identifier but found '3\n}'")
    (parse {|{
      3
}|})

let from_string_fail_unterminated () =
  Alcotest.check_raises "Runaway string in toplevel"
    (Yojson.Json_error "Line 1, bytes 12-13:\nUnexpected end of input")
    (parse {|"unterminated|})

let from_string_fail_nested_unterminated () =
  Alcotest.check_raises "Runaway string in structure"
    (Yojson.Json_error "Line 2, bytes 5-6:\nUnexpected end of input")
    (parse {|[1,
    "]|})

let from_string_fail_unterminated_structure () =
  Alcotest.check_raises "Array never closed"
    (Yojson.Json_error "Line 1, bytes 0-1:\nUnexpected end of input")
    (parse "[")

let from_string_fail_unstarted_structure () =
  Alcotest.check_raises "Array never opened"
    (Yojson.Json_error "Line 1, bytes 0-1:\nInvalid token ']'") (parse "]")

let from_string_fail_unstarted_object () =
  Alcotest.check_raises "Object never opened"
    (Yojson.Json_error "Line 1, bytes 0-1:\nInvalid token '}'") (parse "}")

let from_string_fail_large_int () =
  Alcotest.check_raises "Too large integer"
    (* TODO: this location wrong, shouldn't be negative *)
    (Yojson.Json_error
       "Line 1, bytes -1-19:\nInt overflow '4611686018427387905'")
    (* 2^62 + 1 *)
    (parse_basic "4611686018427387905")

let from_string_fail_escaped_char () =
  Alcotest.check_raises "Invalid escape sequence"
    (Yojson.Json_error "Line 1, bytes 2-4:\nInvalid escape sequence 'a\"'")
    (parse {|"\a"|})

let from_file () =
  let input_file = Filename.temp_file "test_yojson_from_file" ".json" in
  let oc = open_out input_file in
  output_string oc Fixtures.json_string;
  close_out oc;
  Alcotest.(check Testable.yojson)
    __LOC__ Fixtures.json_value
    (Yojson.Safe.from_file input_file);
  Sys.remove input_file

let unquoted_from_string () =
  Alcotest.(check Testable.yojson)
    __LOC__ Fixtures.unquoted_value
    (Yojson.Safe.from_string Fixtures.unquoted_json)

let map_ident () =
  let lexbuf = Lexing.from_string {|{foo:"hello"}|} in
  let lexer_state = Yojson.init_lexer () in

  let ident_expected expectation reference start len =
    let identifier = String.sub reference start len in
    Alcotest.(check string)
      (Format.asprintf "Reference '%s' start %d len %d matches '%s'" reference
         start len expectation)
      expectation identifier;
    ()
  in
  let skip_over f = f lexer_state lexbuf in
  let map_f mapper f = mapper lexer_state f lexbuf in
  let map_ident = map_f Yojson.Safe.map_ident in

  skip_over Yojson.Safe.read_lcurl;
  map_ident (ident_expected "foo");
  skip_over Yojson.Safe.read_colon;

  let key = skip_over Yojson.Safe.read_string in
  Alcotest.(check string) "String is as expected" "hello" key;

  Alcotest.check_raises "Reading } raises End_of_object" Yojson.End_of_object
    (fun () -> Yojson.Safe.read_object_end lexbuf)

let single_json =
  [
    ("from_string", `Quick, from_string);
    ("from_crlf_string", `Quick, from_crlf_string);
    ("from_string_fail_simple", `Quick, from_string_fail_simple);
    ("from_string_fail_lines", `Quick, from_string_fail_lines);
    ("from_string_fail_bytes", `Quick, from_string_fail_bytes);
    ("from_string_fail_unterminated", `Quick, from_string_fail_unterminated);
    ( "from_string_fail_nested_unterminated",
      `Quick,
      from_string_fail_nested_unterminated );
    ( "from_string_fail_unterminated_structure",
      `Quick,
      from_string_fail_unterminated_structure );
    ( "from_string_fail_unstarted_structure",
      `Quick,
      from_string_fail_unstarted_structure );
    ( "from_string_fail_unstarted_object",
      `Quick,
      from_string_fail_unstarted_object );
    ("from_string_fail_large_int", `Quick, from_string_fail_large_int);
    ("from_string_fail_escaped_char", `Quick, from_string_fail_escaped_char);
    ("from_file", `Quick, from_file);
    ("unquoted_from_string", `Quick, unquoted_from_string);
    ("map_ident", `Quick, map_ident);
  ]
