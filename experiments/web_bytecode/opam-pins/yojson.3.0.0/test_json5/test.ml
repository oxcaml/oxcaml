module M = Yojson_five.Safe

let yojson = Alcotest.testable M.pp M.equal

(* any error message will match the string. *)
let any_string = Alcotest.testable Fmt.string (fun _ _ -> true)

let parsing_test_case name error_msg expected input =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (result yojson error_msg))
        name expected (M.from_string input))

let parsing_should_succeed name input expected =
  parsing_test_case name Alcotest.string (Ok expected) input

let parsing_should_fail name input =
  let failure = Error "<anything>" in
  parsing_test_case name any_string failure input

let parsing_should_fail_with_error name input expected =
  parsing_test_case name Alcotest.string (Error expected) input

let parsing_tests =
  [
    parsing_should_fail "Unexpected line break" {|"foo
    bar"|};
    parsing_should_succeed "true" "true" (`Bool true);
    parsing_should_succeed "false" "false" (`Bool false);
    parsing_should_succeed "null" "null" `Null;
    parsing_should_succeed "double quotes string" {|"hello world"|}
      (`String "hello world");
    parsing_should_succeed "single quotes string" {|'hello world'|}
      (`String "hello world");
    parsing_should_succeed "float" "12345.67890" (`Float 12345.67890);
    parsing_should_succeed "hex" "0x1" (`Int 0x1);
    parsing_should_succeed "hex escape sequence" {|"\x61"|} (`String "a");
    parsing_should_succeed "unicode escape sequence" {|"\u03bb"|} (`String "λ");
    parsing_should_succeed "more string escaping"
      "\"Hello \\u03bb \\x77\\x6F\\x72\\x6C\\x64\"" (`String "Hello λ world");
    parsing_should_succeed "escaping quotes" {|"\"\'"|} (`String {|"'|});
    parsing_should_succeed "more escaping quotes" {|'\"\''|} (`String {|"'|});
    parsing_should_succeed "escaping other chars" {|'\A\C\/\D\C'|}
      (`String "AC/DC");
    parsing_should_succeed "built in escapes" {|'\b\n\r\t\f\v'|}
      (`String "\b\n\r\t\012\011");
    parsing_should_succeed "null byte string" {|"\0"|} (`String "\x00");
    parsing_should_succeed "octal string" {|"\077"|} (`String "?");
    parsing_should_succeed "null and octal string" {|"\07"|} (`String "\x007");
    parsing_should_succeed "int" "1" (`Int 1);
    parsing_should_succeed "backslash escape" {|"foo\\bar"|}
      (`String {|foo\bar|});
    parsing_should_succeed "line break" "\"foo\\\nbar\"" (`String "foobar");
    parsing_should_succeed "string and comment" "\"bar\" //foo" (`String "bar");
    (* objects *)
    parsing_should_succeed "empty object" "{}" (`Assoc []);
    parsing_should_succeed "object with double quote string" {|{"foo": "bar"}|}
      (`Assoc [ ("foo", `String "bar") ]);
    parsing_should_succeed "object with single quote string" {|{'foo': 'bar'}|}
      (`Assoc [ ("foo", `String "bar") ]);
    parsing_should_succeed "object with unquoted string" {|{foo: 'bar'}|}
      (`Assoc [ ("foo", `String "bar") ]);
    parsing_should_succeed "trailing comma in object" {|{"one": 1,}|}
      (`Assoc [ ("one", `Int 1) ]);
    parsing_should_succeed "colon in key" {|{"colon:": 1}|}
      (`Assoc [ ("colon:", `Int 1) ]);
    parsing_should_fail "multiple trailing commas in object" {|{"one": 1,,}|};
    parsing_should_fail "just trailing comma in object" "{,}";
    parsing_should_fail "just trailing commas in object" "{,,,}";
    parsing_should_fail "multiple colons in object" {|{one :: 1}|};
    parsing_should_fail "newline in key" {|{new\nline: 1}|};
    (* lists *)
    parsing_should_succeed "empty list" "[]" (`List []);
    parsing_should_succeed "heterogenous list" {|[1, "2", 3.0]|}
      (`List [ `Int 1; `String "2"; `Float 3. ]);
    parsing_should_succeed "trailing comma in list" "[1, 2, 3,]"
      (`List [ `Int 1; `Int 2; `Int 3 ]);
    parsing_should_succeed "trailing comma with space list" "[1, 2, 3, ]"
      (`List [ `Int 1; `Int 2; `Int 3 ]);
    parsing_should_succeed "newlines in list" "[1, 2\n, 3]"
      (`List [ `Int 1; `Int 2; `Int 3 ]);
    parsing_should_fail "multiple trailing commas in list" "[1, 2, 3,,]";
    parsing_should_fail "just trailing comma in list" "[,]";
    parsing_should_fail "multiple trailing commas in list" "[,,,]";
    (* all together *)
    (let expected =
       `Assoc
         [
           ("unquoted", `String "and you can quote me on that");
           ("singleQuotes", `String "I can use \"double quotes\" here");
           ("lineBreaks", `String {|Look, Mom! No \n's!|});
           ("hexadecimal", `Int 0xdecaf);
           ("leadingDecimalPoint", `Float 0.8675309);
           ("andTrailing", `Float 8675309.0);
           ("positiveSign", `Int 1);
           ("trailingComma", `String "in objects");
           ("andIn", `List [ `String "arrays" ]);
           ("backwardsCompatible", `String "with JSON");
         ]
     in
     parsing_should_succeed "More elaborated"
       {|{
  // comments
  unquoted: 'and you can quote me on that',
  singleQuotes: 'I can use "double quotes" here',
 lineBreaks: "Look, Mom! \
No \\n's!",
  hexadecimal: 0xdecaf,
  leadingDecimalPoint: .8675309, andTrailing: 8675309.,
  positiveSign: +1,
  trailingComma: 'in objects', andIn: ['arrays',],
  "backwardsCompatible": "with JSON",
}|}
       expected);
    parsing_should_fail_with_error "unexpected EOF in list" "[1, 2,"
      "Line 1: Unexpected end of input";
    parsing_should_fail_with_error "unexpected EOF on different line" "\n[1, 2,"
      "Line 2: Unexpected end of input";
    parsing_should_fail_with_error "unexpected EOF in assoc" {|{"foo": 1,|}
      "Line 1: Unexpected end of input";
    parsing_should_fail_with_error "missing colon in assoc" {|{"foo"}|}
      "Line 1: Expected ':' but found '}'";
    parsing_should_fail_with_error "bad identifier in assoc" {|{[0]}|}
      "Line 1: Expected string or identifier but found '['";
  ]

let writing_test_case name input expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (M.to_string input))

let writing_tests =
  [
    writing_test_case "Empty object" (`Assoc []) "{}";
    writing_test_case "Empty list" (`List []) "[]";
    writing_test_case "true" (`Bool true) "true";
    writing_test_case "false" (`Bool false) "false";
    writing_test_case "null" `Null "null";
    writing_test_case "string" (`String "hello world") "\"hello world\"";
    writing_test_case "float" (`Float 12345.6789) "12345.6789";
    writing_test_case "hex" (`Int 0x1) "1";
    writing_test_case "int" (`Int 1) "1";
  ]

let () =
  Alcotest.run "JSON5"
    [ ("parsing", parsing_tests); ("writing", writing_tests) ]
