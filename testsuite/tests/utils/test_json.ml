(* TEST
 include ocamlcommon;
*)

(* Test that various json values are serialized correctly. *)
let () =
  (* Create string with all 256 possible byte values *)
  let all_bytes_string = String.init 256 (fun i -> Char.chr i) in

  (* Create a long string for testing buffer handling *)
  let long_string = String.make 200 'x' ^ "middle" ^ String.make 200 'y' in

  let test_json : Json.t = Object [

    (* Test metadata *)
    "test_metadata", Object [
      "version", String "1.0";
      "description", String "Json.print test";
      "purpose", String "Regression testing for JSON encoding"
    ];

    (* String test cases *)
    "strings", Object [
      "empty", String "";
      "single_char", String "a";
      "simple_ascii", String "hello world";
      "with_quotes", String "say \"hello\"";
      "with_backslash", String "path\\to\\file";
      "with_newline", String "line1\nline2";
      "with_tab", String "col1\tcol2";
      "with_carriage_return", String "text\rmore";
      "with_backspace", String "text\bback";
      "mixed_escapes", String "\"tab:\t\\n\"";
      "json_like_string", String "{\"key\": \"value\"}";
      "control_chars", String "\000\001\031\127\255";
      "printable_boundaries", String " ~\031\127";
      "long_string", String long_string;
      "all_bytes", String all_bytes_string
    ];

    (* Number test cases *)
    "numbers", Object [
      "zero", Json.int 0;
      "positive_small", Json.int 42;
      "negative_small", Json.int (-17);
      "large_positive", Json.int 1000000;
      "large_negative", Json.int (-999999);
      "max_int", Json.int max_int;
      "min_int", Json.int min_int
    ];

    (* Array test cases *)
    "arrays", Object [
      "empty", Array [];
      "single_string", Array [String "lone"];
      "multiple_strings", Array [
        String "one";
        String "two";
        String "three"
      ];
      "numbers", Array [
        Json.int 1;
        Json.int 2;
        Json.int 3
      ];
      "mixed", Array [
        String "text";
        Json.int 123;
        String "more\ntext"
      ];
      "with_escapes", Array [
        String "normal";
        String "with\tspecial\nchars";
        String "\001control\127chars\255"
      ]
    ];

    (* Nested structure test cases *)
    "nested", Object [
      "object_in_object", Object [
        "inner_field", String "inner_value";
        "inner_number", Json.int 999;
        "inner_escape", String "quotes\"and\nnewlines"
      ];
      "array_of_objects", Array [
        Object [
          "id", Json.int 1;
          "name", String "first\titem"
        ];
        Object [
          "id", Json.int 2;
          "name", String "second\"item"
        ]
      ]
    ];

    (* Edge cases *)
    "edge_cases", Object [
      "field_with_escape_in_name", String "value";
      "field\nwith\tnewline", String "tricky_field_name";
      "unicode_boundaries", String "\031\032\126\127";
      "repeated_escapes", String "\\\\\\\"\\n\\t\\r\\b";
      "empty_vs_null", String ""
    ]

  ] in

  (* Output the complete JSON object *)
  Format.printf "%a@." (Json.print ~mode:Pretty) test_json


(* Test pretty and compact formatting *)
let () =
  let strip_whitespace str =
    String.to_seq str
    |> Seq.filter (fun c -> not (Char.Ascii.is_white c))
    |> String.of_seq in
  let test_json json =
    print_newline ();
    let pretty_json = Format.asprintf "%a" (Json.print ~mode:Pretty) json in
    let compact_json = Format.asprintf "%a" (Json.print ~mode:Compact) json in
    (* Assert that the compact formatting is the same as the pretty formatting,
       but with no whitespace. *)
    let expected_compact_json = strip_whitespace pretty_json in
    if not (String.equal compact_json expected_compact_json)
    then
      (Printf.eprintf "expected: %s\n" expected_compact_json;
      Printf.eprintf "actual: %s\n" compact_json;
      failwith "Compact json representation differs from expected");
    (* Print the pretty formatting *)
    print_endline pretty_json
  in
  test_json (Object []);
  test_json (Object [ "key", String "value" ]);
  test_json (Object (List.init 10 (fun _ -> ("key", Json.String "value"))));
  test_json (Array []);
  test_json (Array [ String "value" ]);
  test_json (Array (List.init 10 (fun _ -> Json.String "value")));
  test_json (Object [
    "name", String "complex";
    "id", Json.int 42;
    "ratio", Json.float 0.25;
    "enabled", True;
    "deleted", False;
    "parent", Null;
    "tags", Array [ String "a"; String "b" ];
    "point", Object [ "x", Json.int 1; "y", Json.int (-2) ];
    "empty_array", Array [];
    "empty_object", Object [];
    "matrix", Array [
      Array [ Json.int 1; Json.int 2; Json.int 3 ];
      Array [];
      Array [ Json.float 1.5; Null; True ]
    ];
    "config", Object [
      "verbose", False;
      "level", Json.int 3;
      "threshold", Json.float 1e-3;
      "output", String "out/\"file\".json";
      "fallback", Json.option None;
      "limits", Object [
        "min", Json.int min_int;
        "max", Json.int max_int;
        "step", Json.option (Some (Json.float 0.5))
      ];
      "plugins", Array [
        Object [ "name", String "lint"; "enabled", True ];
        Object [
          "name", String "format";
          "enabled", False;
          "options", Object [ "width", Json.int 80; "indent", Json.int 2 ]
        ]
      ]
    ]
  ])
