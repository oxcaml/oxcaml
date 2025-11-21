(* TEST
 include config;
 binary_modules = "config build_path_prefix_map iarray_shim misc";
 bytecode;
*)

let () =
  (* Create string with all 256 possible byte values *)
  let all_bytes_string = String.init 256 (fun i -> Char.chr i) in

  (* Create a long string for testing buffer handling *)
  let long_string = String.make 200 'x' ^ "middle" ^ String.make 200 'y' in

  let test_json = Misc.Json.object_ [

    (* Test metadata *)
    Misc.Json.field "test_metadata" (Misc.Json.object_ [
      Misc.Json.field "version" (Misc.Json.string "1.0");
      Misc.Json.field "description"
        (Misc.Json.string "Json.escape_unicode test");
      Misc.Json.field "purpose"
        (Misc.Json.string "Regression testing for JSON encoding")
    ]);

    (* String test cases *)
    Misc.Json.field "strings" (Misc.Json.object_ [
      Misc.Json.field "empty" (Misc.Json.string "");
      Misc.Json.field "single_char" (Misc.Json.string "a");
      Misc.Json.field "simple_ascii" (Misc.Json.string "hello world");
      Misc.Json.field "with_quotes" (Misc.Json.string "say \"hello\"");
      Misc.Json.field "with_backslash" (Misc.Json.string "path\\to\\file");
      Misc.Json.field "with_newline" (Misc.Json.string "line1\nline2");
      Misc.Json.field "with_tab" (Misc.Json.string "col1\tcol2");
      Misc.Json.field "with_carriage_return" (Misc.Json.string "text\rmore");
      Misc.Json.field "with_backspace" (Misc.Json.string "text\bback");
      Misc.Json.field "mixed_escapes" (Misc.Json.string "\"tab:\t\\n\"");
      Misc.Json.field "json_like_string"
        (Misc.Json.string "{\"key\": \"value\"}");
      Misc.Json.field "control_chars" (Misc.Json.string "\000\001\031\127\255");
      Misc.Json.field "printable_boundaries" (Misc.Json.string " ~\031\127");
      Misc.Json.field "long_string" (Misc.Json.string long_string);
      Misc.Json.field "all_bytes" (Misc.Json.string all_bytes_string)
    ]);

    (* Number test cases *)
    Misc.Json.field "numbers" (Misc.Json.object_ [
      Misc.Json.field "zero" (Misc.Json.int 0);
      Misc.Json.field "positive_small" (Misc.Json.int 42);
      Misc.Json.field "negative_small" (Misc.Json.int (-17));
      Misc.Json.field "large_positive" (Misc.Json.int 1000000);
      Misc.Json.field "large_negative" (Misc.Json.int (-999999));
      Misc.Json.field "max_int" (Misc.Json.int max_int);
      Misc.Json.field "min_int" (Misc.Json.int min_int)
    ]);

    (* Array test cases *)
    Misc.Json.field "arrays" (Misc.Json.object_ [
      Misc.Json.field "empty" (Misc.Json.array []);
      Misc.Json.field "single_string"
        (Misc.Json.array [Misc.Json.string "lone"]);
      Misc.Json.field "multiple_strings" (Misc.Json.array [
        Misc.Json.string "one";
        Misc.Json.string "two";
        Misc.Json.string "three"
      ]);
      Misc.Json.field "numbers" (Misc.Json.array [
        Misc.Json.int 1;
        Misc.Json.int 2;
        Misc.Json.int 3
      ]);
      Misc.Json.field "mixed" (Misc.Json.array [
        Misc.Json.string "text";
        Misc.Json.int 123;
        Misc.Json.string "more\ntext"
      ]);
      Misc.Json.field "with_escapes" (Misc.Json.array [
        Misc.Json.string "normal";
        Misc.Json.string "with\tspecial\nchars";
        Misc.Json.string "\001control\127chars\255"
      ])
    ]);

    (* Nested structure test cases *)
    Misc.Json.field "nested" (Misc.Json.object_ [
      Misc.Json.field "object_in_object" (Misc.Json.object_ [
        Misc.Json.field "inner_field" (Misc.Json.string "inner_value");
        Misc.Json.field "inner_number" (Misc.Json.int 999);
        Misc.Json.field "inner_escape"
          (Misc.Json.string "quotes\"and\nnewlines")
      ]);
      Misc.Json.field "array_of_objects" (Misc.Json.array [
        Misc.Json.object_ [
          Misc.Json.field "id" (Misc.Json.int 1);
          Misc.Json.field "name" (Misc.Json.string "first\titem")
        ];
        Misc.Json.object_ [
          Misc.Json.field "id" (Misc.Json.int 2);
          Misc.Json.field "name" (Misc.Json.string "second\"item")
        ]
      ])
    ]);

    (* Edge cases *)
    Misc.Json.field "edge_cases" (Misc.Json.object_ [
      Misc.Json.field "field_with_escape_in_name" (Misc.Json.string "value");
      Misc.Json.field ("field\nwith\tnewline")
        (Misc.Json.string "tricky_field_name");
      Misc.Json.field "unicode_boundaries"
        (Misc.Json.string "\031\032\126\127");
      Misc.Json.field "repeated_escapes"
        (Misc.Json.string "\\\\\\\"\\n\\t\\r\\b");
      Misc.Json.field "empty_vs_null" (Misc.Json.string "")
    ])

  ] in

  (* Output the complete JSON object *)
  Printf.printf "%s\n" test_json
