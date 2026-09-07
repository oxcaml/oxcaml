let to_string_tests =
  let test ?suf expected =
    Alcotest.(check string)
      __LOC__ expected
      (Yojson.Safe.to_string ?suf Fixtures.json_value)
  in
  [
    ( "to_string with default settings",
      `Quick,
      fun () -> test Fixtures.json_string );
    ( "to_string with newline",
      `Quick,
      fun () -> test ~suf:"\n" Fixtures.json_string_newline );
    ( "to_string without newline",
      `Quick,
      fun () -> test ~suf:"" Fixtures.json_string );
  ]

let replace_crlf s =
  (* finds indices of \r\n *)
  let rec find_rn_idx from acc =
    match String.index_from_opt s from '\r' with
    | None ->
        (* no \r left in string *)
        acc
    | Some i -> (
        match s.[i + 1] with
        | exception Invalid_argument _ ->
            (* \r was last in string *)
            acc
        | '\n' -> find_rn_idx (i + 2) (i :: acc)
        | _ -> find_rn_idx (i + 1) acc)
  in
  (* reads backwards to avoid List.rev *)
  let rec cut_parts until acc = function
    | [] ->
        (* last part, read from front *)
        let part = String.sub s 0 until in
        part :: acc
    | i :: idx ->
        let part = String.sub s (i + 2) (until - i - 2) in
        cut_parts i (part :: acc) idx
  in
  find_rn_idx 0 [] |> cut_parts (String.length s) [] |> String.concat "\n"

let to_file_tests =
  let test ?suf expected =
    let output_file = Filename.temp_file "test_yojson_to_file" ".json" in
    Yojson.Safe.to_file ?suf output_file Fixtures.json_value;
    let file_content =
      let ic = open_in_bin output_file in
      let length = in_channel_length ic in
      let s = really_input_string ic length in
      close_in ic;
      replace_crlf s
    in
    Sys.remove output_file;
    Alcotest.(check string) __LOC__ expected file_content
  in
  [
    ( "to_file with default settings",
      `Quick,
      fun () -> test Fixtures.json_string_newline );
    ( "to_file with newline",
      `Quick,
      fun () -> test ~suf:"\n" Fixtures.json_string_newline );
    ( "to_file without newline",
      `Quick,
      fun () -> test ~suf:"" Fixtures.json_string );
  ]

let seq_to_file_tests =
  let test ?suf () =
    let output_file = Filename.temp_file "test_yojson_seq_to_file" ".json" in
    let data = [ `String "foo"; `String "bar" ] in
    Yojson.Safe.seq_to_file ?suf output_file (List.to_seq data);
    let read_data =
      let seq = Yojson.Safe.seq_from_file output_file in
      let acc = ref [] in
      Seq.iter (fun v -> acc := v :: !acc) seq;
      List.rev !acc
    in
    Sys.remove output_file;
    Alcotest.(check (list Testable.yojson))
      "seq_{to,from}_file roundtrip" data read_data
  in
  [
    ("seq_to_file with default settings", `Quick, fun () -> test ());
    ("seq_to_file with newline", `Quick, fun () -> test ~suf:"\n" ());
    ("seq_to_file without newline", `Quick, fun () -> test ~suf:"" ());
  ]

let single_json =
  List.flatten [ to_file_tests; to_string_tests; seq_to_file_tests ]
