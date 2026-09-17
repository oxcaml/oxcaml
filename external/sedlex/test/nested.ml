let%expect_test _ =
  let lb = Sedlexing.Utf8.from_string "ab" in
  let res =
    match%sedlex lb with
      | 'a' -> ( match%sedlex lb with 'b' -> "ok" | _ -> "error")
      | _ -> "error"
  in
  print_endline res;
  [%expect {| ok |}]
