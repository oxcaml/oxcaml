open Printf

let next_tok buf =
  let open Sedlexing.Utf8 in
  match%sedlex buf with
    | "a", Utf8 (Chars "+-×÷") -> sprintf "with Chars: %s" (lexeme buf)
    | "b", Utf8 ("+" | "-" | "×" | "÷") ->
        sprintf "with or_pattern: %s" (lexeme buf)
    | Latin1 "\xc0", Utf8 "À", Ascii (Utf8 (Latin1 (Utf8 (Chars "À")))) ->
        sprintf "mixed encoding: %s" (lexeme buf)
    | Ascii (Star '\x00' .. '\x7f') -> sprintf "only ascii: %s" (lexeme buf)
    | Utf8 (Star '\x00' .. '\x7f') ->
        assert false
        (* utf8 char interval can only match ascii because of the OCaml lexer. The regexp above should match instead  *)
    | Latin1 (Star '\x00' .. '\xff') -> sprintf "only latin1: %s" (lexeme buf)
    | _ -> failwith (sprintf "Unexpected character: %s" (lexeme buf))

let%expect_test _ =
  Sedlexing.Utf8.from_string "a+" |> next_tok |> print_string;
  [%expect {| with Chars: a+ |}];
  Sedlexing.Utf8.from_string "a÷" |> next_tok |> print_string;
  [%expect {| with Chars: a÷ |}];
  Sedlexing.Utf8.from_string "b+" |> next_tok |> print_string;
  [%expect {| with or_pattern: b+ |}];
  Sedlexing.Utf8.from_string "b÷" |> next_tok |> print_string;
  [%expect {| with or_pattern: b÷ |}];
  Sedlexing.Utf8.from_string "ÀÀÀ" |> next_tok |> print_string;
  [%expect {| mixed encoding: ÀÀÀ |}];
  Sedlexing.Utf8.from_string "az\x7f"
  |> next_tok |> String.escaped |> print_string;
  [%expect {| only ascii: az\127 |}];
  Sedlexing.Utf8.from_string "az\u{c0}" |> next_tok |> print_string;
  [%expect {| only latin1: azÀ |}]
