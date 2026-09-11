let rec token buf =
  match%sedlex buf with
    | white_space ->
        print_endline "\tWhitespace";
        token buf
    | Sub (Chars "ab", "b") ->
        print_endline "a";
        token buf
    | Chars "ab" | "c" ->
        print_endline "abc";
        token buf
    | Intersect ("d", Chars "abd") ->
        print_endline "d";
        token buf
    | eof -> print_endline "\tEnd"
    | any ->
        print_endline "Other";
        token buf
    | _ -> failwith "Internal failure: Reached impossible place"

let () =
  let lexbuf = Sedlexing.Utf8.from_string "a b c d e" in
  token lexbuf
