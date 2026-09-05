let rec token buf =
  match%sedlex buf with
    | white_space ->
        print_endline "\tWhitespace";
        token buf
    | 'a', Rep (white_space, 1) ->
        print_endline "a\n\tWhitespace";
        token buf
    | Rep ("bc", 2) ->
        print_endline "bcbc";
        token buf
    | Rep ("d", 1 .. 1) ->
        print_endline "d";
        token buf
    | Rep ("ef", 1 .. 3) ->
        Printf.printf "%s\n" (Sedlexing.Utf8.lexeme buf);
        token buf
    | eof -> print_endline "\tEnd"
    | any ->
        print_endline "Other";
        token buf
    | _ -> failwith "Internal failure: Reached impossible place"

let () =
  let lexbuf = Sedlexing.Utf8.from_string "a bcbc d ef efef efefef" in
  token lexbuf
