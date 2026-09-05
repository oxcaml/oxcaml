let ucase = [%sedlex.regexp? 'A' .. 'Z']
let lcase = [%sedlex.regexp? 'a' .. 'z']

let rec token buf =
  match%sedlex buf with
    | lcase ->
        print_char 'L';
        token buf
    | Compl (ucase | lcase) ->
        print_char '?';
        token buf
    | ucase ->
        print_char 'U';
        token buf
    | eof -> print_endline "."
    | _ -> assert false

let () =
  let lexbuf = Sedlexing.Latin1.from_string "Abc::DefG" in
  token lexbuf
