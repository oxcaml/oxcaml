let write fn s =
  let oc = open_out fn in
  output_string oc s;
  close_out oc

let () =
  let ocaml_version_str = Sys.argv.(1) in
  let ocaml_version =
    Scanf.sscanf ocaml_version_str "%u.%u" (fun a b -> (a, b))
  in
  write "ast-version"
    (match ocaml_version with
    | 4, 08 -> "408"
    | 4, 09 -> "409"
    | 4, 10 -> "410"
    | 4, 11 -> "411"
    | 4, 12 -> "412"
    | 4, 13 -> "413"
    | 4, 14 -> "414"
    | 5, 0 ->
        "414"
        (* Ast_500 aliases Ast_414, since the AST hasn't changed between those two *)
    | 5, 1 -> "501"
    | 5, 2 -> "502"
    | 5, 3 -> "503"
    | 5, 4 -> "504"
    | 5, 5 -> "505"
    | _ ->
        Printf.eprintf "Unknown OCaml version %s\n" ocaml_version_str;
        exit 1)
