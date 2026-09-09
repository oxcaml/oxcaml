rule rewrite is_current ocaml_version has_module_holes = parse
  |          "(*IF_CURRENT " ([^'*']* as s) "*)"
    { let chunk = if is_current
        then "             " ^ s ^          "  "
        else Lexing.lexeme lexbuf
      in
      print_string chunk;
      rewrite is_current ocaml_version has_module_holes lexbuf
    }
  |          "(*IF_MODULE_HOLES " ([^'*']* as s) "*)"
    { let chunk = if has_module_holes
        then String.make (String.length "(*IF_MODULE_HOLES ") ' ' ^ s ^ "  "
        else Lexing.lexeme lexbuf
      in
      print_string chunk;
      rewrite is_current ocaml_version has_module_holes lexbuf
    }
  |          "(*IF_AT_LEAST " ([^'*' ' ']* as v) " " ([^'*']* as s) "*)"
    { let chunk = if (v <= ocaml_version)
        then "              " ^ String.make (String.length v + 1) ' ' ^ s ^          "  "
        else Lexing.lexeme lexbuf
      in
      print_string chunk;
      rewrite is_current ocaml_version has_module_holes lexbuf
    }
  |          "(*IF_NOT_AT_LEAST " ([^'*' ' ']* as v) " " ([^'*']* as s) "*)"
    { let chunk = if not (v <= ocaml_version)
        then "                  " ^ String.make (String.length v + 1) ' ' ^ s ^          "  "
        else Lexing.lexeme lexbuf
      in
      print_string chunk;
      rewrite is_current ocaml_version has_module_holes lexbuf
    }
  | _ as c
    { print_char c;
      rewrite is_current ocaml_version has_module_holes lexbuf
    }
  | eof { () }


