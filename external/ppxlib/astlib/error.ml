let raise_error_txt ~loc = Location.raise_errorf ~loc "%a" Format.pp_print_text

let migration_error ~loc ~from ~to_ msg =
  let s =
    Format.sprintf
      "ppxlib migration error (migrating ocaml.%s to ocaml.%s): %s\n\n\
       Ppxlib was likely trying to migrate from %s to 5.2.0 and back again, \
       but encountered an AST node that could not be encoded. Either remove \
       the feature or open an issue at \
       https://github.com/ocaml-ppx/ppxlib/issues."
      from to_ msg Sys.ocaml_version
  in
  raise_error_txt ~loc s

let invalid_encoding ~loc ~version msg =
  let s =
    Format.sprintf
      "ppxlib invalid encoding: %s\n\n\
       Ppxlib failed to decode a feature from the OCaml %s AST. This is likely \
       caused by the use of a %s+ language feature within code directly \
       interpreted by a ppx. If you'd like this feature to be supported by the \
       ppx involved, please reach out to the ppx maintainers. If this does not \
       seem right, please do open an issue at \
       https://github.com/ocaml-ppx/ppxlib/issues."
      msg version version
  in
  raise_error_txt ~loc s
