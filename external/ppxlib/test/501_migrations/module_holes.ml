let parse source =
  Ocaml_common.Parse.implementation (Lexing.from_string source)

let round_trip ast =
  let migrated = Astlib.Migrate_999_500.copy_structure ast in
  let restored = Astlib.Migrate_500_999.copy_structure migrated in
  if ast <> restored then failwith "module-expression migration changed the AST"

let () =
  round_trip (parse "module M = Existing");
  match parse "module M = _" with
  | exception Ocaml_common.Syntaxerr.Error _ ->
      print_endline "compiler does not support module holes"
  | ast ->
      round_trip ast;
      round_trip (parse "module M = (_ [@test])");
      round_trip (parse "module M = F(_)");
      print_endline "module holes round-trip successfully"
