(* TEST
 include ocamlcommon;
 bytecode;
*)

open Parsetree

let parse parser source = parser (Lexing.from_string source)

let check_directive source name =
  let phrase = parse Parse.toplevel_phrase source in
  let printed = Format.asprintf "%a" Pprintast.top_phrase phrase in
  match parse Parse.toplevel_phrase printed with
  | Ptop_dir { pdir_name; _ } -> assert (pdir_name.txt = name)
  | Ptop_def _ -> assert false

let () =
  check_directive "#use \"file.ml\";;" "use";
  check_directive "# Custom;;" "Custom";
  (match parse Parse.expression "obj#method_name" with
   | { pexp_desc = Pexp_send (_, method_name); _ } ->
       assert (method_name.txt = "method_name")
   | _ -> assert false);
  let round_trip source =
    parse Parse.expression (Pprintast.string_of_expression
      (parse Parse.expression source))
  in
  (match round_trip "M.#N.field" with
   | { pexp_desc = Pexp_unboxed_field (_, field); _ } ->
       assert (Longident.flatten field.txt = ["N"; "field"])
   | _ -> assert false);
  (match round_trip "(M.#N).field" with
   | { pexp_desc = Pexp_field ({ pexp_desc = Pexp_construct (name, None); _ },
                              _); _ } ->
       assert (Longident.flatten name.txt = ["M"; "#N"])
   | _ -> assert false);
  ignore (round_trip "M.#N.(0)");
  List.iter (fun source ->
    let pat = parse Parse.pattern source in
    match pat.ppat_desc with
    | Ppat_type path ->
        let ty = parse Parse.core_type source in
        (match ty.ptyp_desc with
         | Ptyp_class (class_path, []) ->
             assert (Longident.same path.txt class_path.txt)
         | _ -> assert false)
    | _ -> assert false)
    ["#M.t"; "#F(X).t"; "#F (* comment *) (G(X.Y)) (Z).M.t"];
  List.iter (fun source ->
    let pat = parse Parse.pattern source in
    let expr = parse Parse.expression source in
    match pat.ppat_desc, expr.pexp_desc with
    | Ppat_construct (pname, _), Pexp_construct (ename, _) ->
        assert (Longident.last pname.txt = "#Count");
        assert (Longident.same pname.txt ename.txt);
        let check printed =
          let reparsed = parse Parse.pattern printed in
          match reparsed.ppat_desc with
          | Ppat_construct (name, _) ->
              assert (Longident.same name.txt pname.txt)
          | _ -> assert false
        in
        check (Format.asprintf "%a" Pprintast.pattern pat);
        check (Pprintast.string_of_expression expr)
    | _ -> assert false)
    ["#Count"; "#Count (X)"; "M.N.#Count x"];
  let name = parse Parse.constr_ident "M.N.#Count" in
  assert (Longident.flatten name = ["M"; "N"; "#Count"]);
  let printed = Format.asprintf "%a" Pprintast.longident name in
  assert (Longident.same name (parse Parse.longident printed));
  List.iter (fun source ->
    match parse Parse.implementation source with
    | _ -> assert false
    | exception Syntaxerr.Error _ -> ())
    ["module #Count = struct end"; "type t = #Count"];
  let spaces = String.make 2048 ' ' in
  let source = "#F(" ^ spaces ^ "X).t" in
  let index = ref 0 in
  let input = Lexing.from_function (fun bytes length ->
    let length = min length (min 17 (String.length source - !index)) in
    Bytes.blit_string source !index bytes 0 length;
    index := !index + length;
    length)
  in
  Lexer.init ();
  List.iter (fun spelling ->
    ignore (Lexer.token input);
    assert (Lexing.lexeme input = spelling))
    ["#"; "F"; "("; "X"; ")"; "."; "t"; ""]
