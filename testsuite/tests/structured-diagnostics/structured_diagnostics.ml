(* TEST
 readonly_files = "mode_error.ml no_diagnostic.ml type_error.ml \
                   unicode_error.ml warning_only.ml warnings_then_error.ml";
 set stdlib = "-nostdlib -I ${ocamlsrcdir}/stdlib";
 arguments = "${ocamlrun} ${ocamlc_byte} ${ocamlopt_byte} ${stdlib}";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

exception Malformed of string

type json =
  | Null
  | Bool of bool
  | Number of string
  | String of string
  | Array of json list
  | Object of (string * json) list

let parse text =
  let length = String.length text in
  let position = ref 0 in
  let fail message = raise (Malformed message) in
  let peek () = if !position < length then Some text.[!position] else None in
  let advance () = incr position in
  let skip_whitespace () =
    let rec loop () =
      match peek () with
      | Some (' ' | '\t' | '\r') ->
        advance ();
        loop ()
      | Some _ | None -> ()
    in
    loop ()
  in
  let expect character =
    match peek () with
    | Some found when found = character -> advance ()
    | Some found -> fail (Printf.sprintf "expected %c, found %c" character found)
    | None -> fail (Printf.sprintf "expected %c, found end of input" character)
  in
  let hexadecimal character =
    match character with
    | '0' .. '9' -> Char.code character - Char.code '0'
    | 'a' .. 'f' -> Char.code character - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code character - Char.code 'A' + 10
    | _ -> fail "expected a hexadecimal digit"
  in
  let code_unit () =
    let code = ref 0 in
    for _ = 1 to 4 do
      match peek () with
      | None -> fail "unterminated escape"
      | Some digit ->
        code := (!code * 16) + hexadecimal digit;
        advance ()
    done;
    !code
  in
  let leads_surrogate_pair code = code >= 0xd800 && code <= 0xdbff in
  let trails_surrogate_pair code = code >= 0xdc00 && code <= 0xdfff in
  let scalar_value () =
    let code = code_unit () in
    if not (leads_surrogate_pair code)
    then code
    else begin
      (match (peek (), text.[!position + 1]) with
      | Some '\\', 'u' ->
        position := !position + 2
      | _ -> fail "lone leading surrogate");
      let trail = code_unit () in
      if not (trails_surrogate_pair trail) then fail "expected a trailing surrogate";
      0x10000 + ((code - 0xd800) * 0x400) + (trail - 0xdc00)
    end
  in
  let parse_string () =
    expect '"';
    let buffer = Buffer.create 32 in
    let rec loop () =
      match peek () with
      | None -> fail "unterminated string"
      | Some '\n' -> fail "unescaped newline in string"
      | Some '"' ->
        advance ();
        Buffer.contents buffer
      | Some '\\' ->
        advance ();
        (match peek () with
        | None -> fail "unterminated escape"
        | Some 'u' ->
          advance ();
          Buffer.add_utf_8_uchar buffer (Uchar.of_int (scalar_value ()))
        | Some escaped ->
          advance ();
          Buffer.add_char buffer
            (match escaped with
            | 'n' -> '\n'
            | 't' -> '\t'
            | 'r' -> '\r'
            | 'b' -> '\b'
            | 'f' -> '\012'
            | '"' -> '"'
            | '\\' -> '\\'
            | '/' -> '/'
            | _ -> fail "unknown escape"));
        loop ()
      | Some character ->
        advance ();
        Buffer.add_char buffer character;
        loop ()
    in
    loop ()
  in
  let parse_literal spelling value =
    let stop = !position + String.length spelling in
    if stop > length || String.sub text !position (String.length spelling) <> spelling
    then fail (Printf.sprintf "expected %s" spelling);
    position := stop;
    value
  in
  let parse_number () =
    let start = !position in
    let rec loop () =
      match peek () with
      | Some ('-' | '+' | '.' | 'e' | 'E' | '0' .. '9') ->
        advance ();
        loop ()
      | Some _ | None -> ()
    in
    loop ();
    if !position = start then fail "expected a number";
    Number (String.sub text start (!position - start))
  in
  let rec parse_value () =
    skip_whitespace ();
    match peek () with
    | None -> fail "expected a value"
    | Some '"' -> String (parse_string ())
    | Some '{' -> parse_object ()
    | Some '[' -> parse_array ()
    | Some 't' -> parse_literal "true" (Bool true)
    | Some 'f' -> parse_literal "false" (Bool false)
    | Some 'n' -> parse_literal "null" Null
    | Some _ -> parse_number ()
  and parse_object () =
    expect '{';
    skip_whitespace ();
    match peek () with
    | Some '}' ->
      advance ();
      Object []
    | Some _ | None ->
      let rec loop fields =
        skip_whitespace ();
        let name = parse_string () in
        skip_whitespace ();
        expect ':';
        let value = parse_value () in
        let fields = (name, value) :: fields in
        skip_whitespace ();
        match peek () with
        | Some ',' ->
          advance ();
          loop fields
        | Some '}' ->
          advance ();
          Object (List.rev fields)
        | Some _ | None -> fail "expected , or } in object"
      in
      loop []
  and parse_array () =
    expect '[';
    skip_whitespace ();
    match peek () with
    | Some ']' ->
      advance ();
      Array []
    | Some _ | None ->
      let rec loop items =
        let items = parse_value () :: items in
        skip_whitespace ();
        match peek () with
        | Some ',' ->
          advance ();
          loop items
        | Some ']' ->
          advance ();
          Array (List.rev items)
        | Some _ | None -> fail "expected , or ] in array"
      in
      loop []
  in
  let value = parse_value () in
  skip_whitespace ();
  if !position <> length then fail "unexpected trailing input";
  value

let field name json =
  match json with
  | Object fields -> List.assoc_opt name fields
  | Null | Bool _ | Number _ | String _ | Array _ -> None

let string_field name json =
  match field name json with
  | Some (String value) -> Some value
  | _ -> None

let list_field name json =
  match field name json with
  | Some (Array items) -> Some items
  | _ -> None

let rec text_of json =
  match json with
  | Object _ -> (
    match (string_field "kind" json, string_field "text" json) with
    | Some "text", Some text -> text
    | _ -> (
      match list_field "content" json with
      | Some content -> String.concat "" (List.map text_of content)
      | None -> ""))
  | Array items -> String.concat "" (List.map text_of items)
  | Null | Bool _ | Number _ | String _ -> ""

let flag = "-structured-diagnostics"

type tool =
  | Bytecode
  | Native

let ocamlrun, bytecode_compiler, native_compiler, shared_flags =
  match Array.to_list Sys.argv with
  | _ :: ocamlrun :: bytecode :: native :: flags ->
    (ocamlrun, bytecode, native, String.concat " " flags)
  | _ -> failwith "expected ocamlrun, ocamlc, ocamlopt and the stdlib flags"

let compiler = function
  | Bytecode -> bytecode_compiler
  | Native -> native_compiler

let read path = In_channel.with_open_bin path In_channel.input_all

type outcome =
  { status : int;
    out : string;
    err : string
  }

let compile ?(environment = "") ?(tool = Bytecode) ?(options = "-c") ~structured
    file =
  let status =
    Sys.command
      (Printf.sprintf "%s %s %s %s %s %s > stdout.txt 2> stderr.txt" environment
         ocamlrun (compiler tool) shared_flags
         (if structured then flag else "")
         (options ^ " " ^ file))
  in
  { status; out = read "stdout.txt"; err = read "stderr.txt" }

let nonempty_lines text =
  List.filter (fun line -> line <> "") (String.split_on_char '\n' text)

let parsed text =
  List.map
    (fun line ->
      match parse line with
      | json -> Some json
      | exception Malformed _ -> None)
    (nonempty_lines text)

let diagnostics text = List.filter_map (fun parsed -> parsed) (parsed text)

let all_lines_are_json text =
  let parsed = parsed text in
  parsed <> [] && List.for_all Option.is_some parsed

let no_line_is_json text = List.for_all Option.is_none (parsed text)

let contains text sub =
  let length = String.length sub in
  let rec loop start =
    start + length <= String.length text
    && (String.sub text start length = sub || loop (start + 1))
  in
  loop 0

let check name condition =
  Printf.printf "%s: %s\n" name (if condition then "ok" else "FAILED")

let located_in file diagnostic =
  match field "loc" diagnostic with
  | Some loc -> string_field "file" loc = Some file
  | None -> false

let body_text diagnostic =
  match list_field "body" diagnostic with
  | Some (block :: _) -> text_of block
  | Some [] | None -> ""

let () =
  let file = "type_error.ml" in
  let off = compile ~structured:false file in
  let on = compile ~structured:true file in
  check "generic: flag off reports prose" (no_line_is_json off.err);
  check "generic: flag off reports something" (String.trim off.err <> "");
  check "generic: flag on reports one json object"
    (List.length (nonempty_lines on.err) = 1 && all_lines_are_json on.err);
  check "generic: exit status is unchanged" (off.status = on.status);
  match diagnostics on.err with
  | [ diagnostic ] ->
    let text = body_text diagnostic in
    check "generic: title carries the severity"
      (string_field "title" diagnostic = Some "Error");
    check "generic: diagnostic is located in the source file"
      (located_in file diagnostic);
    check "generic: no entities" (list_field "entities" diagnostic = Some []);
    check "generic: no glossary" (list_field "glossary" diagnostic = Some []);
    check "generic: first block carries the normal rendered text"
      (text = String.trim off.err);
    check "generic: rendered text spans several lines"
      (String.contains text '\n');
    check "generic: rendered text quotes the offending source"
      (String.contains text '"')
  | _ -> check "generic: exactly one diagnostic" false

let () =
  let file = "mode_error.ml" in
  let off = compile ~structured:false file in
  let on = compile ~structured:true file in
  check "mode: flag off reports prose" (no_line_is_json off.err);
  check "mode: flag on reports one json object"
    (List.length (nonempty_lines on.err) = 1 && all_lines_are_json on.err);
  check "mode: exit status is unchanged" (off.status = on.status);
  match diagnostics on.err with
  | [ diagnostic ] ->
    check "mode: diagnostic is specialized"
      (match string_field "title" diagnostic with
      | Some title -> title <> "Error" && title <> ""
      | None -> false);
    check "mode: diagnostic is located in the source file"
      (located_in file diagnostic);
    check "mode: entities are interned"
      (match list_field "entities" diagnostic with
      | Some (_ :: _) -> true
      | Some [] | None -> false);
    check "mode: glossary is populated"
      (match list_field "glossary" diagnostic with
      | Some (_ :: _) -> true
      | Some [] | None -> false);
    check "mode: body is not the rendered message"
      (body_text diagnostic <> String.trim off.err)
  | _ -> check "mode: exactly one diagnostic" false

let () =
  let file = "warnings_then_error.ml" in
  let off = compile ~structured:false file in
  let on = compile ~structured:true file in
  let diagnostics = diagnostics on.err in
  check "several: flag off reports prose" (no_line_is_json off.err);
  check "several: every line is json" (all_lines_are_json on.err);
  check "several: one line per diagnostic" (List.length diagnostics = 3);
  check "several: exit status is unchanged" (off.status = on.status);
  check "several: every diagnostic is located in the source file"
    (List.for_all (located_in file) diagnostics);
  check "several: severities are two warnings then an error"
    (List.map (string_field "title") diagnostics
    = [ Some "Warning 8 [partial-match]";
        Some "Warning 8 [partial-match]";
        Some "Error"
      ])

let () =
  let file = "warning_only.ml" in
  let off = compile ~options:"-i" ~structured:false file in
  let on = compile ~options:"-i" ~structured:true file in
  check "stdout: unaffected by the flag" (off.out = on.out);
  check "stdout: carries the inferred signature" (String.trim on.out <> "");
  check "stdout: carries no json" (no_line_is_json on.out);
  check "stdout: warning still reported on stderr"
    (all_lines_are_json on.err && List.length (diagnostics on.err) = 1)

let () =
  let file = "no_diagnostic.ml" in
  let off = compile ~structured:false file in
  let on = compile ~structured:true file in
  check "silent: nothing is reported either way"
    (String.trim off.err = "" && String.trim on.err = "");
  check "silent: compilation succeeds either way"
    (off.status = 0 && on.status = 0)

let () =
  let file = "unicode_error.ml" in
  let off = compile ~structured:false file in
  let on = compile ~structured:true file in
  check "unicode: flag on reports one json object"
    (List.length (nonempty_lines on.err) = 1 && all_lines_are_json on.err);
  match diagnostics on.err with
  | [ diagnostic ] ->
    let text = body_text diagnostic in
    check "unicode: decoded text is the normal rendered text"
      (text = String.trim off.err);
    check "unicode: decoded text keeps the source characters"
      (contains text "λ" && contains text "→" && contains text "μ")
  | _ -> check "unicode: exactly one diagnostic" false

let driver_path name ?environment ?options ~titles file =
  List.iter
    (fun (tool, tool_name) ->
      let name = Printf.sprintf "%s (%s)" name tool_name in
      let off = compile ?environment ~tool ?options ~structured:false file in
      let on = compile ?environment ~tool ?options ~structured:true file in
      check (name ^ ": flag off reports prose") (no_line_is_json off.err);
      check (name ^ ": exit status is unchanged") (off.status = on.status);
      check (name ^ ": flag on reports json only") (all_lines_are_json on.err);
      check (name ^ ": one line per diagnostic")
        (List.length (nonempty_lines on.err) = List.length titles);
      check (name ^ ": severities are preserved")
        (List.map (string_field "title") (diagnostics on.err)
        = List.map Option.some titles))
    [ (Bytecode, "ocamlc"); (Native, "ocamlopt") ]

let () =
  driver_path "environment" ~environment:"OCAML_COLOR=bogus"
    ~titles:[ "Warning 46 [bad-env-variable]" ]
    "no_diagnostic.ml"

let () =
  driver_path "unknown flag" ~options:"-not-a-flag -c" ~titles:[ "Error" ]
    "no_diagnostic.ml"

let () =
  driver_path "unknown suffix" ~titles:[ "Error" ] "no_diagnostic.unknown"

let () =
  let off = compile ~options:"-not-a-flag -c" ~structured:false
      "no_diagnostic.ml"
  in
  let on = compile ~options:"-not-a-flag -c" ~structured:true
      "no_diagnostic.ml"
  in
  check "unknown flag: usage is printed as prose" (contains off.err "Usage:");
  check "unknown flag: usage is dropped rather than emitted"
    (not (contains on.err "Usage:"))
