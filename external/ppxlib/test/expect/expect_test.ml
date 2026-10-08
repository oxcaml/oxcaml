open StdLabels

let compiler_version =
  match String.split_on_char ~sep:'.' Sys.ocaml_version with
  | major :: minor :: _ -> (int_of_string major, int_of_string minor)
  | _ -> assert false

let include_compiler_version range =
  let cmajor, cminor = compiler_version in
  match (range : Expect_lexer.version_range) with
  | Only (major, minor) -> cmajor = major && cminor = minor
  | From (major, minor) -> cmajor > major || (cmajor = major && cminor >= minor)
  | Up_to (major, minor) -> cmajor < major || (cmajor = major && cminor <= minor)
  | Between ((min_major, min_minor), (max_major, max_minor)) ->
      (cmajor > min_major && cmajor < max_major)
      || (cmajor = min_major && cminor >= min_minor)
      || (cmajor = max_major && cminor <= max_minor)

let read_file file =
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let file_contents = really_input_string ic len in
  close_in ic;
  file_contents

let run_expect_test file ~f =
  let file_contents = read_file file in
  let lexbuf = Lexing.from_string file_contents in
  lexbuf.lex_curr_p <-
    { pos_fname = file; pos_cnum = 0; pos_lnum = 1; pos_bol = 0 };

  let expected = f file_contents lexbuf in

  let corrected_file = file ^ ".corrected" in
  if file_contents <> expected then (
    let oc = open_out_bin corrected_file in
    output_string oc expected;
    close_out oc)
  else (
    if Sys.file_exists corrected_file then Sys.remove corrected_file;
    exit 0)

let capture_trimmed_fmt printer arg =
  let buf = Buffer.create 1024 in
  let buf_fmt = Format.formatter_of_buffer buf in
  printer buf_fmt arg;
  String.trim (Buffer.contents buf)

let print_loc _ _ ppf (loc : Location.t) =
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  Format.fprintf ppf "Line _";
  if startchar >= 0 then
    Format.fprintf ppf ", characters %d-%d" startchar endchar;
  Format.fprintf ppf ":@."

let report_printer () =
  let default = Location.default_report_printer () in
  let trimmed_pp report_printer ppf report =
    let trimmed = capture_trimmed_fmt (default.pp report_printer) report in
    Format.fprintf ppf "%s\n" trimmed
  in
  {
    default with
    pp = trimmed_pp;
    pp_main_loc = print_loc;
    pp_submsg_loc = print_loc;
  }

let setup_printers ppf =
  Location.formatter_for_warnings := ppf;
  Location.warning_reporter := Location.default_warning_reporter;
  Location.report_printer := report_printer;
  Location.alert_reporter := Location.default_alert_reporter

let apply_rewriters : Parsetree.toplevel_phrase -> Parsetree.toplevel_phrase =
  function
  | Ptop_dir _ as x -> x
  | Ptop_def s ->
      let s = Ppxlib.Selected_ast.of_ocaml Structure s in
      let s' = Ppxlib.Driver.map_structure s in
      Ptop_def (Ppxlib.Selected_ast.to_ocaml Structure s')

let execute_phrase ppf phr =
  let trimmed =
    capture_trimmed_fmt
      (fun ppf phr -> ignore (Toploop.execute_phrase true ppf phr))
      phr
  in
  match trimmed with "" -> () | _ -> Format.fprintf ppf "%s\n" trimmed

let pp_version ppf (major, minor) = Format.fprintf ppf "%d.%d" major minor

let pp_range ppf range =
  match (range : Expect_lexer.version_range) with
  | Only v -> pp_version ppf v
  | From v -> Format.fprintf ppf ">= %a" pp_version v
  | Up_to v -> Format.fprintf ppf "<= %a" pp_version v
  | Between (v1, v2) ->
      Format.fprintf ppf "%a <=> %a" pp_version v1 pp_version v2

let run_code ppf starting_pos code =
  let lexbuf = Lexing.from_string code in
  lexbuf.lex_curr_p <- { starting_pos with pos_lnum = 1 };
  let phrases = !Toploop.parse_use_file lexbuf in
  List.iter phrases ~f:(function
    | Parsetree.Ptop_def [] -> ()
    | phr -> (
        try
          let phr = apply_rewriters phr in
          if !Clflags.dump_source then
            Format.fprintf ppf "%a@?" Ppxlib.Pprintast.top_phrase
              (Ppxlib.Selected_ast.Of_ocaml.copy_toplevel_phrase phr);
          execute_phrase ppf phr
        with exn -> Location.report_exception ppf exn))

let trash_buffer = Buffer.create 1024
let trash_ppf = Format.formatter_of_buffer trash_buffer

let handle_ignore_block ppf starting_pos code =
  Format.fprintf ppf "%s[%%%%ignore]@." code;
  run_code trash_ppf starting_pos code;
  Buffer.clear trash_buffer

let handle_regular_expect_block ppf starting_pos code =
  Format.fprintf ppf "%s[%%%%expect{|@." code;
  run_code ppf starting_pos code;
  Format.fprintf ppf "@?|}]@."

let handle_versioned_expect_blocks ppf starting_pos code vexpect_blocks =
  let matched = ref false in
  let loc =
    {
      Ppxlib.Location.loc_start = starting_pos;
      loc_end = starting_pos;
      loc_ghost = false;
    }
  in
  Format.fprintf ppf "%s@?" code;
  List.iter vexpect_blocks ~f:(fun (range, content) ->
      Format.fprintf ppf "[%%%%expect_in %a {|@." pp_range range;
      if include_compiler_version range && not !matched then (
        matched := true;
        run_code ppf starting_pos code;
        Format.fprintf ppf "@?|}]@.")
      else if include_compiler_version range && !matched then
        Ppxlib.Location.raise_errorf ~loc
          "Multiple versioned expect block in a group matched our compiler \
           version %a"
          pp_version compiler_version
      else Format.fprintf ppf "%s|}]@." content);
  if not !matched then
    Ppxlib.Location.raise_errorf ~loc
      "No versioned expect block in a group matched our compiler version %a"
      pp_version compiler_version

let main () =
  let rec map_tree = function
    | Outcometree.Oval_constr (name, params) ->
        Outcometree.Oval_constr (name, List.map ~f:map_tree params)
    | Oval_variant (name, Some param) ->
        Oval_variant (name, Some (map_tree param))
    | Oval_string (s, maxlen, kind) ->
        Oval_string (s, (if maxlen < 8 then 8 else maxlen), kind)
    (*IF_NOT_AT_LEAST 504 | Oval_tuple tl -> Oval_tuple (List.map ~f:map_tree tl) *)
    (*IF_AT_LEAST 504 | Oval_tuple tl -> Oval_tuple (List.map ~f:(fun (label, v) -> (label, map_tree v)) tl) *)
    (*IF_NOT_AT_LEAST 504 | Oval_array tl -> Oval_array (List.map ~f:map_tree tl) *)
    (*IF_AT_LEAST 504 | Oval_array (tl, mutable_) -> Oval_array ((List.map ~f:map_tree tl), mutable_) *)
    | Oval_list tl -> Oval_list (List.map ~f:map_tree tl)
    | Oval_record fel ->
        Oval_record
          (List.map ~f:(fun (name, tree) -> (name, map_tree tree)) fel)
    | tree -> tree
  in
  let print_out_value = !Toploop.print_out_value in
  (* Achieve 4.14 printing behaviour, as introduced in
     https://github.com/ocaml/ocaml/pull/10565 *)
  (Toploop.print_out_value :=
     fun ppf tree -> print_out_value ppf (map_tree tree));
  run_expect_test Sys.argv.(1) ~f:(fun file_contents lexbuf ->
      let chunks = Expect_lexer.split_file ~file_contents lexbuf in

      let buf = Buffer.create (String.length file_contents + 1024) in
      let ppf = Format.formatter_of_buffer buf in
      setup_printers ppf;
      Topfind.log := ignore;

      let _ = Warnings.parse_options false "@a-4-29-40-41-42-44-45-48-58" in
      Clflags.real_paths := false;
      Toploop.initialize_toplevel_env ();

      (* Findlib stuff *)
      let preds = [ "toploop" ] in
      let preds =
        match Sys.backend_type with
        | Native -> "native" :: preds
        | Bytecode -> "byte" :: preds
        | Other _ -> preds
      in
      Topfind.add_predicates preds;
      (* This just adds the include directories since the [ppx] library
         is statically linked in *)
      Topfind.load_deeply [ "ppxlib" ];

      List.iter chunks
        ~f:(fun { Expect_lexer.phrases; phrases_start; expect } ->
          match expect with
          | Ignore -> handle_ignore_block ppf phrases_start phrases
          | Regular -> handle_regular_expect_block ppf phrases_start phrases
          | Versioned vexpects ->
              handle_versioned_expect_blocks ppf phrases_start phrases vexpects);
      Buffer.contents buf)

let () =
  try main ()
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
