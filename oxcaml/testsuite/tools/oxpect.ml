let with_ppf_to_string =
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  fun f ->
    match f ppf with
    | () ->
       Format.pp_print_flush ppf ();
       let s = Buffer.contents buf in
       Buffer.clear buf;
       s
    | exception e ->
       Buffer.clear buf;
       raise e

module Expect_kind = struct
  type t =
    | Error
    | Warn
    | Type
    | Lambda
    | Rawlambda
    | Output

  let of_string = function
    | "expect.error" -> Ok Error
    | "expect.warn" -> Ok Warn
    | "expect.type" -> Ok Type
    | "expect.lambda" -> Ok Lambda
    | "expect.rawlambda" -> Ok Rawlambda
    | "expect.output" -> Ok Output
    | _ -> Result.Error ()

  let to_string = function
    | Error -> "expect.error"
    | Warn -> "expect.warn"
    | Type -> "expect.type"
    | Lambda -> "expect.lambda"
    | Rawlambda -> "expect.rawlambda"
    | Output -> "expect.output"
end

module StrTbl = Hashtbl.Make (String)

module File = struct
  type expectation = {
    kind: Expect_kind.t;
    loc: Location.t option;
    mutable body_rev: string list;
  }

  type phrase = {
    loc: Location.t;
    tag: string;
    mutable expectations: expectation list
  }

  type t = {
    phrase_list: phrase list;
    phrase_table: phrase StrTbl.t;
    misc: phrase;
    kinds_present: Expect_kind.t list;
  }

  let of_phrases phrases misc_exps =
    let mk_expectations exps =
      exps |> List.map (fun (kind, loc) : expectation ->
        { kind; loc = Some loc; body_rev = [] })
    in
    let phrase_list =
      phrases |> List.map (fun (loc, tag, exps) : phrase ->
        {loc; tag; expectations = mk_expectations exps})
    in
    let phrase_table = StrTbl.create 10 in
    phrase_list |> List.iter (fun (p : phrase) ->
      StrTbl.add phrase_table p.tag p);
    let misc =
      { loc = Location.none; tag = ""; expectations = mk_expectations misc_exps }
    in
    let kinds_present =
      (misc :: phrase_list)
      |> List.concat_map (fun (p : phrase) ->
        p.expectations |> List.map (fun (e : expectation) -> e.kind))
      |> List.sort_uniq compare
    in
    { phrase_list; phrase_table; misc; kinds_present }

  let empty () = of_phrases [] []

  let has_kind (t : t) k = List.mem k t.kinds_present

  let add_text' (t : t) ~create ~tag ~kind txt_fn =
    let phrase =
      Option.value (StrTbl.find_opt t.phrase_table tag) ~default:t.misc
    in
    match List.find_opt (fun e -> e.kind = kind) phrase.expectations with
    | Some e -> e.body_rev <- txt_fn () :: e.body_rev
    | None ->
       if create then
         phrase.expectations <-
           { kind; loc = None; body_rev = [txt_fn ()] }
           :: phrase.expectations

  let add_text t ~create ~tag ~kind txt =
    add_text' t ~create ~tag ~kind (fun () -> txt)

  let add_ppf t ~create ~tag ~kind f =
    add_text' t ~create ~tag ~kind (fun () -> with_ppf_to_string f)

  let copy_all (dst : t) (src : t) =
    let copy_phrase (p : phrase) =
      p.expectations |> List.iter (fun (e : expectation) ->
        e.body_rev |> List.rev |> List.iter (fun txt ->
          add_text dst ~create:true ~tag:p.tag ~kind:e.kind txt))
    in
    List.iter copy_phrase src.phrase_list;
    copy_phrase src.misc

  let reassemble t original oc =
    let output_exps pos exps =
      let pos = ref pos in
      let txt =
        exps
        |> List.map
          (fun (e : expectation) ->
            e.loc |> Option.iter (fun (l : Location.t) -> pos := l.loc_end.pos_cnum);
            match e.body_rev with
            | [] -> ""
            | body_rev ->
               Printf.sprintf "\n[%%%%%s {|\n%s|}]"
                 (Expect_kind.to_string e.kind)
                 (String.concat "" (List.rev body_rev)))
        |> String.concat ""
      in
      !pos, txt
    in
    let output_orig pos pos' =
      output_string oc (String.sub original pos (pos' - pos))
    in
    let pos, misc = output_exps 0 t.misc.expectations in
    if misc <> "" then begin
      (* Unsatisfying idempotence hack! *)
      let misc = misc ^ "\n# 1 \"_\"\n" in
      (* To ensure idempotence, check that these aren't already present *)
      if not (String.starts_with ~prefix:misc original) then
        output_string oc misc;
    end;
    let rec go pos = function
      | [] ->
         output_orig pos (String.length original)
      | p :: ps ->
         let pos' = p.loc.loc_end.pos_cnum in
         output_orig pos pos';
         let pos', txt = output_exps pos' p.expectations in
         output_string oc txt;
         go pos' ps
    in
    go pos t.phrase_list
end

let oxpect_magic = "OXPECT 98eb574af4572afa"

let instrument (ss : Parsetree.structure) =
  let phrases = ref [] in
  let open Parsetree in
  let remap_locations ~tag si =
    let open Location in
    let open Parsetree in
    let line = si.pstr_loc.loc_start.pos_lnum in
    let pos (p : Lexing.position) =
      { p with pos_fname = tag; pos_lnum = p.pos_lnum - line }
    in
    let location _ l =
      { l with loc_start = pos l.loc_start; loc_end = pos l.loc_end }
    in
    let mapper = { Ast_mapper.default_mapper with location } in
    mapper.structure_item mapper si
  in
  let rec collect_expectations ~tag acc = function
    | { pstr_desc = Pstr_extension(({txt=ext_name; _}, _payload), _);
        pstr_loc = ext_loc } :: rest
         when String.starts_with ~prefix:"expect" ext_name ->
       let kind =
         match Expect_kind.of_string ext_name with
         | Ok t -> t
         | Error () ->
            (* FIXME: not this, it's not a fixedpoint *)
            let mark (p : Lexing.position) = { p with pos_fname = tag } in
            let loc = { ext_loc with loc_start = mark ext_loc.loc_start;
                                     loc_end = mark ext_loc.loc_end } in
            raise (Syntaxerr.(Error (Not_expecting (loc, ext_name))))
       in
       let acc = (kind, ext_loc) :: acc in
       collect_expectations ~tag acc rest
    | ss -> ss, List.rev acc
  in
  let rec go i ss =
    match ss with
    | [] -> []
    | si :: rest ->
       let tag = Printf.sprintf "<phrase_%d>" i in
       let rest, expectations = collect_expectations ~tag [] rest in
       phrases := (si.pstr_loc, tag, expectations) :: !phrases;
       let open Ast_helper in
       let log =
         let msg = oxpect_magic ^ "\n" ^ tag ^ "\n" in
         let lid x = Location.mknoloc (Longident.Lident x) in
         let app f x = Exp.apply (Exp.ident (lid f)) [ Nolabel, x ] in
         let cmd =
           Exp.sequence
             (app "print_string" (Exp.constant (Const.string msg)))
             (Exp.sequence
                (app "flush" (Exp.ident (lid "stdout")))
                (app "flush" (Exp.ident (lid "stderr"))))
         in
         Str.eval (Exp.open_ (Opn.mk (Mod.ident (lid "Stdlib"))) cmd)
       in
       let si = remap_locations ~tag si in
       let loc = si.pstr_loc in
       let si = Str.include_ ~loc (Incl.mk ~loc (Mod.structure ~loc [si])) in
       log :: si :: go (i+1) rest
  in
  let ss, misc_exps = collect_expectations ~tag:"" [] ss in
  let ss = go 0 ss in
  File.of_phrases (List.rev !phrases) misc_exps, ss

let run filename =
  Clflags.native_code := true;
  Clflags.continue_on_type_error := true;
  Clflags.zap_mliless_modality_to_id := true;
  Clflags.locations := false;
  Clflags.unique_ids := false;
  let file = ref (File.empty ()) in
  Compiler_hooks.register Parse_tree_impl (fun ss ->
    let new_file, ss = instrument ss in
    (* Keep any errors that arose before parsing *)
    File.copy_all new_file !file;
    file := new_file;
    ss);

  let reporter : Location.report_printer =
    let strip_loc (loc : Location.t) =
      let strip_name (p : Lexing.position) = { p with pos_fname = "_" } in
      { loc with loc_start = strip_name loc.loc_start;
                 loc_end = strip_name loc.loc_end }
    in
    let super = Location.batch_mode_printer in
    { super with
      pp = (fun self _ppf report ->
        let kind : Expect_kind.t = match report.kind with
          | Report_warning_as_error _
          | Report_alert_as_error _
          | Report_error -> Error
          | Report_warning _
          | Report_alert _ -> Warn
        in
        File.add_ppf !file
          ~create:true
          ~tag:report.main.loc.loc_start.pos_fname
          ~kind
          (fun ppf -> super.pp self ppf report));
      pp_main_loc = (fun self report ppf loc ->
        super.pp_main_loc self report ppf (strip_loc loc));
      pp_submsg_loc = (fun self report ppf loc ->
        super.pp_submsg_loc self report ppf (strip_loc loc)) }
  in
  Location.report_printer := (fun () -> Location.reset (); reporter);
  Compiler_hooks.register Typed_tree_impl (fun (tt : Typedtree.implementation) ->
    if File.has_kind !file Type then begin
      tt.structure.str_items |> List.iter (fun (si : Typedtree.structure_item) ->
        let loc = si.str_loc in
        match si.str_desc with
        | Tstr_include incl ->
           File.add_ppf !file ~create:false ~tag:loc.loc_start.pos_fname ~kind:Type
             (fun ppf ->
               Printtyp.wrap_printing_env ~error:false incl.incl_mod.mod_env (fun () ->
                 Format.fprintf ppf "%a@." (Printtyp.printed_signature "_") incl.incl_type))
        | _ -> ())
      end);
  (* Disassemble module-block Lambda terms *)
  let rec walk_lambda =
    let open Lambda in
    function
    | Llet (_, _, id, uid, e, l)
         when Ident.name id = "include" && uid = Lambda.debug_uid_none ->
       walk_lambda e @ walk_lambda l
    | Llet (_, _, id, _, e, l) -> ([id],None,e) :: walk_lambda l
    | Lletrec (fns, l) ->
       List.map (fun (f : rec_binding) -> ([f.id], None, Lfunction f.def)) fns
       @ walk_lambda l
    | Lstaticcatch (e, (lbl, ids), l, _, _) ->
       let ids = List.map (fun (id,_,_) -> id) ids in
       (ids, Some lbl, e) :: walk_lambda l
    | Lsequence (e, l) ->
       ([], None, e) :: walk_lambda l
    | _ -> []
  in
  let print_lambda ~kind (lprog : Lambda.program) =
    walk_lambda lprog.code |> List.iter (fun (ids, lbl, e) ->
      let loc =
        Lambda.try_to_find_location e
        |> Debuginfo.Scoped_location.to_location
      in
      File.add_ppf !file ~create:false ~tag:loc.loc_start.pos_fname ~kind
        (fun ppf ->
          let names =
            ((Option.to_list lbl |> List.map Static_label.to_string) @
             List.map Ident.name ids)
            |> String.concat " "
          in
          Format.fprintf ppf "@[<2>%s =@ %a@]@." names Printlambda.lambda e))
  in
  Compiler_hooks.register Raw_lambda (fun lprog ->
    if File.has_kind !file Rawlambda then print_lambda ~kind:Rawlambda lprog);
  Compiler_hooks.register Lambda (fun lprog ->
    if File.has_kind !file Lambda then print_lambda ~kind:Lambda lprog);

  let (module Compiler : Optcompile.S) =
    Optcompile.native
      (module Unix : Compiler_owee.Unix_intf.S)
      ~flambda2:Flambda2.lambda_to_cmm
  in
  begin try
    Compiler.implementation
      ~start_from:Clflags.Compiler_pass.Parsing
      ~source_file:filename
      ~output_prefix:"program"
      ~keep_symbol_tables:false;
    let exe_name = Filename.temp_file "oxpect_program" ".exe" in
    Compmisc.with_ppf_dump ~file_prefix:exe_name (fun ppf_dump ->
      Compiler.link ~ppf_dump ["./program" ^ Compiler.ext_flambda_obj] exe_name);
    let open Unix in
    let input_rd, input_wr = pipe ~cloexec:true () in
    let output_rd, output_wr = pipe ~cloexec:true () in
    close input_wr;
    let pid = create_process exe_name [| exe_name |] input_rd output_wr output_wr in
    close input_rd;
    close output_wr;
    let ch = in_channel_of_descr output_rd in
    let rec read_all ~tag () =
      match input_line ch with
      | s when String.ends_with ~suffix:oxpect_magic s ->
         if String.length s > String.length oxpect_magic then
           (* No trailing newline on previous line *)
           File.add_text !file ~create:true ~tag ~kind:Output
             (String.sub s 0 (String.length s - String.length oxpect_magic));
         let tag = input_line ch in
         read_all ~tag ()
      | s ->
         File.add_text !file ~create:true ~tag ~kind:Output (s ^ "\n");
         read_all ~tag ()
      | exception End_of_file -> tag
    in
    let tag = read_all ~tag:"" () in
    close_in ch;
    let _, st = waitpid [] pid in
    let status_msg =
      let strsig n =
        Printf.sprintf "%s (%d)" (Sys.signal_to_string n) (Sys.signal_to_int n)
      in
      match st with
      | WEXITED 0 -> None
      | WEXITED n ->
         Some (Printf.sprintf "[Process exit %d]\n" n)
      | WSIGNALED n ->
         Some (Printf.sprintf "[Process killed by %s]\n" (strsig n))
      | WSTOPPED n ->
         Some (Printf.sprintf "[Process stopped by %s]\n" (strsig n))
    in
    status_msg
    |> Option.iter (File.add_text !file ~create:true ~tag ~kind:Output)
  with x ->
    Location.report_exception Format.std_formatter x
  end;
  let original =
    let ic = open_in filename in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  File.reassemble !file original stdout

  

let () =
  match Sys.argv with
  | [| _; filename |] -> run filename
  | _ -> failwith "FIXME arg parsing"
