open Ocaml_ir_common
open Std
open Ocaml_ir_fiber
let tokenize s =
  ((s |>
      (String.map ~f:(function | '\t' | '\n' | '\r' | ' ' -> ' ' | c -> c)))
     |> (String.split_on_char ~sep:' '))
    |> (List.filter ~f:(fun s -> (String.length s) > 0))
let starts_with ~prefix l =
  let prefix_len = String.length prefix in
  if prefix_len <= (String.length l)
  then
    let rec iterate_from_start i =
      if i = prefix_len
      then true
      else
        if (Char.compare (prefix.[i]) (l.[i])) = 0
        then iterate_from_start (i + 1)
        else false in
    iterate_from_start 0
  else false
let last_char l = l.[(String.length l) - 1]
let parse =
  function
  | ".loc"::file_id::line::col::[] ->
      (match ((int_of_string_opt file_id), (int_of_string_opt line),
               (int_of_string_opt col))
       with
       | (Some file_id, Some line, Some col) -> `Loc (file_id, (line, col))
       | _ -> `Other)
  | ".file"::id::filename::[] ->
      (match int_of_string_opt id with
       | Some id ->
           let filename =
             if (Char.compare (filename.[0]) '"') = 0
             then
               String.sub filename ~pos:1 ~len:((String.length filename) - 2)
             else filename in
           `File (id, filename)
       | None -> `Other)
  | ".cfi_startproc"::[] -> `Cfi_startproc
  | ".cfi_endproc"::[] -> `Cfi_endproc
  | tok::[] when
      (starts_with ~prefix:"caml" tok) &&
        ((Char.compare (last_char tok) ':') = 0)
      -> `Symbol
  | tok::[] when
      (starts_with ~prefix:".L" tok) &&
        ((Char.compare (last_char tok) ':') = 0)
      -> `Local_label
  | "call"::"caml_call_gc@PLT"::_ -> `Call_caml_gc
  | ".cfi_adjust_cfa_offset"::_ -> `Ignore
  | _ -> `Other
module State =
  struct
    open Ir_map
    type t =
      {
      mutable cur_ir_pos: Location.Position.t ;
      mutable last_pos_pair: (Line_col.t * Location.Position.t) option ;
      mutable last_end_of_significant_line: Location.Position.t ;
      mutable last_nonfunc_expr: (Line_col.t * Location.Position.t) option ;
      mutable last_function_label: Location.Position.t ;
      mutable last_startproc: (Line_col.t * Location.Position.t) option ;
      mutable mappings: (Line_col.t * Location.Simple.t) list ;
      filenames: (int, string) Hashtbl.t ;
      mutable output: string list ;
      mutable inlined_from_file: int option }
    let create file  : t=
      let cur_ir_pos =
        let open Location.Position in
          { pos_lnum = 1; pos_bol = 0; pos_cnum = 0; pos_fname = file } in
      {
        cur_ir_pos;
        last_pos_pair = None;
        last_end_of_significant_line = cur_ir_pos;
        last_nonfunc_expr = None;
        last_function_label = cur_ir_pos;
        last_startproc = None;
        mappings = [];
        filenames = (Hashtbl.create 4);
        output = [];
        inlined_from_file = None
      }
    let formatted_content s = String.concat ~sep:"\n" (List.rev s.output)
  end
let format_file ~file ~content =
  let asm = String.split_lines content in
  let s = State.create file in
  List.iter asm
    ~f:(fun line ->
          let tokens = tokenize line in
          let end_of_cur_line =
            {
              (s.cur_ir_pos) with
              pos_cnum =
                (((s.cur_ir_pos).pos_bol + (String.length line)) + 1)
            } in
          let emit_expr source_expr_start ir_proc_start ir_proc_end =
            let mapping =
              (source_expr_start,
                {
                  Location.Simple.loc_start = ir_proc_start;
                  loc_end = ir_proc_end
                }) in
            s.mappings <- (mapping :: (s.mappings)) in
          let try_emit_nonfunc_expr () =
            match s.last_nonfunc_expr with
            | None -> ()
            | Some (source_expr_start, ir_proc_start) ->
                (emit_expr source_expr_start ir_proc_start
                   s.last_end_of_significant_line;
                 s.last_nonfunc_expr <- None) in
          let emit_line line =
            s.output <- (line :: (s.output));
            s.cur_ir_pos <-
              ((let pos = s.cur_ir_pos in
                let line_length = (String.length line) + 1 in
                {
                  pos_lnum = (pos.pos_lnum + 1);
                  pos_bol = (pos.pos_bol + line_length);
                  pos_cnum = (pos.pos_bol + line_length);
                  pos_fname = file
                })) in
          let close_inlining_frame () =
            match s.inlined_from_file with
            | None -> ()
            | Some _ ->
                (emit_line "; (Inlining done)"; s.inlined_from_file <- None) in
          let parsed = parse tokens in
          (match parsed with
           | `File (file_id, filename) ->
               Hashtbl.add s.filenames file_id filename
           | `Loc (file_id, source_pos) ->
               (try_emit_nonfunc_expr ();
                if file_id = 1
                then
                  (s.last_pos_pair <- (Some (source_pos, s.cur_ir_pos));
                   close_inlining_frame ())
                else
                  (match Hashtbl.find_opt s.filenames file_id with
                   | Some f ->
                       (if
                          not
                            (Option.equal Int.equal (Some file_id)
                               s.inlined_from_file)
                        then emit_line ("; (Inlined from " ^ (f ^ ")"));
                        s.inlined_from_file <- (Some file_id))
                   | None -> emit_line "; (Inlined from an unknown file)"))
           | `Symbol ->
               (try_emit_nonfunc_expr ();
                s.last_function_label <- (s.cur_ir_pos);
                emit_line line)
           | `Cfi_startproc ->
               (match s.last_pos_pair with
                | Some (source_line_col, _) ->
                    s.last_startproc <-
                      (Some (source_line_col, s.last_function_label))
                | None -> ())
           | `Cfi_endproc ->
               (try_emit_nonfunc_expr ();
                close_inlining_frame ();
                (match s.last_startproc with
                 | Some (source_proc_start, ir_proc_start) ->
                     (emit_expr source_proc_start ir_proc_start
                        end_of_cur_line;
                      s.last_startproc <- None)
                 | None -> ()))
           | `Call_caml_gc -> (try_emit_nonfunc_expr (); emit_line line)
           | `Local_label -> emit_line line
           | `Ignore -> ()
           | `Other ->
               ((match s.last_pos_pair with
                 | Some expr_start ->
                     s.last_nonfunc_expr <- (Some expr_start)
                 | None -> ());
                s.last_end_of_significant_line <- end_of_cur_line;
                emit_line ("  " ^ line)));
          (match parsed with | `Loc _ -> () | _ -> s.last_pos_pair <- None));
  ((State.formatted_content s), s.mappings)
let filter_out_unused_mappings asm_mappings source_locations =
  let source_locations_tree =
    (List.map source_locations
       ~f:(fun
             (label,
              ({ Location.Simple.loc_start = loc_start; loc_end;_} as
                 source_location))
             ->
             ((let open Ir_map.Line_col in
                 ((of_position loc_start), (of_position loc_end))),
               (label, source_location))))
      |> Ir_map.Tree.of_alist in
  (List.filter_map asm_mappings
     ~f:(fun (source_line_col, asm_location) ->
           (Ir_map.Tree.fold_range source_locations_tree source_line_col
              ~init:None
              ~f:(fun ~interval ~data:(label_new, source_location_new) agg ->
                    match agg with
                    | None -> Some (interval, label_new, source_location_new)
                    | Some (interval_best_so_far, label, source_location) ->
                        let compare =
                          Ir_map.Line_col.get_better_interval interval
                            interval_best_so_far in
                        if compare < 0
                        then Some (interval, label_new, source_location_new)
                        else
                          Some (interval_best_so_far, label, source_location)))
             |>
             (Option.map
                ~f:(fun (_, label, source_location) ->
                      Ir_map.Entry.create ~label ~source_location
                        ~ir_location:asm_location))))
    |> List.rev
let parse_asm_file ~file ~content source_locations =
  let (asm_output, asm_mappings) = format_file ~file ~content in
  (asm_output, (filter_out_unused_mappings asm_mappings source_locations))
let asm_file output_prefix = (Filename.remove_extension output_prefix) ^ ".s"
let get_mappings ~output_prefix source_locations =
  let asm_file = asm_file output_prefix in
  Let_syntax.bind (Fiber.Sys.file_exists asm_file)
    ~f:(fun file_exists ->
          if file_exists = `Yes
          then
            Let_syntax.map (Fiber.Io.read_file ~path:asm_file)
              ~f:(fun content ->
                    parse_asm_file ~file:asm_file ~content source_locations)
          else Fiber.return ("", []))
module For_testing = struct let format_file = format_file end
