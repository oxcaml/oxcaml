let run_z3 code =
  let with_temp_file suffix f =
    let filename = Filename.temp_file "oxcaml-z3-" suffix in
    Misc.try_finally
      (fun () -> f filename)
      ~always:(fun () -> Misc.remove_file filename)
  in
  with_temp_file ".smt2" @@ fun input_file ->
  with_temp_file ".out" @@ fun output_file ->
  Out_channel.with_open_text input_file (fun out_channel ->
      Out_channel.output_string out_channel code);
  let command =
    Filename.quote_command "z3" ["-smt2"; input_file] ~stderr:output_file
      ~stdout:output_file
  in
  let ret = Ccomp.command command in
  if ret <> 0 then Misc.fatal_errorf "Z3 failed with return code %d" ret;
  let output = In_channel.with_open_text output_file In_channel.input_all in
  output

module Id_gen = struct
  type t =
    { id_table : int Label.Tbl.t;
      labels_by_id : Label.t array;
      width : int
    }

  let bitwidth_of_count count =
    match count with 0 | 1 -> 1 | num_blocks -> 1 + Misc.log2 (num_blocks - 1)

  let create (cfg : Cfg.t) =
    let labels_by_id = cfg.blocks |> Label.Tbl.to_seq_keys |> Array.of_seq in
    Array.sort Label.compare labels_by_id;
    (* CR hwasilewski for xclerc: do we want this sort? not required but
       provides determins across runs, i.e if we compile the same cfg (with the
       same labels) it will have the same z3 IDs. Not sure there is much gain
       from this. *)
    let block_count = Array.length labels_by_id in
    let id_table = Label.Tbl.create block_count in
    Array.iteri (fun id label -> Label.Tbl.add id_table label id) labels_by_id;
    { id_table; labels_by_id; width = bitwidth_of_count block_count }

  let width t = t.width

  let get_id { id_table; width; labels_by_id = _ } ~label =
    let id_number =
      match Label.Tbl.find_opt id_table label with
      | Some id -> id
      | None ->
        Misc.fatal_errorf "No Z3 id assigned to CFG label %a" Label.format label
    in
    Printf.sprintf "(_ bv%d %d)" id_number width

  let label_of_id t id =
    if id < 0 || id >= Array.length t.labels_by_id
    then Misc.fatal_errorf "Invalid Z3 node ID %d" id;
    t.labels_by_id.(id)
end

let z3_graph_of_cfg fmt ~(cfg : Cfg.t) ~(id_gen : Id_gen.t) =
  Format.fprintf fmt "(rule (entry %s))"
    (Id_gen.get_id id_gen ~label:cfg.entry_label);
  Label.Tbl.iter
    (fun label (value : Cfg.basic_block) ->
      let id = Id_gen.get_id id_gen ~label in
      if not (Cfg.is_never_terminator value.terminator.desc)
      then Format.fprintf fmt "(rule (is-node %s))\n" id;
      Cfg.successor_labels ~exn:true ~normal:true value
      |> Label.Set.iter (fun succ_label ->
          let succ_id = Id_gen.get_id id_gen ~label:succ_label in
          Format.fprintf fmt "(rule (edge %s %s))\n" id succ_id))
    cfg.blocks

let fmt_dom_code_begin fmt ~id_gen =
  let width = Id_gen.width id_gen in
  Format.fprintf fmt
    {|
(define-sort node () (_ BitVec %d))

(declare-rel edge (node node))
(declare-rel is-node (node))
(declare-rel entry (node))
(declare-rel reachable (node))
(declare-rel not-dom (node node))
(declare-rel dom (node node))
(declare-rel strict-dom (node node))
(declare-rel not-idom (node node))
(declare-rel idom (node node))
(declare-rel df (node node))

(declare-var a node)
(declare-var b node)
(declare-var c node)

(rule (=> (edge a b) (is-node a)))
(rule (=> (edge a b) (is-node b)))
(rule (=> (entry a) (is-node a)))

(rule (=> (entry a) (reachable a)))
(rule (=> (and (reachable a) (edge a b))
          (reachable b)))

(rule (=> (and (is-node a) (entry b) (distinct a b))
          (not-dom b a)))

(rule (=> (and (edge b c) (not-dom b a) (distinct a c))
          (not-dom c a)))

(rule (=> (and (reachable a) (reachable b) (not (not-dom b a)))
          (dom b a)))

(rule (=> (and (dom a b) (dom a c) (dom b c) (distinct a b) (distinct b c))
          (not-idom a c)))

(rule (=> (and (dom a b) (distinct a b) (not (not-idom a b)))
          (idom a b)))

(rule (=> (and (dom b a) (distinct a b)) (strict-dom b a)))
(rule (=> (and (edge b c) (dom b a) (not (strict-dom c a))) (df a c)))
|}
    width

let fmt_dom_code_end fmt =
  Format.pp_print_string fmt
    {|
(echo "BEGIN_IDOM")
(query idom :print-answer true)
(echo "END_IDOM")
(echo "BEGIN_DF")
(query df :print-answer true)
(echo "END_DF")
|}

(* CR hwasilewski for xclerc: here begins the parsing code written by GPT, I
   haven't really read it *)
let lines_between ~begin_marker ~end_marker output =
  let rec drop_until_marker = function
    | [] -> Misc.fatal_errorf "Marker %S not found in Z3 output" begin_marker
    | line :: lines ->
      if String.equal (String.trim line) begin_marker
      then lines
      else drop_until_marker lines
  in
  let rec take_until_marker acc = function
    | [] -> Misc.fatal_errorf "Marker %S not found in Z3 output" end_marker
    | line :: lines ->
      if String.equal (String.trim line) end_marker
      then List.rev acc
      else take_until_marker (line :: acc) lines
  in
  output |> String.split_on_char '\n' |> drop_until_marker
  |> take_until_marker []

let is_binary_digit = function
  | '0' | '1' -> true
  | '2' .. '9' | 'a' .. 'f' | 'A' .. 'F' | _ -> false

let bitvector_tokens line =
  let length = String.length line in
  let rec find_end is_digit index =
    if index < length && is_digit line.[index]
    then find_end is_digit (index + 1)
    else index
  in
  let rec loop index tokens =
    if index + 2 > length
    then List.rev tokens
    else if
      Char.equal line.[index] '#'
      && (Char.equal line.[index + 1] 'x' || Char.equal line.[index + 1] 'b')
    then
      let is_digit =
        if Char.equal line.[index + 1] 'x'
        then Char.Ascii.is_hex_digit
        else is_binary_digit
      in
      let end_index = find_end is_digit (index + 2) in
      if end_index = index + 2
      then loop (index + 1) tokens
      else
        let token = String.sub line index (end_index - index) in
        loop end_index (token :: tokens)
    else loop (index + 1) tokens
  in
  loop 0 []

let int_of_bitvector token =
  let length = String.length token in
  if length <= 2 || not (Char.equal token.[0] '#')
  then Misc.fatal_errorf "Invalid bitvector in Z3 output: %S" token;
  let prefix =
    match token.[1] with
    | 'x' -> "0x"
    | 'b' -> "0b"
    | _ -> Misc.fatal_errorf "Invalid bitvector in Z3 output: %S" token
  in
  let digits = String.sub token 2 (length - 2) in
  match int_of_string_opt (prefix ^ digits) with
  | Some value -> value
  | None -> Misc.fatal_errorf "Invalid bitvector in Z3 output: %S" token

let parse_idom_pairs output =
  let lines =
    lines_between ~begin_marker:"BEGIN_IDOM" ~end_marker:"END_IDOM" output
    |> List.filter_map (fun line ->
        let line = String.trim line in
        if String.equal line "" then None else Some line)
  in
  match lines with
  | [] -> Misc.fatal_error "Missing IDOM query result in Z3 output"
  | "unsat" :: _ -> []
  | "sat" :: answer_lines ->
    List.fold_left
      (fun pairs line ->
        match bitvector_tokens line with
        | [] -> pairs
        | [node; immediate_dominator] ->
          (int_of_bitvector node, int_of_bitvector immediate_dominator) :: pairs
        | tokens ->
          Misc.fatal_errorf
            "Unexpected bitvectors in IDOM answer line %S: expected 2, got %d"
            line (List.length tokens))
      [] answer_lines
    |> List.rev
  | result :: _ ->
    Misc.fatal_errorf "Unexpected IDOM query result from Z3: %S" result

let parse_doms ~(id_gen : Id_gen.t) ~entry_label output =
  let doms = Label.Tbl.create (Array.length id_gen.labels_by_id) in
  Label.Tbl.add doms entry_label entry_label;
  parse_idom_pairs output
  |> List.iter (fun (node_id, immediate_dominator_id) ->
      let node = Id_gen.label_of_id id_gen node_id in
      let immediate_dominator =
        Id_gen.label_of_id id_gen immediate_dominator_id
      in
      if Label.Tbl.mem doms node
      then
        Misc.fatal_errorf "Z3 returned multiple IDOMs for label %a" Label.format
          node;
      Label.Tbl.add doms node immediate_dominator);
  doms
