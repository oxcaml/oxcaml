[@@@ocaml.warning "+a-40-41-42"]

type doms = Label.t Label.Tbl.t

let fmt_dom_code_begin fmt ~id_gen =
  let width = Cfg_z3.Label_id_gen.width id_gen in
  Format.fprintf fmt
    {|
(set-option :fp.engine datalog)

(define-sort node () (_ BitVec %d))

(declare-rel edge (node node))
(declare-rel is-node (node))
(declare-rel entry (node))
(declare-rel reachable (node))
(declare-rel not-dom (node node))
(declare-rel dom (node node))
(declare-rel not-idom (node node))
(declare-rel idom (node node))

(declare-var a node)
(declare-var b node)
(declare-var c node)

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
|}
    width

let fmt_dom_code_end fmt =
  Format.pp_print_string fmt
    {|
(echo "BEGIN_IDOM")
(query idom :print-answer true)
(echo "END_IDOM")
|}

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

let parse_doms ~(id_gen : Cfg_z3.Label_id_gen.t) ~entry_label output =
  let doms = Label.Tbl.create (Cfg_z3.Label_id_gen.length id_gen) in
  Label.Tbl.add doms entry_label entry_label;
  parse_idom_pairs output
  |> List.iter (fun (node_id, immediate_dominator_id) ->
      let node = Cfg_z3.Label_id_gen.key_of_id_exn id_gen node_id in
      let immediate_dominator =
        Cfg_z3.Label_id_gen.key_of_id_exn id_gen immediate_dominator_id
      in
      if Label.Tbl.mem doms node
      then
        Misc.fatal_errorf "Z3 returned multiple IDOMs for label %a" Label.format
          node;
      Label.Tbl.add doms node immediate_dominator);
  doms

let debug_print_dom doms =
  Label.Map.iter
    (fun label immediate_dominator ->
      Format.eprintf "%a -> %a@." Label.format label Label.format
        immediate_dominator)
    (Label.Tbl.to_map doms)

let calculate_idom (cfg : Cfg.t) : doms =
  let buffer = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer buffer in
  let id_gen = Cfg_z3.create_label_id_gen cfg in
  fmt_dom_code_begin fmt ~id_gen;
  Cfg_z3.z3_graph_of_cfg fmt ~cfg ~id_gen;
  fmt_dom_code_end fmt;
  Format.pp_print_flush fmt ();
  let z3_output = Buffer.contents buffer |> Cfg_z3.run_z3 in
  parse_doms ~id_gen ~entry_label:cfg.entry_label z3_output

(* CR hwasilewski: add validators for the dominance frontier and forest. *)
let validate_idom (cfg : Cfg.t) (doms : doms) =
  (* CR hwasilewski: Note: we assume that cfg has no dead code here. *)
  let z3_doms = calculate_idom cfg in
  let doms_equal =
    Label.Map.equal Label.equal (Label.Tbl.to_map doms)
      (Label.Tbl.to_map z3_doms)
  in
  if not doms_equal
  then (
    Format.eprintf "CFG dominators:@.";
    debug_print_dom doms;
    Format.eprintf "Z3 dominators:@.";
    debug_print_dom z3_doms;
    (* CR hwasilewski for xclerc: cannot import Printcfg here, it causes a
       cyclic dependency. *)
    Misc.fatal_errorf
      "validate_idoms: Dominator validation failed: dominator calculated by \
       Datalog does not agree with the Cfg_dominators, cfg '%s'"
      cfg.fun_name)
