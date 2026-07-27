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
  let output = In_channel.with_open_text output_file In_channel.input_all in
  if ret <> 0
  then
    Misc.fatal_errorf "Z3 failed with return code %d. Input: @.%s@.Output: @.%s"
      ret code output;
  output

let fmt_fact fmt relation arguments =
  let fmt_argument fmt argument = Format.fprintf fmt " %s" argument in
  Format.fprintf fmt "(rule (%s%a))@." relation
    (Format.pp_print_list fmt_argument)
    arguments

module type Id_gen_S = sig
  type key

  type t

  val create : key list -> t

  val get_id : t -> key:key -> string

  val width : t -> int

  val key_of_id_exn : t -> int -> key
end

module type Id_key = sig
  type t

  val compare : t -> t -> int

  val format : Format.formatter -> t -> unit

  module Tbl : Hashtbl.S with type key = t
end

module Make_id_gen (Key : Id_key) = struct
  type key = Key.t

  type t =
    { id_table : int Key.Tbl.t;
      keys_by_id : key array;
      width : int
    }

  let bitwidth_of_count count =
    match count with 0 | 1 -> 1 | num_blocks -> 1 + Misc.log2 (num_blocks - 1)

  let create (keys : key list) =
    let keys_by_id = keys |> List.sort_uniq Key.compare |> Array.of_list in
    let key_count = Array.length keys_by_id in
    let id_table = Key.Tbl.create key_count in
    Array.iteri (fun id key -> Key.Tbl.add id_table key id) keys_by_id;
    { id_table; keys_by_id; width = bitwidth_of_count key_count }

  let width t = t.width

  let get_id { id_table; width; keys_by_id = _ } ~key =
    let id_number =
      match Key.Tbl.find_opt id_table key with
      | Some id -> id
      | None -> Misc.fatal_errorf "No Z3 id assigned to %a" Key.format key
    in
    Printf.sprintf "(_ bv%d %d)" id_number width

  let key_of_id_exn t id =
    if id < 0 || id >= Array.length t.keys_by_id
    then Misc.fatal_errorf "Invalid Z3 node ID %d" id;
    t.keys_by_id.(id)
end

module Label_id_gen = Make_id_gen (Label)
module Instruction_id_gen = Make_id_gen (InstructionId)

module Reg_id_gen = Make_id_gen (struct
  type t = Reg.t

  let compare = Reg.compare

  let format = Printreg.reg

  module Tbl = Reg.Tbl
end)

let create_label_id_gen (cfg : Cfg.t) =
  cfg.blocks |> Label.Tbl.to_seq_keys |> List.of_seq |> Label_id_gen.create

let create_instruction_id_gen (cfg : Cfg.t) =
  Cfg.fold_all_instructions cfg ~init:[]
    ~f:
      { f =
          (fun instruction_ids instruction ->
            instruction.Cfg.id :: instruction_ids)
      }
  |> Instruction_id_gen.create

let create_reg_id_gen (cfg : Cfg.t) =
  let add_instruction_regs : type a.
      Reg.t list -> a Cfg.instruction -> Reg.t list =
   fun regs instruction ->
    let add_regs regs regs_to_add =
      Array.fold_left (fun regs reg -> reg :: regs) regs regs_to_add
    in
    let regs = add_regs regs instruction.arg in
    add_regs regs instruction.res
  in
  let init_regs = Proc.loc_exn_bucket :: Array.to_list cfg.fun_args in
  Cfg.fold_all_instructions cfg ~init:init_regs ~f:{ f = add_instruction_regs }
  |> Reg_id_gen.create

let z3_graph_of_cfg fmt ~(cfg : Cfg.t) ~(id_gen : Label_id_gen.t) =
  fmt_fact fmt "entry" [Label_id_gen.get_id id_gen ~key:cfg.entry_label];
  Label.Tbl.iter
    (fun label (value : Cfg.basic_block) ->
      let id = Label_id_gen.get_id id_gen ~key:label in
      fmt_fact fmt "is-node" [id];
      Cfg.successor_labels ~exn:true ~normal:true value
      |> Label.Set.iter (fun succ_label ->
          let succ_id = Label_id_gen.get_id id_gen ~key:succ_label in
          fmt_fact fmt "edge" [id; succ_id]))
    cfg.blocks

let fmt_dom_code_begin fmt ~id_gen =
  let width = Label_id_gen.width id_gen in
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

let fmt_liveness_code_begin fmt instr_id_gen reg_id_gen =
  Format.fprintf fmt
    {|
(define-sort instr () (_ BitVec %d))
(define-sort reg () (_ BitVec %d))

(declare-rel next (instr instr))
(declare-rel arg (instr reg))
(declare-rel res (instr reg))
(declare-rel exn-next (instr instr))
(declare-rel exn-bucket (reg))
(declare-rel expected-before (instr reg))
(declare-rel expected-across (instr reg))
(declare-rel tailcall-self (instr))

(declare-rel before (instr reg))
(declare-rel across (instr reg))
(declare-rel not-removable (instr))
(declare-rel bad ())

(declare-var i0 instr)
(declare-var i1 instr)
(declare-var r reg)

(rule (=> (and (res i0 r) (next i0 i1) (before i1 r)) (not-removable i0)))

(rule (=> (and (next i0 i1) (before i1 r)
               (not (res i0 r))
               (not (tailcall-self i0)))
          (across i0 r)))

(rule (=> (and (exn-next i0 i1) (before i1 r) (not (exn-bucket r))) (across i0 r)))

(rule (=> (across i0 r) (before i0 r)))
(rule (=> (and (not-removable i0) (arg i0 r)) (before i0 r)))

(rule (=> (and (not (expected-before i0 r)) (before i0 r)) bad))
(rule (=> (and (expected-before i0 r) (not (before i0 r))) bad))

(rule (=> (and (not (expected-across i0 r)) (across i0 r)) bad))
(rule (=> (and (expected-across i0 r) (not (across i0 r))) bad))
|}
    (Instruction_id_gen.width instr_id_gen)
    (Reg_id_gen.width reg_id_gen)

let fmt_liveness_code_end fmt = Format.pp_print_string fmt "(query bad)"

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

let parse_doms ~(id_gen : Label_id_gen.t) ~entry_label output =
  let doms = Label.Tbl.create (Array.length id_gen.keys_by_id) in
  Label.Tbl.add doms entry_label entry_label;
  parse_idom_pairs output
  |> List.iter (fun (node_id, immediate_dominator_id) ->
      let node = Label_id_gen.key_of_id_exn id_gen node_id in
      let immediate_dominator =
        Label_id_gen.key_of_id_exn id_gen immediate_dominator_id
      in
      if Label.Tbl.mem doms node
      then
        Misc.fatal_errorf "Z3 returned multiple IDOMs for label %a" Label.format
          node;
      Label.Tbl.add doms node immediate_dominator);
  doms
