[@@@ocaml.warning "+a-40-41-42"]

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

  val length : t -> int

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

  let length t = Array.length t.keys_by_id

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
