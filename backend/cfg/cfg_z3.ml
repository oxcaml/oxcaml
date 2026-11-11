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

let run_validation_fallback code =
  match run_z3 code |> String.trim with
  | "unsat" -> "Z3 accepted the compiler result; internal Datalog failed"
  | "sat" -> "Z3 also rejected the compiler result"
  | output -> Format.sprintf "unexpected Z3 output: %S" output
  | exception exn -> Format.sprintf "Z3 raised: %s" (Printexc.to_string exn)

let fmt_fact fmt relation arguments =
  let fmt_argument fmt argument = Format.fprintf fmt " %s" argument in
  Format.fprintf fmt "(rule (%s%a))@." relation
    (Format.pp_print_list fmt_argument)
    arguments

let bitwidth_of_count count =
  match count with 0 | 1 -> 1 | count -> 1 + Misc.log2 (count - 1)

module Instruction_id_gen = struct
  type t = { width : int }

  let create ids =
    let max_id =
      List.fold_left
        (fun max_id id -> Int.max max_id (InstructionId.to_int_unsafe id))
        0 ids
    in
    { width = bitwidth_of_count (max_id + 1) }

  let width t = t.width

  let get_id_int _t ~key = InstructionId.to_int_unsafe key

  let get_id ({ width; _ } as t) ~key =
    Printf.sprintf "(_ bv%d %d)" (get_id_int t ~key) width
end

module Reg_id_gen = struct
  type t =
    { id_table : int Reg.Tbl.t;
      width : int
    }

  let create regs =
    let regs = List.sort_uniq Reg.compare regs in
    let reg_count = List.length regs in
    let id_table = Reg.Tbl.create reg_count in
    List.iteri (fun id reg -> Reg.Tbl.add id_table reg id) regs;
    { id_table; width = bitwidth_of_count reg_count }

  let width t = t.width

  let get_id_int { id_table; width = _ } ~key =
    match Reg.Tbl.find_opt id_table key with
    | Some id -> id
    | None -> Misc.fatal_errorf "No Z3 id assigned to %a" Printreg.reg key

  let get_id ({ width; _ } as t) ~key =
    Printf.sprintf "(_ bv%d %d)" (get_id_int t ~key) width
end

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
