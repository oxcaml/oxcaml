(* The counters of the conditional branches that [Branch_relaxation] relaxes
   because their target is too far: the sequences that replace them have none,
   so they are dropped (see [Fdo_dropped_counters]). The target is a fake one,
   whose conditional branches reach 8 instructions and whose relaxed sequences
   reach anywhere. The test prints the Linear code after relaxation, with the
   padding elided, and checks that every counter is still in the code or
   dropped. *)

let () =
  Env.set_current_unit
    (Unit_info.make ~source_file:"branch_relaxation_counters.ml"
       ~for_pack_prefix:Compilation_unit.Prefix.empty Impl
       "branch_relaxation_counters")

let function_ =
  Fdo_counter.Function.create_original
    (Fdo_counter.Name.create ~name_str:"Test.f" ~discriminator:0)

let body_hash = Fdo_counter.Body_hash.of_int32 1l

let counter arm =
  Fdo_counter.of_position
    (Fdo_counter.Position.create_branch ~function_ ~body_hash ~index:0 ~arm)

let annotation arm = Fdo_annotation.singleton (counter arm)

let successor target arm : Linear.successor =
  { target; fdo_annotation = annotation arm }

module Target = struct
  type distance = int

  type relaxed_instruction =
    | Condbranch of Operation.test * Label.t
    | Branch of Label.t

  let offset_pc_at_branch = 0

  let relaxed_instruction_size _ : Branch_relaxation_intf.instruction_size =
    { size = 1; max_displacement = None }

  let relaxed_instruction_desc : relaxed_instruction -> Linear.instruction_desc
      = function
    | Condbranch (test, target) ->
      Lcondbranch
        { test;
          taken = { target; fdo_annotation = Fdo_annotation.empty };
          fallthrough_annotation = Fdo_annotation.empty
        }
    | Branch target -> Lbranch target

  let relax_poll () = assert false

  let relax_allocation ~num_bytes:_ ~dbginfo:_ ~res:_ ~mode:_ = assert false

  let relax_stackcheck ~max_frame_size_bytes:_ = assert false

  let relax_condbranch test target ~arg:_ = Condbranch (test, target)

  let relax_branch target = Branch target
end

module Relaxation = Branch_relaxation.Make (Target)

(* A pseudo-register, so that the output names no register of the target.
   Pseudo-registers are numbered after the target's physical registers, of which
   each target has a different number, so the numbering restarts at 0. *)
let reg =
  Reg.For_testing.set_state ~stamp:0 ~relocatable_regs:[];
  Reg.create Cmm.Int

let near = Label.new_label ()

let far = Label.new_label ()

let padding = 40

let code =
  ([ Lcondbranch
       { test = Iinttest_imm (Ceq, 0);
         taken = successor far 0;
         fallthrough_annotation = annotation 1
       };
     (* Relaxed as a whole, although only one of its targets is far. *)
     Lcondbranch3
       { lt = Some (successor far 2);
         eq = None;
         gt = Some (successor near 3);
         fallthrough_annotation = annotation 4
       };
     Lcondbranch
       { test = Iinttest_imm (Clt, 0);
         taken = successor near 5;
         fallthrough_annotation = annotation 6
       };
     Llabel_for_jump_target near ]
    : Linear.instruction_desc list)
  @ List.init padding (fun _ : Linear.instruction_desc -> Lop Pause)
  @ [Linear.Llabel_for_jump_target far; Linear.Lreturn]

let size (desc : Linear.instruction_desc) :
    Branch_relaxation_intf.instruction_size =
  match[@warning "-fragile-match"] desc with
  | Lcondbranch _ | Lcondbranch3 _ -> { size = 1; max_displacement = Some 20 }
  | Llabel_for_jump_target _ -> { size = 0; max_displacement = None }
  | _ -> { size = 1; max_displacement = None }

let body =
  List.fold_right
    (fun desc next ->
      Linear.instr_cons desc [| reg |] [||] next
        ~available_before:Reg_availability_set.Unreachable
        ~available_across:Reg_availability_set.Unreachable
        ~phantom_available_before:None)
    code Linear.end_instr

let rec counters (instr : Linear.instruction) =
  let of_successor (successor : Linear.successor option) =
    Option.fold ~none:[] successor ~some:(fun (successor : Linear.successor) ->
        Fdo_annotation.to_list successor.fdo_annotation)
  in
  match[@warning "-fragile-match"] instr.desc with
  | Lend -> []
  | Lcondbranch { taken; fallthrough_annotation; test = _ } ->
    of_successor (Some taken)
    @ Fdo_annotation.to_list fallthrough_annotation
    @ counters instr.next
  | Lcondbranch3 { lt; eq; gt; fallthrough_annotation } ->
    List.concat_map of_successor [lt; eq; gt]
    @ Fdo_annotation.to_list fallthrough_annotation
    @ counters instr.next
  | _ -> counters instr.next

let rec print (instr : Linear.instruction) =
  match[@warning "-fragile-match"] instr.desc with
  | Lend -> ()
  | Lop Pause ->
    let rec skip (instr : Linear.instruction) =
      match[@warning "-fragile-match"] instr.desc with
      | Lop Pause -> skip instr.next
      | _ -> instr
    in
    Format.printf "  (padding)@.";
    print (skip instr)
  | _ ->
    Format.printf "  %a@." Printlinear.instr instr;
    print instr.next

let print_counters title counters =
  Format.printf "@[<hov 2>%s:%a@]@." title
    (Format.pp_print_list
       ~pp_sep:(fun _ () -> ())
       (fun ppf counter ->
         Format.fprintf ppf "@ %s" (Fdo_counter.to_string counter)))
    counters

let mem counter counters = List.exists (Fdo_counter.equal counter) counters

let () =
  (* One instruction per line. *)
  Format.set_margin 200;
  Format.set_max_indent 190;
  Format.printf "Labels: near = %a, far = %a@.@." Label.format near Label.format
    far;
  let before = counters body in
  Fdo_dropped_counters.reset ();
  Relaxation.relax body ~initial_sizes:(List.map size code)
    ~out_of_line_code_block_sizes:[];
  let after = counters body in
  let dropped = Fdo_dropped_counters.registered () in
  print body;
  Format.printf "@.%a" Fdo_dropped_counters.print dropped;
  let dropped = List.map fst (Fdo_dropped_counters.to_list dropped) in
  print_counters "lost"
    (List.filter
       (fun counter -> not (mem counter after || mem counter dropped))
       before)
