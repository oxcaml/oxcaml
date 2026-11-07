(** This module implements a validator for register allocation. The algorithm is
    based on a paper by Silvain Rideau and Xavier Leroy titled "Validating
    Register Allocation and Spilling" which can be found here

    [1] https://xavierleroy.org/publi/validation-regalloc.pdf

    The solution is adapted to the different representation of CFG that is used
    in this compiler, including exception handling. Also, the arguments for a
    function call are specified as preassigned registers instead of
    reconstructing the argument locations from the function type. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR-soon xclerc for xclerc: try to enable warning 4. *)

open! Int_replace_polymorphic_compare
module DLL = Doubly_linked_list
include Cfg_intf.S

module Location : sig
  type t

  val of_reg : Reg.t -> t option

  val of_reg_exn : Reg.t -> t

  val of_regs_exn : Reg.t array -> t array

  val to_loc_lossy : t -> Reg.location

  val print : Cmm.machtype_component -> Format.formatter -> t -> unit

  val compare : t -> t -> int

  val equal : t -> t -> bool

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end = struct
  module Stack = struct
    (** This type is based on [Reg.stack_location]. The first difference is that
        for [Stack (Local index)] this types additionally stores [stack_class]
        because local stacks are separate for different stack slot classes.
        Secondly for all stacks it stores index in words and not byte offset.
        That gives the guarantee that if indices are different then the
        locations do not overlap. *)
    type t =
      | Local of
          { index : int;
            stack_class : Stack_class.t
          }
      | Incoming of { index : int }
      | Outgoing of { index : int }
      | Domainstate of { index : int }

    let byte_bits = 8

    let word_size =
      (* CR-someday azewierzejew: The current implementation is limited to 64
         bit architecture because then all values are of the same size and
         alignment. With that assumption two stack locations can have
         overlapping ranges of bytes only if the have exactly the same byte
         offset.

         On 32 bit architecture floats are still 64 bits but other values are 32
         bits. Then for [DomainState], [Incoming] and [Outgoing] there can be
         two locations with different offsets that overlap. An example is a slot
         for an integer with offset 4 and a slot for a float with offset 0. The
         ranges they refer to are respectively [4..8] and [0..8] so they overlap
         but the offsets are different. *)
      let word_size = 8 in
      if Sys.word_size <> word_size * byte_bits
      then
        Regalloc_utils.fatal
          "regalloc validation only supports 64 bit architecture, got word \
           size %d"
          Sys.word_size;
      word_size

    let byte_offset_to_word_index offset =
      if offset mod word_size <> 0
      then
        Regalloc_utils.fatal
          "regalloc validation expects aligned offsets, got offset %d with \
           remainder %d"
          offset (offset mod word_size);
      offset / word_size

    let word_index_to_byte_offset index = index * word_size

    let of_stack_loc ~stack_class loc =
      match loc with
      | Reg.Local index -> Local { index; stack_class }
      | Reg.Incoming offset ->
        Incoming { index = byte_offset_to_word_index offset }
      | Reg.Outgoing offset ->
        (* macOS on arm requires unaligned stack locations for C calls. *)
        if Target_system.is_macos () && Target_system.is_arm ()
        then Outgoing { index = offset / word_size }
        else Outgoing { index = byte_offset_to_word_index offset }
      | Reg.Domainstate offset ->
        Domainstate { index = byte_offset_to_word_index offset }

    let to_stack_loc_lossy t =
      match t with
      | Local { index; _ } -> Reg.Local index
      | Incoming { index } -> Reg.Incoming (word_index_to_byte_offset index)
      | Outgoing { index } -> Reg.Outgoing (word_index_to_byte_offset index)
      | Domainstate { index } ->
        Reg.Domainstate (word_index_to_byte_offset index)

    let compare (t1 : t) (t2 : t) : int =
      match t1, t2 with
      | ( Local { index = i1; stack_class = c1 },
          Local { index = i2; stack_class = c2 } ) ->
        let c = Int.compare i1 i2 in
        if c <> 0
        then c
        else Int.compare (Stack_class.hash c1) (Stack_class.hash c2)
      | Incoming { index = i1 }, Incoming { index = i2 } -> Int.compare i1 i2
      | Outgoing { index = i1 }, Outgoing { index = i2 } -> Int.compare i1 i2
      | Domainstate { index = i1 }, Domainstate { index = i2 } ->
        Int.compare i1 i2
      | Local _, (Incoming _ | Outgoing _ | Domainstate _) -> -1
      | (Incoming _ | Outgoing _ | Domainstate _), Local _ -> 1
      | Incoming _, (Outgoing _ | Domainstate _) -> -1
      | (Outgoing _ | Domainstate _), Incoming _ -> 1
      | Outgoing _, Domainstate _ -> -1
      | Domainstate _, Outgoing _ -> 1
  end

  type t =
    | Reg of Regs.Phys_reg.t
    | Stack of Stack.t

  let of_reg reg =
    match reg.Reg.loc with
    | Reg.Unknown -> None
    | Reg.Reg idx -> Some (Reg idx)
    | Reg.Stack stack ->
      Some
        (Stack
           (Stack.of_stack_loc
              ~stack_class:(Stack_class.of_machtype reg.Reg.typ)
              stack))

  let of_reg_exn reg = of_reg reg |> Option.get

  let of_regs_exn loc_arr = Array.map of_reg_exn loc_arr

  let to_loc_lossy t =
    match t with
    | Reg idx -> Reg.Reg idx
    | Stack stack -> Reg.Stack (Stack.to_stack_loc_lossy stack)

  let print typ ppf t =
    Printreg.loc ~unknown:(fun _ -> assert false) ppf (to_loc_lossy t) typ

  let compare (t1 : t) (t2 : t) : int =
    match t1, t2 with
    | Reg r1, Reg r2 -> Regs.Phys_reg.compare r1 r2
    | Stack s1, Stack s2 -> Stack.compare s1 s2
    | Reg _, Stack _ -> -1
    | Stack _, Reg _ -> 1

  let equal (t1 : t) (t2 : t) : bool = compare t1 t2 = 0

  module T = struct
    type nonrec t = t

    let compare = compare
  end

  module Set = Set.Make (T)
  module Map = Map.Make (T)
end

module Reg_id : sig
  type t =
    | Preassigned of { location : Location.t }
    | Named of { stamp : Reg.Stamp.t }

  val compare : t -> t -> int

  (** [of_reg reg] is the identity of [reg] before allocation; it checks that
      [reg] has a location iff it is preassigned, which only holds before
      allocation. *)
  val of_reg : Reg.t -> t

  val to_loc_lossy : t -> Reg.location
end = struct
  type t =
    | Preassigned of { location : Location.t }
    | Named of { stamp : Reg.Stamp.t }

  let of_reg (reg : Reg.t) =
    let loc = Location.of_reg reg in
    if not (Bool.equal (Option.is_some loc) (Reg.is_preassigned reg))
    then
      Regalloc_utils.fatal
        "Mismatch between register having location (%b) and register being a \
         preassigned register (%b)"
        (Option.is_some loc) (Reg.is_preassigned reg);
    match loc with
    | Some location -> Preassigned { location }
    | None -> Named { stamp = reg.stamp }

  let to_loc_lossy t =
    match t with
    | Preassigned { location } -> Location.to_loc_lossy location
    | Named _ -> Reg.Unknown

  let compare (t1 : t) (t2 : t) =
    match t1, t2 with
    | Preassigned { location = l1 }, Preassigned { location = l2 } ->
      Location.compare l1 l2
    | Named { stamp = s1 }, Named { stamp = s2 } -> Reg.Stamp.compare s1 s2
    | Preassigned _, Named _ -> -1
    | Named _, Preassigned _ -> 1
end

module Register : sig
  module For_print : sig
    type t
  end

  type t =
    { reg_id : Reg_id.t;
      for_print : For_print.t
    }

  val create : Reg.t -> t

  val to_dummy_reg : t -> Reg.t

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val typ : t -> Cmm.machtype_component

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end = struct
  module For_print = struct
    type t =
      { name : Reg.Name.t;
        stamp : Reg.Stamp.t;
        preassigned : bool;
        typ : Cmm.machtype_component
      }
  end

  type t =
    { reg_id : Reg_id.t;
      for_print : For_print.t
    }

  let create (reg : Reg.t) : t =
    { reg_id = Reg_id.of_reg reg;
      for_print =
        { name = reg.name;
          stamp = reg.stamp;
          preassigned = reg.preassigned;
          typ = reg.typ
        }
    }

  let to_dummy_reg (t : t) : Reg.t =
    let name = t.for_print.name in
    let typ = t.for_print.typ in
    let stamp = t.for_print.stamp in
    let preassigned = t.for_print.preassigned in
    let loc = Reg_id.to_loc_lossy t.reg_id in
    Reg.For_printing.create ~name ~typ ~stamp ~preassigned ~loc

  let typ (t : t) = t.for_print.typ

  let print (ppf : Format.formatter) (t : t) : unit =
    match t.reg_id with
    | Preassigned { location } ->
      Format.fprintf ppf "R[%a]" (Location.print t.for_print.typ) location
    | Named _ -> Printreg.reg ppf (to_dummy_reg t)

  let compare (t1 : t) (t2 : t) : int = Reg_id.compare t1.reg_id t2.reg_id

  let equal (t1 : t) (t2 : t) : bool = compare t1 t2 = 0

  module T = struct
    type nonrec t = t

    let compare = compare
  end

  module Set = Set.Make (T)
  module Map = Map.Make (T)
end

module Instruction = struct
  type 'a t =
    { desc : 'a;
      arg : Register.t array;
      res : Register.t array
    }

  module Kind = struct
    type 'a t =
      | Terminator : terminator t
      | Basic : basic t
  end

  let to_prealloc (type a) ~(alloced : a instruction) (t : a t) : a instruction
      =
    { alloced with
      arg = Array.map Register.to_dummy_reg t.arg;
      res = Array.map Register.to_dummy_reg t.res
    }
end

(* The split pass may rematerialize a register by inserting a copy of a pure,
   side-effect-free instruction (an immutable load, a constant, an [Intop_imm]
   from a safe subset, ...) that originally produced its value. Such an
   instruction is a legitimate addition during register allocation even though
   its [desc] is not [Op (Spill | Reload | Move)]. The exact set of accepted
   shapes is defined by [Regalloc_split_utils.is_rematerializable_shape] and
   shared with the classification side ([Regalloc_split_utils.Uses.compute]) so
   the two ends stay in lock-step. *)
let is_rematerialized_shape (instr : basic Cfg.instruction) =
  Regalloc_split_utils.is_rematerializable_shape instr.desc

module Description : sig
  (** A pre-allocation snapshot of instruction [desc], [arg] and [res] fields
      and [fun_args], rather than a copy of the mutable CFG. Instructions are
      indexed by ID; their original block labels are recorded for the cycle and
      dominance checks on rematerializations.

      The validator allows deletion of pure basic instructions with dead results
      and insertion of spills, reloads, moves, checked rematerializations, and
      [Always] terminators for edge splitting. It checks surviving instruction
      order and requires [fun_args] to retain their preassigned locations.

      It assumes surviving instructions remain in their original blocks and CFG
      changes only subdivide edges. *)
  type t

  (** Returns the snapshot of a basic instruction, or [None] if it was added
      during allocation. *)
  val find_basic : t -> basic instruction -> basic Instruction.t option

  (** Returns the snapshot of a terminator, or [None] if it was added during
      allocation. *)
  val find_terminator :
    t -> terminator instruction -> terminator Instruction.t option

  val create : Cfg_with_layout.t -> t option

  (** What [verify] establishes about the instructions the register allocator
      deleted or added, for the equation transfer:

      - the description instructions (with their ids) that the allocator
        deleted, indexed by their effective successor in the pre-allocation
        instruction order (a surviving basic instruction, or a terminator);
        [verify] checks that every deleted instruction is pure, and the transfer
        then checks at the successor that their results are dead (cf.
        [Transfer.check_deleted_before]);

      - for each rematerialized instruction the allocator added, the
        description-level instruction it is a copy of, with the rematerialized
        register as its result; [verify] checks the rematerialization the
        allocator recorded against the description (cf.
        [verify_rematerialization]). *)
  type verification

  val verify :
    t ->
    rematerializations:Regalloc_rematerializations.t ->
    Cfg_with_layout.t ->
    verification

  val find_deleted_before :
    verification ->
    InstructionId.t ->
    (InstructionId.t * basic Instruction.t) list

  val find_rematerialized :
    verification -> InstructionId.t -> basic Instruction.t option

  val reg_fun_args : t -> Register.t array
end = struct
  type basic_info =
    { successor_id : InstructionId.t;
      instr : basic Instruction.t
    }

  type t =
    { instructions : (InstructionId.t, basic_info) Hashtbl.t;
      terminators : (InstructionId.t, terminator Instruction.t) Hashtbl.t;
      first_instruction_in_block : InstructionId.t Label.Tbl.t;
      instruction_labels : (InstructionId.t, Label.t) Hashtbl.t;
          (* The block of every instruction, basic or terminator, for
             [verify_rematerialization]. *)
      reg_fun_args : Register.t array
    }

  type verification =
    { deleted_before :
        ( InstructionId.t,
          (InstructionId.t * basic Instruction.t) list )
        Hashtbl.t;
      rematerialized : (InstructionId.t, basic Instruction.t) Hashtbl.t
    }

  let find_basic t instr =
    Hashtbl.find_opt t.instructions instr.id
    |> Option.map (fun info -> info.instr)

  let find_terminator t instr = Hashtbl.find_opt t.terminators instr.id

  let reg_fun_args t = t.reg_fun_args

  let find_deleted_before verification id =
    Option.value ~default:[] (Hashtbl.find_opt verification.deleted_before id)

  let find_rematerialized verification id =
    Hashtbl.find_opt verification.rematerialized id

  let is_regalloc_specific_basic (desc : Cfg.basic) =
    match desc with Op (Reload | Spill) -> true | _ -> false

  let add_instr_id ~seen_ids ~context id =
    if Hashtbl.mem seen_ids id
    then
      Regalloc_utils.fatal "Duplicate instruction no. %a while %s"
        InstructionId.format id context;
    Hashtbl.add seen_ids id ()

  let add_basic ~seen_ids ~successor_id ~label t instr =
    let id = instr.id in
    add_instr_id ~seen_ids
      ~context:"adding a basic instruction to the description" id;
    Hashtbl.add t.instruction_labels id label;
    if is_regalloc_specific_basic instr.desc
    then
      Regalloc_utils.fatal
        "Instruction no. %a is specific to the regalloc phase while creating \
         pre-allocation description"
        InstructionId.format id;
    Hashtbl.add t.instructions id
      { successor_id;
        instr =
          { Instruction.desc = instr.desc;
            arg = Array.map Register.create instr.arg;
            res = Array.map Register.create instr.res
          }
      }

  let add_terminator ~seen_ids ~label t instr =
    let id = instr.id in
    add_instr_id ~seen_ids
      ~context:"adding a terminator instruction to the description" id;
    Hashtbl.add t.instruction_labels id label;
    Hashtbl.add t.terminators id
      { Instruction.desc = instr.desc;
        arg = Array.map Register.create instr.arg;
        res = Array.map Register.create instr.res
      }

  let do_create cfg =
    Regalloc_invariants.precondition cfg;
    if Regalloc_utils.Param.get Regalloc_utils.validator_debug
    then
      (* CR-someday: We don't save the file with [fun_name] in the filename
         because there is an appended stamp that is fragile and is annoying when
         testing. Currently it's not a problem because we abort the build
         whenever register allocation fails but if there was a fallback mode
         then the interesting files would be instantly overwritten. *)
      Cfg_with_layout.save_as_dot ~annotate_instr:[Printcfg.instruction]
        ~filename:"before.dot" cfg "before_allocation_before_validation";
    let basic_count, terminator_count =
      Cfg_with_layout.fold_instructions cfg
        ~instruction:(fun (basic_count, terminator_count) _ ->
          basic_count + 1, terminator_count)
        ~terminator:(fun (basic_count, terminator_count) _ ->
          basic_count, terminator_count + 1)
        ~init:(0, 0)
    in
    let seen_ids = Hashtbl.create (basic_count + terminator_count) in
    let reg_fun_args =
      (Cfg_with_layout.cfg cfg).fun_args
      |> Array.map (fun reg ->
          let reg = Register.create reg in
          (* Assert that [fun_args] are preassigned. *)
          (match reg.reg_id with
          | Preassigned _ -> ()
          | Named _ ->
            Regalloc_utils.fatal
              "Register in function arguments that isn't preassigned: %a"
              Register.print reg);
          reg)
    in
    let t =
      { instructions = Hashtbl.create basic_count;
        terminators = Hashtbl.create terminator_count;
        first_instruction_in_block = Label.Tbl.create terminator_count;
        instruction_labels = Hashtbl.create (basic_count + terminator_count);
        reg_fun_args
      }
    in
    Label.Tbl.iter
      (fun _ (block : Cfg.basic_block) ->
        let label = block.start in
        add_terminator ~seen_ids ~label t block.terminator;
        let first_instruction_id =
          DLL.fold_right
            ~f:(fun instr successor_id ->
              add_basic ~seen_ids ~successor_id ~label t instr;
              instr.id)
            block.body ~init:block.terminator.id
        in
        Label.Tbl.add t.first_instruction_in_block block.start
          first_instruction_id)
      (Cfg_with_layout.cfg cfg).Cfg.blocks;
    t

  let create cfg =
    match !Oxcaml_flags.regalloc_validate with
    | false -> None
    | true -> Some (do_create cfg)

  let verify_reg_array ~context ~reg_arr ~loc_arr =
    if Array.length reg_arr <> Array.length loc_arr
    then
      Regalloc_utils.fatal
        "%s: register array length has changed. Before: %d. Now: %d." context
        (Array.length reg_arr) (Array.length loc_arr);
    Array.iter2
      (fun (reg_desc : Register.t) loc_reg ->
        match reg_desc.reg_id, Location.of_reg loc_reg with
        | _, None ->
          Regalloc_utils.fatal "%s: location is still unknown after allocation"
            context
        | Named { stamp = _ }, _ -> ()
        | Preassigned { location = l1 }, Some l2 when Location.equal l1 l2 -> ()
        | Preassigned { location = prev_loc }, Some new_loc ->
          Regalloc_utils.fatal
            "%s: changed preassigned register's location from %a to %a" context
            (Location.print (Register.typ reg_desc))
            prev_loc
            (Location.print loc_reg.Reg.typ)
            new_loc)
      reg_arr loc_arr;
    ()

  let verify_reg_arrays (type a) ~id (instr : a Cfg.instruction)
      (old_instr : a Instruction.t) =
    verify_reg_array
      ~context:
        (Printf.sprintf "In instruction's no %s arguments"
           (InstructionId.to_string id))
      ~reg_arr:old_instr.arg ~loc_arr:instr.arg;
    verify_reg_array
      ~context:
        (Printf.sprintf "In instruction's no %s results"
           (InstructionId.to_string id))
      ~reg_arr:old_instr.res ~loc_arr:instr.res

  (* Follow the description's successor chain starting at [old_successor_id],
     skipping ids whose corresponding description-instruction was validly
     deleted by [Cfg_deadcode] (i.e. not present in [seen_ids] and with a pure
     [desc]). The returned id is the first one that is either still present in
     the post-allocation CFG, or that is not in the description at all (e.g. the
     block's terminator). *)
  let rec effective_successor_id ~seen_ids t old_successor_id =
    if Hashtbl.mem seen_ids old_successor_id
    then old_successor_id
    else
      match Hashtbl.find_opt t.instructions old_successor_id with
      | Some info when Cfg.is_pure_basic info.instr.Instruction.desc ->
        effective_successor_id ~seen_ids t info.successor_id
      | _ -> old_successor_id

  (* A definition of a description pseudo-register: the instruction writing it,
     and the register as it appears among that instruction's results. *)
  type definition =
    { def_id : InstructionId.t;
      def_label : Label.t;
      def_instr : basic Instruction.t option; (* [None] for a terminator *)
      def_register : Register.t
    }

  (* The definitions of every pseudo-register of the description, by stamp. *)
  let compute_definitions t : (Reg.Stamp.t, definition list) Hashtbl.t =
    let definitions = Hashtbl.create (Hashtbl.length t.instructions) in
    let add ~id ~def_instr (res : Register.t array) =
      let def_label = Hashtbl.find t.instruction_labels id in
      Array.iter
        (fun (register : Register.t) ->
          match register.reg_id with
          | Preassigned _ -> ()
          | Named { stamp } ->
            Hashtbl.replace definitions stamp
              ({ def_id = id; def_label; def_instr; def_register = register }
              :: Option.value ~default:[] (Hashtbl.find_opt definitions stamp)))
        res
    in
    Hashtbl.iter
      (fun id (info : basic_info) ->
        add ~id ~def_instr:(Some info.instr) info.instr.res)
      t.instructions;
    Hashtbl.iter
      (fun id (terminator : terminator Instruction.t) ->
        add ~id ~def_instr:None terminator.res)
      t.terminators;
    definitions

  (* The inputs and the output of [verify_rematerialization], threaded through
     [verify]. The analyses are only computed when needed. Dominators and loop
     information are computed on the post-allocation CFG: the allocator only
     inserts blocks on edges, preserving dominance between original blocks and
     which original blocks are on cycles. *)
  type rematerialization_context =
    { rematerializations : Regalloc_rematerializations.t;
      definitions : (Reg.Stamp.t, definition list) Hashtbl.t Lazy.t;
      is_in_cycle : (Label.t -> bool) Lazy.t;
      dominators : Cfg_dominators.t Lazy.t;
      rematerialized : (InstructionId.t, basic Instruction.t) Hashtbl.t
    }

  (* Checks the rematerialization the allocator recorded for the added
     instruction [instr] (cf. [Regalloc_rematerializations]) against the
     description, and returns the description-level instruction the equation
     transfer is to use for [instr]: the recorded source instruction, with the
     pseudo-register whose stamp was recorded (call it [reg]) as its result.

     The transfer thereby treats [instr] as computing the value of [reg] from
     the values of the source's arguments where [instr] stands. For the
     description alone to justify this, we check that:

     - [instr] has the same [desc] as the source, which is a description
     instruction with exactly one result;

     - the source's result and arguments, and every register on the chain from
     [reg] back to the source's result, are pseudo-registers with exactly one
     definition, in a block that is not part of any cycle: such a register is
     assigned at most once per execution of the function, so it keeps the same
     value once defined, and the source's result is then equal to the source
     applied to its arguments wherever both are defined;

     - [reg] is the source's result, or is reached from it through moves between
     registers of the same machtype (each move defining the next register of the
     chain).

     The equations check the source's arguments at [instr], but do not prove
     that the source has already executed: the copy itself discharges the result
     equation. For loads we separately require the source's original block to
     strictly dominate the copy's block. Split inserts copies at block entry, so
     their sources must be in earlier blocks. Using the description's source
     block also covers sources subsequently deleted as dead code. Since basic
     instructions cannot raise, the source executed before every exit from that
     block in the original CFG, making the copy non-speculative.
     [Regalloc_split_utils.is_rematerializable_shape] then ensures the same
     arguments produce the same value (immutable non-atomic loads, constants,
     pure arithmetic). *)
  let verify_rematerialization ctx t ~label (instr : basic Cfg.instruction) :
      basic Instruction.t =
    let id = instr.id in
    let fail fmt =
      Format.kasprintf
        (fun msg ->
          Regalloc_utils.fatal "Rematerialized instruction no. %a: %s"
            InstructionId.format id msg)
        fmt
    in
    let { Regalloc_rematerializations.source; stamp = rematerialized } =
      match Regalloc_rematerializations.find ctx.rematerializations id with
      | Some rematerialization -> rematerialization
      | None -> fail "no recorded rematerialization"
    in
    let source_instr =
      match Hashtbl.find_opt t.instructions source with
      | Some (info : basic_info) -> info.instr
      | None ->
        fail "source instruction no. %a is not in the description"
          InstructionId.format source
    in
    if not (Cfg.equal_basic instr.desc source_instr.Instruction.desc)
    then
      fail "%a differs from its source instruction no. %a (%a)"
        Printcfg.basic_desc instr.desc InstructionId.format source
        Printcfg.basic_desc source_instr.Instruction.desc;
    (* The single definition of the pseudo-register [stamp], which must be in a
       block outside any cycle. *)
    let single_definition ~what (stamp : Reg.Stamp.t) : definition =
      match Hashtbl.find_opt (Lazy.force ctx.definitions) stamp with
      | None | Some [] ->
        fail "%s with stamp %a has no definition" what Reg.Stamp.format stamp
      | Some (definition :: _ :: _ as definitions) ->
        fail "%s %a has %d definitions" what Register.print
          definition.def_register (List.length definitions)
      | Some [definition] ->
        if Lazy.force ctx.is_in_cycle definition.def_label
        then
          fail
            "%s %a is defined by instruction no. %a in block %a, which is part \
             of a cycle"
            what Register.print definition.def_register InstructionId.format
            definition.def_id Label.format definition.def_label;
        definition
    in
    let stamp_of ~what (register : Register.t) : Reg.Stamp.t =
      match register.reg_id with
      | Preassigned _ ->
        fail "%s %a is preassigned" what Register.print register
      | Named { stamp } -> stamp
    in
    let source_result =
      match source_instr.Instruction.res with
      | [| source_result |] -> source_result
      | res ->
        fail "source instruction no. %a has %d results, expected one"
          InstructionId.format source (Array.length res)
    in
    (let what = "source result" in
     ignore
       (single_definition ~what (stamp_of ~what source_result) : definition));
    Array.iter
      (fun arg ->
        let what = "source argument" in
        ignore (single_definition ~what (stamp_of ~what arg) : definition))
      source_instr.Instruction.arg;
    (* Walk the chain of moves from [stamp] back to the source's result,
       returning the description register of [stamp]. *)
    let rec check_chain ~visited (stamp : Reg.Stamp.t) : Register.t =
      if List.exists (Reg.Stamp.equal stamp) visited
      then
        fail "register with stamp %a is defined by a cycle of moves"
          Reg.Stamp.format stamp;
      let definition = single_definition ~what:"register" stamp in
      (if not (InstructionId.equal definition.def_id source)
       then
         match definition.def_instr with
         | Some
             { Instruction.desc = Op Move;
               arg = [| move_arg |];
               res = [| move_res |]
             }
           when Cmm.equal_machtype_component (Register.typ move_arg)
                  (Register.typ move_res) ->
           let (_ : Register.t) =
             check_chain ~visited:(stamp :: visited)
               (stamp_of ~what:"register" move_arg)
           in
           ()
         | Some _ | None ->
           fail
             "register %a is defined by instruction no. %a, which is neither \
              the source instruction no. %a nor a move"
             Register.print definition.def_register InstructionId.format
             definition.def_id InstructionId.format source);
      definition.def_register
    in
    let res = check_chain ~visited:[] rematerialized in
    (match instr.desc with
    | Op (Load _) ->
      let source_label = Hashtbl.find t.instruction_labels source in
      if
        not
          (Cfg_dominators.is_strictly_dominating
             (Lazy.force ctx.dominators)
             source_label label)
      then
        fail
          "source load no. %a in block %a does not strictly dominate block %a"
          InstructionId.format source Label.format source_label Label.format
          label
    | _ -> ());
    let reg_instr : basic Instruction.t =
      { Instruction.desc = source_instr.Instruction.desc;
        arg = source_instr.Instruction.arg;
        res = [| res |]
      }
    in
    verify_reg_arrays ~id instr reg_instr;
    reg_instr

  let verify_basic ~seen_ids ~successor_id ~remat_ctx ~label t instr =
    let id = instr.id in
    add_instr_id ~seen_ids
      ~context:"checking a basic instruction in the new CFG" id;
    match
      Hashtbl.find_opt t.instructions id, is_regalloc_specific_basic instr.desc
    with
    (* The instruction was present before. *)
    | Some { instr = old_instr; successor_id = old_successor_id }, false ->
      let effective_old_successor_id =
        effective_successor_id ~seen_ids t old_successor_id
      in
      if not (InstructionId.equal effective_old_successor_id successor_id)
      then
        Regalloc_utils.fatal
          "The instruction's no. %a successor id has changed. Before \
           allocation: %a. After allocation (ignoring instructions added by \
           allocation): %a."
          InstructionId.format id InstructionId.format
          effective_old_successor_id InstructionId.format successor_id;
      (match instr.desc, old_instr.desc with
      | Op (Name_for_debugger _), Op (Name_for_debugger _) ->
        (* IRC uses `Reg.interf` to represent the adjacency lists for the
           interference graph, which can lead to cycles. *)
        ()
      | _ ->
        if not (Cfg.equal_basic instr.desc old_instr.desc)
        then
          Regalloc_utils.fatal "The desc of instruction with id %a changed"
            InstructionId.format id);
      verify_reg_arrays ~id instr old_instr;
      (* Return new successor id which is the id of the current instruction. *)
      id
    | None, true ->
      (* Added regalloc specific instruction that wasn't before. The new
         successor is the same as old one because this instruction is
         ignored. *)
      successor_id
    | Some _, true ->
      Regalloc_utils.fatal
        "Register allocation changed existing instruction no. %a into a \
         register allocation specific instruction"
        InstructionId.format id
    | None, false -> (
      match instr.desc with
      | Op Move ->
        (* A move instruction, while no regalloc-specific, can be inserted
           because of phi moves in split/rename. *)
        successor_id
      | _ when is_rematerialized_shape instr ->
        (* The split pass may rematerialize a register by emitting a copy of a
           pure source instruction (immutable load, constant, safe [Intop_imm],
           ...; cf. [Regalloc_split.RewriteAsRematerialize]). The shape check
           above is the same one used at classification time, so only
           instructions the rematerialization analysis would itself emit can
           pass it; the rematerialization the allocator recorded for the
           instruction is then checked against the description. Like [Op Move]
           additions, this instruction does not advance the pre-allocation
           successor chain, so we keep [successor_id] as-is. *)
        Hashtbl.replace remat_ctx.rematerialized id
          (verify_rematerialization remat_ctx t ~label instr);
        successor_id
      | _ ->
        Regalloc_utils.fatal
          "Register allocation added non-regalloc specific instruction no. %a"
          InstructionId.format id)

  let compare_terminators ~successor_ids ~id (old_instr : terminator)
      (instr : terminator) =
    let compare_label l1 l2 =
      let s1 = Label.Tbl.find successor_ids l1 in
      let s2 = Label.Tbl.find successor_ids l2 in
      if not (InstructionId.equal s1 s2)
      then
        Regalloc_utils.fatal
          "When checking equivalence of labels before and after allocation got \
           different successor id's. Successor (label, instr id) before: (%a, \
           %a). Successor (label, instr id) after: (%a, %a)."
          Label.format l1 InstructionId.format s1 Label.format l2
          InstructionId.format s2
    in
    match old_instr, instr with
    | Never, Never -> ()
    | Always l1, Always l2 -> compare_label l1 l2
    | ( Parity_test { ifso = ifso1; ifnot = ifnot1 },
        Parity_test { ifso = ifso2; ifnot = ifnot2 } ) ->
      compare_label ifso1 ifso2;
      compare_label ifnot1 ifnot2
    | ( Truth_test { ifso = ifso1; ifnot = ifnot1 },
        Truth_test { ifso = ifso2; ifnot = ifnot2 } ) ->
      compare_label ifso1 ifso2;
      compare_label ifnot1 ifnot2
    | ( Float_test { width = w1; lt = lt1; eq = eq1; gt = gt1; uo = uo1 },
        Float_test { width = w2; lt = lt2; eq = eq2; gt = gt2; uo = uo2 } )
      when Cmm.equal_float_width w1 w2 ->
      compare_label lt1 lt2;
      compare_label eq1 eq2;
      compare_label gt1 gt2;
      compare_label uo1 uo2
    | ( Int_test { lt = lt1; eq = eq1; gt = gt1; is_signed = sign1; imm = imm1 },
        Int_test { lt = lt2; eq = eq2; gt = gt2; is_signed = sign2; imm = imm2 }
      )
      when Scalar.Signedness.equal sign1 sign2
           && Option.equal Int.equal imm1 imm2 ->
      compare_label lt1 lt2;
      compare_label eq1 eq2;
      compare_label gt1 gt2
    | Switch labels1, Switch labels2 ->
      Array.iter2 (fun l1 l2 -> compare_label l1 l2) labels1 labels2
    | Return, Return -> ()
    | Raise rk1, Raise rk2 when Lambda.equal_raise_kind rk1 rk2 -> ()
    | Tailcall_self { destination = l1 }, Tailcall_self { destination = l2 } ->
      compare_label l1 l2
    | Tailcall_func call1, Tailcall_func call2
      when Cfg.equal_func_call_operation call1 call2 ->
      ()
    | Call_no_return call1, Call_no_return call2
      when Cfg.equal_external_call_operation call1 call2 ->
      ()
    | ( Call { op = call1; label_after = l1 },
        Call { op = call2; label_after = l2 } )
      when Cfg.equal_func_call_operation call1 call2 ->
      compare_label l1 l2
    | ( Prim { op = prim1; label_after = l1 },
        Prim { op = prim2; label_after = l2 } )
      when Cfg.equal_prim_call_operation prim1 prim2 ->
      compare_label l1 l2
    | ( Invalid { message = m1; label_after = None; _ },
        Invalid { message = m2; label_after = None; _ } )
      when String.compare m1 m2 = 0 ->
      ()
    | ( Invalid { message = m1; label_after = Some l1; _ },
        Invalid { message = m2; label_after = Some l2; _ } )
      when String.compare m1 m2 = 0 ->
      compare_label l1 l2
    | _ ->
      Regalloc_utils.fatal
        "The desc of terminator with id %a changed, before: %a, after: %a."
        InstructionId.format id
        (Printcfg.terminator_desc ~sep:", ")
        old_instr
        (Printcfg.terminator_desc ~sep:", ")
        instr

  let verify_terminator ~seen_ids ~(successor_ids : InstructionId.t Label.Tbl.t)
      t instr : InstructionId.t =
    let id = instr.id in
    add_instr_id ~seen_ids
      ~context:"checking a terminator instruction in the new CFG" id;
    match Hashtbl.find_opt t.terminators id with
    (* The instruction was present before. *)
    | Some old_instr ->
      verify_reg_arrays ~id instr old_instr;
      compare_terminators ~successor_ids ~id old_instr.desc instr.desc;
      id
    | None -> (
      match instr.desc with
      | Always successor ->
        (* A terminator added by the register allocator. The successor
           instruction can be found in the next block. *)
        Label.Tbl.find successor_ids successor
      | _ ->
        Regalloc_utils.fatal
          "Register allocation added a terminator no. %a but that's not \
           allowed for this type of terminator: %a"
          InstructionId.format id Printcfg.terminator instr)

  let compute_successor_ids t (cfg : Cfg.t) =
    let visited_labels = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
    let successor_ids = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
    (* Finds and stores successor id for a given block. *)
    let rec get_id (block : Cfg.basic_block) =
      match Label.Tbl.find_opt successor_ids block.start with
      | Some id -> id
      | None ->
        if Label.Tbl.mem visited_labels block.start
        then
          Misc.fatal_errorf
            "Visiting the same block %a without knowing the successor \
             instruction's id. That means there's a loop consisting of only \
             instructions added by the register allocator."
            Label.format block.start;
        Label.Tbl.add visited_labels block.start ();
        let first_id = get_first_non_regalloc_id t block in
        Label.Tbl.add successor_ids block.start first_id;
        first_id
    (* Finds successor id in or after the given block. *)
    and get_first_non_regalloc_id t (block : Cfg.basic_block) =
      let res : Cfg.basic Cfg.instruction option =
        DLL.fold_left
          ~f:(fun acc instr ->
            match acc with
            | Some _ -> acc
            | None ->
              if Hashtbl.mem t.instructions instr.id then Some instr else None)
          block.body ~init:None
      in
      match res with
      | Some instr -> instr.id
      | None -> (
        match
          block.terminator.desc, Hashtbl.mem t.terminators block.terminator.id
        with
        | _, true -> block.terminator.id
        | Always label, false -> get_id (Cfg.get_block_exn cfg label)
        | _, false ->
          Regalloc_utils.fatal
            "Register allocation added a terminator no. %a but that's not \
             allowed for this type of terminator: %a"
            InstructionId.format block.terminator.id Printcfg.terminator
            block.terminator)
    in
    Label.Tbl.iter
      (fun _ block ->
        (* Force compuatation of the given id. *)
        let (_ : InstructionId.t) = get_id block in
        ())
      cfg.blocks;
    successor_ids

  let verify t ~rematerializations cfg =
    Regalloc_invariants.postcondition_layout cfg;
    verify_reg_array ~reg_arr:t.reg_fun_args ~context:"In function arguments"
      ~loc_arr:(Cfg_with_layout.cfg cfg).fun_args;
    let seen_ids =
      Hashtbl.create
        (Hashtbl.length t.instructions + Hashtbl.length t.terminators)
    in
    let successor_ids = compute_successor_ids t (Cfg_with_layout.cfg cfg) in
    let remat_ctx =
      { rematerializations;
        definitions = lazy (compute_definitions t);
        is_in_cycle =
          lazy
            (let loop_infos = Cfg_loop_infos.build (Cfg_with_layout.cfg cfg) in
             Cfg_loop_infos.is_in_loop loop_infos);
        dominators = lazy (Cfg_dominators.build (Cfg_with_layout.cfg cfg));
        rematerialized = Hashtbl.create 8
      }
    in
    Label.Tbl.iter
      (fun _ (block : Cfg.basic_block) ->
        let successor_id =
          verify_terminator ~seen_ids ~successor_ids t block.terminator
        in
        let first_instruction_id =
          DLL.fold_right
            ~f:(fun instr successor_id ->
              verify_basic ~seen_ids ~successor_id ~remat_ctx ~label:block.start
                t instr)
            block.body ~init:successor_id
        in
        ignore (first_instruction_id : InstructionId.t))
      (Cfg_with_layout.cfg cfg).Cfg.blocks;
    let deleted_before = Hashtbl.create 8 in
    Hashtbl.iter
      (fun id (info : basic_info) ->
        if not (Hashtbl.mem seen_ids id)
        then (
          (* [Cfg_deadcode] runs at the end of the split pass and removes any
             pure basic instruction whose results are dead (e.g. because a
             rematerialization rerouted all of its uses, possibly cascading to
             upstream pure instructions whose results were only consumed by the
             deleted ones). Its rule has two halves and we check both: purity
             here, mirroring the predicate [Cfg_deadcode.remove_deadcode] uses,
             and deadness in the equation transfer, at the deleted instruction's
             effective successor (cf. [Transfer.check_deleted_before]). Deadness
             cannot be taken for granted: the equations alone do not notice the
             deletion of a live instruction whose result register is also one of
             its arguments at the same location. *)
          if not (Cfg.is_pure_basic info.instr.Instruction.desc)
          then
            Regalloc_utils.fatal
              "Instruction no. %a was deleted by register allocator"
              InstructionId.format id;
          let successor_id =
            effective_successor_id ~seen_ids t info.successor_id
          in
          Hashtbl.replace deleted_before successor_id
            ((id, info.instr)
            :: Option.value ~default:[]
                 (Hashtbl.find_opt deleted_before successor_id))))
      t.instructions;
    Hashtbl.iter
      (fun id _ ->
        if not (Hashtbl.mem seen_ids id)
        then
          Regalloc_utils.fatal
            "Terminator no. %a was deleted by register allocator"
            InstructionId.format id)
      t.terminators;
    { deleted_before; rematerialized = remat_ctx.rematerialized }
end

module Equation_set : sig
  (** This corresponds to the set of equations defined in section 3.2 of in the
      paper [1]. The definition is simplified substantially because here none of
      the locations (e.g. registers or stack) are allowed to overlap. *)
  type t

  val empty : t

  val union : t -> t -> t

  val subset : t -> t -> bool

  (** This corresponds to case (10) in Fig. 1 of the paper [1]. *)
  val rename_loc : arg:Location.t -> res:Location.t -> t -> t

  (** This corresponds to case (3) in Fig. 1 of the paper [1]. *)
  val rename_reg : arg:Register.t -> res:Register.t -> t -> t

  (** Calling [remove_result], [verify_destoyed_locations] and [add_argument] in
      this order corresponds to case (7) in Fig. 1 of the paper [1]. This
      implementation is also generalized for all cases not handled by
      [rename_loc] or [rename_reg].

      The compatibility check has two halves (see [compatible_one] below): the
      "two registers at one location" half is unconditional (it would always
      represent a regalloc aliasing bug); the "one register at two locations"
      half is skipped for the registers in [relaxed]. The only caller passing a
      non-empty [relaxed] is the transfer of a rematerialized copy inserted by
      the split pass, for the copy's own result: the copy recomputes a value
      that is also legitimately expected, further down, at the stack slot
      written by the spill of the original computation. *)
  val remove_result :
    relaxed:Register.Set.t ->
    reg_res:Register.t array ->
    loc_res:Location.t array ->
    t ->
    (t, string) Result.t

  val verify_destroyed_locations :
    destroyed:Location.t array -> t -> (t, string) Result.t

  val add_argument :
    reg_arg:Register.t array -> loc_arg:Location.t array -> t -> t

  val is_empty : t -> bool

  (** [has_register t reg] is [true] iff some equation in [t] has [reg] on its
      left-hand side. *)
  val has_register : t -> Register.t -> bool

  val print : Format.formatter -> t -> unit
end = struct
  module Equation = struct
    (** The equations here contain a bit more than the paper [1] has. In the
        said paper the equation is a named variable on one side and location on
        the other side because moving arguments to the correct position before
        call is done implicitly.

        In our representation of CFG before the call [foo %rax] there is
        explicit instruction [%rax := x]. Therefore the left-hand side of the
        equation in our case is either [Named] or [Preassigned]. The equations
        for [Preassigned { location }] are always expected to have exactly
        [location] on the right-hand side.

        It's possible to have very specific code with only no-op moves, spills
        and reloads that breaks the previous assumption but there are multiple
        soft assumptions that prevent existence of such code. Result of
        validation on such code is subject to change and shouldn't be relied
        upon.

        For that reason equations of form [Preassigned { location } = location]
        give us implicitly a set of live preassigned locations. That verifies
        that none of the preassigned locations are destroyed or assigning to the
        preassigned location doesn't destroy a [Named] variable. *)
    type t = Register.t * Location.t

    let print ppf (r, l) =
      Format.fprintf ppf "%a=%a" Register.print r
        (Location.print (Register.typ r))
        l
  end

  exception Verification_failed of string

  type t =
    { for_loc : Register.Set.t Location.Map.t;
      for_reg : Location.Set.t Register.Map.t
    }

  let empty = { for_loc = Location.Map.empty; for_reg = Register.Map.empty }

  let remove ((eq_reg, eq_loc) : Equation.t) t =
    { for_loc =
        Location.Map.update eq_loc
          (function
            | None -> None
            | Some set ->
              let set = Register.Set.remove eq_reg set in
              if Register.Set.is_empty set then None else Some set)
          t.for_loc;
      for_reg =
        Register.Map.update eq_reg
          (function
            | None -> None
            | Some set ->
              let set = Location.Set.remove eq_loc set in
              if Location.Set.is_empty set then None else Some set)
          t.for_reg
    }

  let add ((eq_reg, eq_loc) : Equation.t) t =
    { for_loc =
        Location.Map.update eq_loc
          (fun set ->
            match set with
            | None -> Some (Register.Set.singleton eq_reg)
            | Some set -> Some (Register.Set.add eq_reg set))
          t.for_loc;
      for_reg =
        Register.Map.update eq_reg
          (function
            | None -> Some (Location.Set.singleton eq_loc)
            | Some set -> Some (Location.Set.add eq_loc set))
          t.for_reg
    }

  let has_register t reg = Register.Map.mem reg t.for_reg

  let is_empty t =
    let loc_res = Location.Map.is_empty t.for_loc in
    let reg_res = Register.Map.is_empty t.for_reg in
    assert (Bool.equal loc_res reg_res);
    loc_res

  let subset t1 t2 =
    Location.Map.for_all
      (fun loc regs1 ->
        assert (not (Register.Set.is_empty regs1));
        match Location.Map.find_opt loc t2.for_loc with
        | None -> false
        | Some regs2 -> Register.Set.subset regs1 regs2)
      t1.for_loc

  let union t1 t2 =
    (* [Map.union] shares the subtrees that occur in only one operand, so this
       costs O(overlap) rather than O(size); the guard keeps the join with an
       empty set allocation-free. *)
    if is_empty t1
    then t2
    else if is_empty t2
    then t1
    else
      { for_loc =
          Location.Map.union
            (fun _loc regs1 regs2 -> Some (Register.Set.union regs1 regs2))
            t1.for_loc t2.for_loc;
        for_reg =
          Register.Map.union
            (fun _reg locs1 locs2 -> Some (Location.Set.union locs1 locs2))
            t1.for_reg t2.for_reg
      }

  let array_fold2 f acc arr1 arr2 =
    let acc = ref acc in
    Array.iter2 (fun v1 v2 -> acc := f !acc v1 v2) arr1 arr2;
    !acc

  let compatible_one ~relaxed ~reg ~loc t =
    let check_equation eq_reg eq_loc =
      (* This check corresponds to simplified check that "(x, l) is compatible
         with E" from chapter 3.2 section "Unsatisfiability and Overlap" from
         the paper [1], where "x" is [reg] and "l" is [loc]. Because we don't
         consider overlap at all, the condition simplifies to [(x' = x && l' =
         l) || (x' <> x && l' <> l)]. *)
      let reg_eq = Register.equal eq_reg reg in
      let loc_eq = Location.equal eq_loc loc in
      if not (Bool.equal reg_eq loc_eq)
      then (
        Format.fprintf Format.str_formatter
          "Unsatisfiable equations when removing result equations.\n\
           Existing equation has to agree on 0 or 2 sides (cannot be exactly \
           1) with the removed equation.\n\
           Existing equation %a.\n\
           Removed equation: %a."
          Equation.print (eq_reg, eq_loc) Equation.print (reg, loc);
        let message = Format.flush_str_formatter () in
        raise (Verification_failed message))
    in
    (* "Same-location" half: a second register sharing [loc] would mean two
       distinct values live at the same physical location at the same program
       point — always a regalloc bug (aliasing). This half is unconditional and
       is the load-bearing soundness check even under rematerialization. *)
    (match Location.Map.find_opt loc t.for_loc with
    | None -> ()
    | Some regs ->
      let eq_loc = loc in
      Register.Set.iter (fun eq_reg -> check_equation eq_reg eq_loc) regs);
    (* "Same-register" half: under the original Rideau/Leroy semantics each
       description-stamp lives at exactly one location at any program point,
       which makes this check the symmetric companion of the same-location half.
       A rematerialized copy inserted by the split pass breaks that assumption
       for its own result stamp [x]: the copy recomputes the (single-assignment)
       value of [x] into a register, while a later block may still reload [x]
       from the stack slot written by the spill of the original computation,
       upstream of the copy. Walking backward, at the copy the set then holds
       both [x = register] and [x = slot], and both are satisfied. The slot
       equation must pass through the copy untouched, to be discharged upstream
       at the spill (by [rename_location]) and at the original definition, where
       this half is enforced again. Hence the half is skipped exactly for the
       stamps in [relaxed], which the transfer sets to the copy's result and to
       nothing anywhere else. *)
    (if not (Register.Set.mem reg relaxed)
     then
       match Register.Map.find_opt reg t.for_reg with
       | None -> ()
       | Some locs ->
         let eq_reg = reg in
         Location.Set.iter (fun eq_loc -> check_equation eq_reg eq_loc) locs);
    ()

  let remove_result ~relaxed ~reg_res ~loc_res t =
    try
      Array.iter2
        (fun reg loc -> compatible_one ~relaxed ~reg ~loc t)
        reg_res loc_res;
      let t =
        array_fold2 (fun t reg loc -> remove (reg, loc) t) t reg_res loc_res
      in
      Ok t
    with Verification_failed message -> Error message

  let verify_destroyed_locations ~destroyed t =
    (* CR azewierzejew for azewierzejew: Add checking stack for stack_location
       other than Local. *)
    try
      Array.iter
        (fun destroyed_loc ->
          match Location.Map.find_opt destroyed_loc t.for_loc with
          | None -> ()
          | Some regs ->
            assert (not (Register.Set.is_empty regs));
            let typ = Register.Set.choose regs |> Register.typ in
            Format.fprintf Format.str_formatter
              "Destroying a location %a in which live registers %a are stored"
              (Location.print typ) destroyed_loc
              (Format.pp_print_seq
                 ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
                 Register.print)
              (Register.Set.to_seq regs);
            let message = Format.flush_str_formatter () in
            raise (Verification_failed message))
        destroyed;
      Ok t
    with Verification_failed message -> Error message

  let add_argument ~reg_arg ~loc_arg t =
    array_fold2 (fun t reg loc -> add (reg, loc) t) t reg_arg loc_arg

  let rename_loc ~arg ~res t =
    let regs =
      Location.Map.find_opt res t.for_loc
      |> Option.value ~default:Register.Set.empty
    in
    Register.Set.fold
      (fun reg acc -> acc |> remove (reg, res) |> add (reg, arg))
      regs t

  let rename_reg ~arg ~res t =
    let locs =
      Register.Map.find_opt res t.for_reg
      |> Option.value ~default:Location.Set.empty
    in
    Location.Set.fold
      (fun loc acc -> acc |> remove (res, loc) |> add (arg, loc))
      locs t

  let print ppf t =
    let first = ref true in
    let print_eq eq_reg eq_loc =
      if !first then first := false else Format.fprintf ppf " ";
      Format.fprintf ppf "%a" Equation.print (eq_reg, eq_loc)
    in
    Location.Map.iter
      (fun eq_loc regs ->
        Register.Set.iter (fun eq_reg -> print_eq eq_reg eq_loc) regs)
      t.for_loc
end

(* The inputs of the equation transfer: the pre-allocation description, and what
   [Description.verify] established about the instructions the allocator deleted
   or added. *)
module type Transfer_inputs = sig
  val description : Description.t

  val verification : Description.verification
end

let print_reg_as_loc ppf reg =
  Printreg.loc
    ~unknown:(fun ppf -> Format.fprintf ppf "<Unknown>")
    ppf reg.Reg.loc reg.Reg.typ

module Domain : Cfg_dataflow.Domain_S with type t = Equation_set.t = struct
  (** This type corresponds to the set of equations in the dataflow from the
      paper [1]. The value corresponing to "Top" is not represented. The
      transfer function return [(domain, error) result] and the "Top" element
      corresponds to [Error _] for which dataflow is terminated immediately
      without propagating the error value all the way to the entrypoint. A
      side-effect of this is that an erroreous but unreachable code will make
      the validation fail (where the algorithm in [1] will allow such code). *)

  type t = Equation_set.t

  let bot = Equation_set.empty

  let join t1 t2 = Equation_set.union t1 t2

  let less_equal t1 t2 = Equation_set.subset t1 t2
end

module Transfer_error : sig
  type t

  val create :
    Equation_set.t ->
    instr_kind:'a Instruction.Kind.t ->
    exn:Equation_set.t option ->
    reg_instr:'a Instruction.t ->
    loc_instr:'a instruction ->
    string ->
    t

  val print : Format.formatter -> t -> unit
end = struct
  type 'a unpacked =
    { instr_kind : 'a Instruction.Kind.t;
      equations : Equation_set.t;
      exn_equations : Equation_set.t option;
      reg_instr : 'a Instruction.t;
      loc_instr : 'a instruction;
      message : string
    }

  type t = T : _ unpacked -> t

  let create (type a) equations ~(instr_kind : a Instruction.Kind.t) ~exn
      ~(reg_instr : a Instruction.t) ~(loc_instr : a instruction) message =
    T
      { instr_kind;
        message;
        equations;
        reg_instr;
        loc_instr;
        exn_equations = exn
      }

  let print_unpacked (type a) ppf (t : a unpacked) =
    let reg_instr, loc_instr =
      match t.instr_kind with
      | Basic ->
        let t : basic unpacked = t in
        ( `Basic (Instruction.to_prealloc ~alloced:t.loc_instr t.reg_instr),
          `Basic t.loc_instr )
      | Terminator ->
        let t : terminator unpacked = t in
        ( `Terminator (Instruction.to_prealloc ~alloced:t.loc_instr t.reg_instr),
          `Terminator t.loc_instr )
    in
    Format.fprintf ppf "CFG REGALLOC Check failed in instr %a:\n"
      InstructionId.format t.loc_instr.id;
    Format.fprintf ppf "Instruction's description before allocation: %a\n"
      Printcfg.instruction reg_instr;
    Format.fprintf ppf "Instruction's description after allocation: %a\n"
      (Printcfg.instruction_with_print_reg ~print_reg:print_reg_as_loc)
      loc_instr;
    Format.fprintf ppf "Message: %s\n" t.message;
    Format.fprintf ppf "Live equations for the normal successor: [%a]\n"
      Equation_set.print t.equations;
    Option.iter
      (fun exn_equations ->
        Format.fprintf ppf
          "Live equations for the exceptional successor: [%a]\n"
          Equation_set.print exn_equations)
      t.exn_equations;
    ()

  let print ppf (T t) = print_unpacked ppf t
end

module Transfer (Inputs : Transfer_inputs) :
  Cfg_dataflow.Backward_transfer
    with type domain = Domain.t
     and type error = Transfer_error.t
     and type context = unit = struct
  type domain = Domain.t

  type error = Transfer_error.t

  type context = unit

  let description = Inputs.description

  let verification = Inputs.verification

  (** This corresponds to case (10) in Fig. 1 of the paper [1]. *)
  let rename_location equations ~loc_instr =
    assert (Array.length loc_instr.arg = 1);
    assert (Array.length loc_instr.res = 1);
    Equation_set.rename_loc
      ~arg:(Location.of_reg_exn loc_instr.arg.(0))
      ~res:(Location.of_reg_exn loc_instr.res.(0))
      equations

  (** This corresponds to case (3) in Fig. 1 of the paper [1]. *)
  let rename_register equations ~(reg_instr : _ Instruction.t) =
    assert (Array.length reg_instr.arg = 1);
    assert (Array.length reg_instr.res = 1);
    Equation_set.rename_reg ~arg:reg_instr.arg.(0) ~res:reg_instr.res.(0)
      equations

  (** For equations coming from exceptional path remove the expected equations.
  *)
  let remove_exn_bucket equations =
    let phys_reg = Proc.loc_exn_bucket in
    let reg = Register.create phys_reg in
    let loc =
      match reg.reg_id with
      | Preassigned { location } -> location
      | Named _ ->
        Misc.fatal_error
          "Regalloc_validate.remove_exn_bucket: exception bucket register must \
           be preassigned"
    in
    Equation_set.remove_result ~relaxed:Register.Set.empty equations
      ~reg_res:[| reg |] ~loc_res:[| loc |]
    |> Result.map_error (fun message ->
        Printf.sprintf "While removing exn bucket: %s" message)

  (** This corresponds to case (7) in Fig. 1 of the paper [1] generalized for
      all other cases not handled by [rename_location] or [rename_register]. We
      have an additional parameter which is the equations for the exceptional
      path successor which is not present in the paper [1] because exceptions
      are not considered there. *)
  let append_equations (type a) equations ~(instr_kind : a Instruction.Kind.t)
      ~exn ~relaxed ~(reg_instr : a Instruction.t) ~(loc_instr : a instruction)
      ~destroyed =
    let bind f res = Result.bind res f in
    let wrap_error res =
      Result.map_error
        (fun message ->
          Transfer_error.create equations ~instr_kind ~exn ~reg_instr ~loc_instr
            message)
        res
    in
    let exn =
      exn
      |> Option.map (fun exn ->
          (* Handle the exceptional path specific conversions here because in
             [exception_] we don't have enough information in order to give a
             meaningful error message. *)
          exn
          (* Remove the equality for [exn_bucket] if it exists. *)
          |> remove_exn_bucket
          |> wrap_error
          |> bind (fun equations ->
              (* Verify the destroyed registers for exceptional path only. *)
              equations
              |> Equation_set.verify_destroyed_locations
                   ~destroyed:
                     (Location.of_regs_exn (Proc.destroyed_at_raise ()))
              |> Result.map_error (fun message ->
                  Printf.sprintf
                    "While verifying locations destroyed at raise: %s" message)
              |> wrap_error))
      (* If instruction can't raise [Option.is_none exn] then use empty set of
         equations as that's the same as skipping the step. *)
      |> Option.value ~default:(Ok Domain.bot)
    in
    equations
    |>
    (* First remove the result equations. *)
    Equation_set.remove_result ~relaxed ~reg_res:reg_instr.Instruction.res
      ~loc_res:(Location.of_regs_exn loc_instr.res)
    |> wrap_error
    |> bind (fun equations ->
        (* Join the exceptional path equations. *)
        exn
        |> Result.map (fun exn_equations ->
            Equation_set.union equations exn_equations))
    |> bind (fun equations ->
        (* Verify the destroyed registers (including the exceptional path). *)
        Equation_set.verify_destroyed_locations ~destroyed equations
        |> wrap_error)
    |> Result.map (fun equations ->
        (* Add all eqations for the arguments. *)
        Equation_set.add_argument ~reg_arg:reg_instr.Instruction.arg
          ~loc_arg:(Location.of_regs_exn loc_instr.arg)
          equations)

  (* Second half of the [Cfg_deadcode] deletion rule (the first half, purity, is
     checked by [Description.verify], which also attaches every deleted
     instruction to its effective successor): the deleted instruction's results
     must be dead where it stood. [equations] are the equations before
     [loc_instr], the effective successor; the deleted instruction stood between
     its surviving predecessor and [loc_instr], and the instructions the
     allocator may have inserted in between (spills, reloads, moves) only rename
     locations, so the set of registers with an equation is the same at both
     points. An equation on one of the deleted results means a surviving
     instruction still expects the value the deleted instruction computed. *)
  let check_deleted_before (type a) ~(instr_kind : a Instruction.Kind.t)
      ~(reg_instr : a Instruction.t) ~(loc_instr : a instruction) equations :
      (domain, error) result =
    let live_result =
      List.find_map
        (fun ((deleted_id, deleted) : InstructionId.t * basic Instruction.t) ->
          Array.find_map
            (fun (reg : Register.t) ->
              if Equation_set.has_register equations reg
              then Some (deleted_id, deleted, reg)
              else None)
            deleted.Instruction.res)
        (Description.find_deleted_before verification loc_instr.id)
    in
    match live_result with
    | None -> Ok equations
    | Some (deleted_id, deleted, reg) ->
      let message =
        Format.asprintf
          "Instruction no. %a (%a) was deleted by register allocator but its \
           result %a is live"
          InstructionId.format deleted_id Printcfg.basic_desc
          deleted.Instruction.desc Register.print reg
      in
      Error
        (Transfer_error.create equations ~instr_kind ~exn:None ~reg_instr
           ~loc_instr message)

  let basic t instr () : (domain, error) result =
    match Description.find_basic description instr with
    | None -> (
      match instr.desc with
      | Op (Spill | Reload | Move) ->
        Result.ok @@ rename_location t ~loc_instr:instr
      | _ when is_rematerialized_shape instr ->
        (* Rematerialized pure source added by the split pass (immutable load,
           constant, safe [Intop_imm], ...). Unlike [Op (Spill | Reload | Move)]
           (which transport an existing value between locations and are handled
           by [rename_location]), this instruction recomputes a fresh value
           (memory read, constant materialization, or pure arithmetic).
           [Description.verify] has checked the rematerialization the allocator
           recorded for this instruction against the description (cf.
           [Description.verify_rematerialization]) and resolved it into the
           source's description instruction with the rematerialized register as
           its result; the equation transfer thus uses the same abstract
           register stamps as the description-based equations already in the
           equation set. Synthesizing [reg_instr] from the post-allocation
           instruction's [Reg.t] values directly would use the *fresh*
           post-split stamps from [compute_substitution_tree], not the
           description's stamps, and [remove_result]'s "0-or-2 sides" invariant
           would trip on equations keyed by the description's stamps. The
           instruction's [desc] is that of the source, so [destroyed_at_basic]
           returns the same destroyed locations as for the original. *)
        let reg_instr : basic Instruction.t =
          match Description.find_rematerialized verification instr.id with
          | Some reg_instr -> reg_instr
          | None ->
            Regalloc_utils.fatal
              "Rematerialized instruction no. %a was not checked by \
               [Description.verify]"
              InstructionId.format instr.id
        in
        (* The copy's result stamp is exempted from the "one register at two
           locations" check while its result equation is removed: see
           [Equation_set.compatible_one]. *)
        let relaxed =
          Register.Set.of_list (Array.to_list reg_instr.Instruction.res)
        in
        append_equations t ~instr_kind:Instruction.Kind.Basic ~exn:None ~relaxed
          ~reg_instr ~loc_instr:instr
          ~destroyed:(Proc.destroyed_at_basic instr.desc |> Location.of_regs_exn)
      | _ ->
        Regalloc_utils.fatal
          "Regalloc_validate.basic: added instruction no. %a must be Spill, \
           Reload, Move, or a rematerialized instruction: %a"
          InstructionId.format instr.id Printcfg.basic instr)
    | Some instr_before ->
      let equations =
        match instr.desc with
        | Op Move
          when Array.length instr.arg = 1
               && Array.length instr.res = 1
               && Reg.same_loc instr.arg.(0) instr.res.(0) ->
          (* This corresponds to a noop move where the source and target
             registers have the same locations. *)
          Result.ok @@ rename_register t ~reg_instr:instr_before
        | _ ->
          append_equations t ~instr_kind:Instruction.Kind.Basic ~exn:None
            ~relaxed:Register.Set.empty ~reg_instr:instr_before ~loc_instr:instr
            ~destroyed:
              (Proc.destroyed_at_basic instr.desc |> Location.of_regs_exn)
      in
      Result.bind equations
        (check_deleted_before ~instr_kind:Instruction.Kind.Basic
           ~reg_instr:instr_before ~loc_instr:instr)

  let terminator t ~exn instr () =
    match Description.find_terminator description instr with
    | Some instr_before ->
      (* CR-soon azewierzejew: This is kind of fragile for [Tailcall (Self _)]
         because that instruction doesn't strictly adhere to generic
         semantics. *)
      let exn =
        if Cfg.can_raise_terminator instr.desc then Some exn else None
      in
      let equations =
        append_equations t ~instr_kind:Instruction.Kind.Terminator ~exn
          ~relaxed:Register.Set.empty ~reg_instr:instr_before ~loc_instr:instr
          ~destroyed:
            (Proc.destroyed_at_terminator instr.desc |> Location.of_regs_exn)
      in
      Result.bind equations
        (check_deleted_before ~instr_kind:Instruction.Kind.Terminator
           ~reg_instr:instr_before ~loc_instr:instr)
    | None -> (
      Result.ok
      @@
      match instr.desc with
      | Always _ ->
        (* [Always] is always an identity. *)
        assert (not (Cfg.can_raise_terminator instr.desc));
        assert (Array.length instr.arg = 0);
        assert (Array.length instr.res = 0);
        t
      | _ ->
        Regalloc_utils.fatal
          "Register allocation added a terminator no. %a but that's not \
           allowed for this type of terminator: %a"
          InstructionId.format instr.id Printcfg.terminator instr)

  (* This should remove the equations for the exception value, but we do that in
     [Domain.append_equations] because there we have more information to give if
     there's an error. *)
  let exception_ t () = Ok t
end

module Check_backwards (Inputs : Transfer_inputs) =
  Cfg_dataflow.Backward (Domain) (Transfer (Inputs))

let save_as_dot_with_equations ~desc ~res_instr ~res_block ?filename cfg msg =
  Cfg_with_layout.save_as_dot
    ~annotate_instr:
      [ (fun ppf instr ->
          let id =
            match instr with
            | `Basic instr -> instr.id
            | `Terminator instr -> instr.id
          in
          InstructionId.Tbl.find_opt res_instr id
          |> Format.pp_print_option
               ~none:(fun ppf () -> Format.fprintf ppf "Unknown")
               Equation_set.print ppf);
        Printcfg.instruction_with_print_reg ~print_reg:print_reg_as_loc;
        (fun ppf instr ->
          let print printer find instr =
            match find desc instr with
            | Some prev_instr ->
              let instr = Instruction.to_prealloc ~alloced:instr prev_instr in
              printer ppf instr
            | None -> ()
          in
          match instr with
          | `Basic instr -> print Printcfg.basic Description.find_basic instr
          | `Terminator ti ->
            print Printcfg.terminator Description.find_terminator ti) ]
    ~annotate_block_end:(fun ppf block ->
      Label.Tbl.find_opt res_block block.start
      |> Format.pp_print_option
           ~none:(fun ppf () -> Format.fprintf ppf "Unknown")
           Equation_set.print ppf)
    ?filename cfg msg;
  ()

module Error : sig
  module At_entrypoint : sig
    type t =
      { message : string;
        equations : Equation_set.t;
        reg_fun_args : Register.t array;
        loc_fun_args : Location.t array
      }
  end

  module Source : sig
    type t =
      | At_instruction of Transfer_error.t
      | At_entrypoint of At_entrypoint.t
  end

  type t =
    { source : Source.t;
      res_instr : Domain.t InstructionId.Tbl.t;
      res_block : Domain.t Label.Tbl.t;
      desc : Description.t;
      cfg : Cfg_with_layout.t
    }

  val dump : Format.formatter -> t -> unit

  val print : Format.formatter -> t -> unit
end = struct
  module At_entrypoint = struct
    type t =
      { message : string;
        equations : Equation_set.t;
        reg_fun_args : Register.t array;
        loc_fun_args : Location.t array
      }
  end

  module Source = struct
    type t =
      | At_instruction of Transfer_error.t
      | At_entrypoint of At_entrypoint.t

    let print (ppf : Format.formatter) (t : t) : unit =
      match t with
      | At_instruction error -> Transfer_error.print ppf error
      | At_entrypoint { message; equations; reg_fun_args; loc_fun_args } ->
        Format.fprintf ppf "Bad equations at entry point, reason: %s\n" message;
        Format.fprintf ppf "Equations: %a\n" Equation_set.print equations;
        Format.fprintf ppf "Function argument descriptions: %a\n"
          (Format.pp_print_seq
             ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
             Register.print)
          (Array.to_seq reg_fun_args);
        Format.fprintf ppf "Function argument locations: %a\n"
          (Format.pp_print_seq
             ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
             (fun ppf (reg, loc) -> Location.print (Register.typ reg) ppf loc))
          (Array.to_seq (Array.combine reg_fun_args loc_fun_args));
        ()
  end

  type t =
    { source : Source.t;
      res_instr : Domain.t InstructionId.Tbl.t;
      res_block : Domain.t Label.Tbl.t;
      desc : Description.t;
      cfg : Cfg_with_layout.t
    }

  let print (ppf : Format.formatter) ({ source; _ } : t) : unit =
    Source.print ppf source

  let dump (ppf : Format.formatter)
      ({ source; res_instr; res_block; desc; cfg } : t) : unit =
    Source.print ppf source;
    let filename =
      Filename.temp_file
        (X86_proc.string_of_symbol "" (Cfg_with_layout.cfg cfg).fun_name ^ "_")
        ".dot"
    in
    Format.fprintf ppf "Dumping cfg into %s ...\n" filename;
    save_as_dot_with_equations ~desc ~res_instr ~res_block ~filename cfg
      "validation_error";
    Format.fprintf ppf "Dumped cfg into: %s\n" filename;
    ()
end

let verify_entrypoint (equations : Equation_set.t) (desc : Description.t)
    (cfg : Cfg_with_layout.t) :
    (Cfg_with_layout.t, Error.At_entrypoint.t) Result.t =
  let reg_fun_args = Description.reg_fun_args desc in
  let loc_fun_args = Location.of_regs_exn (Cfg_with_layout.cfg cfg).fun_args in
  let bind f r = Result.bind r f in
  Equation_set.remove_result ~relaxed:Register.Set.empty ~reg_res:reg_fun_args
    ~loc_res:loc_fun_args equations
  |> bind (fun equations ->
      (* This check is stronger than the one in the paper [1]. That because C
         allows to start with uninitialized variables as it's explained in
         chapter 3.2 and of section "Dataflow Analysis and Its Uses". Such a
         thing is not allowed in OCaml. Therefore after removing all equations
         for arguments the should be no additional equations left. *)
      if Equation_set.is_empty equations
      then Ok cfg
      else (
        Format.fprintf Format.str_formatter
          "Some equations still present at entrypoint after removing parameter \
           equations: [%a]"
          Equation_set.print equations;
        let message = Format.flush_str_formatter () in
        Error message))
  |> Result.map_error (fun message : Error.At_entrypoint.t ->
      { message; equations; reg_fun_args; loc_fun_args })

let test (desc : Description.t)
    ~(rematerializations : Regalloc_rematerializations.t)
    (cfg : Cfg_with_layout.t) : (Cfg_with_layout.t, Error.t) Result.t =
  if Regalloc_utils.Param.get Regalloc_utils.validator_debug
  then
    (* CR-someday: We don't save the file with [fun_name] in the filename
       because there is an appended stamp that is fragile and is annoying when
       testing. Currently it's not a problem because we abort the build whenever
       register allocation fails but if there was a fallback mode then the
       interesting files would be instantly overwritten. *)
    Cfg_with_layout.save_as_dot
      ~annotate_instr:
        [Printcfg.instruction_with_print_reg ~print_reg:print_reg_as_loc]
      ~filename:"after.dot" cfg "after_allocation_before_validation";
  let verification = Description.verify desc ~rematerializations cfg in
  let module Check_backwards = Check_backwards (struct
    let description = desc

    let verification = verification
  end) in
  let res_instr, res_block, result =
    match
      Check_backwards.run (Cfg_with_layout.cfg cfg) ~init:Domain.bot
        ~map:Check_backwards.Both ()
    with
    | Ok (res_instr, res_block) -> res_instr, res_block, Ok ()
    | Aborted ((res_instr, res_block), error) ->
      res_instr, res_block, Error error
    | Max_iterations_reached ->
      Regalloc_utils.fatal
        "Unable to compute validation equation sets from CFG for function %s@."
        (Cfg_with_layout.cfg cfg).fun_name
  in
  if Regalloc_utils.Param.get Regalloc_utils.validator_debug
  then
    (* CR-someday: We don't save the file with [fun_name] in the filename
       because there is an appended stamp that is fragile and is annoying when
       testing. Currently it's not a problem because we abort the build whenever
       register allocation fails but if there was a fallback mode then the
       interesting files would be instantly overwritten. *)
    save_as_dot_with_equations ~desc ~res_instr ~res_block ~filename:"annot.dot"
      cfg "after_allocation_after_validation";
  match result with
  | Ok () ->
    let entrypoint_equations =
      let cfg = Cfg_with_layout.cfg cfg in
      let entry_block = Cfg.entry_label cfg |> Cfg.get_block_exn cfg in
      let entry_id = Cfg.first_instruction_id entry_block in
      InstructionId.Tbl.find res_instr entry_id
    in
    verify_entrypoint entrypoint_equations desc cfg
    |> Result.map_error (fun (error : Error.At_entrypoint.t) : Error.t ->
        { source = At_entrypoint error; res_instr; res_block; desc; cfg })
  | Error error ->
    Error { source = At_instruction error; res_instr; res_block; desc; cfg }

let run desc ~rematerializations cfg_with_infos =
  let cfg = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  match desc with
  | None -> cfg_with_infos
  | Some desc -> (
    match test desc ~rematerializations cfg with
    | Ok _ -> cfg_with_infos
    | Error error -> Regalloc_utils.fatal "%a%!" Error.dump error)
