[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Oxcaml_utils.Doubly_linked_list
module Int = Numbers.Int
module List = ListLabels

(* CR-soon xclerc for xclerc: some work may be needed on the hash functions, as
   the pass performance seems to be quite sensitive to it. *)
module Quick_hash : sig
  val basic_block : Cfg.basic_block -> int
end = struct
  let[@inline] hash_combine h1 h2 = (h1 * 65599) + h2

  let operation : Operation.t -> int =
   fun op ->
    let repr = Obj.repr op in
    if Obj.is_int repr then -(Obj.magic repr : int) else Obj.tag repr

  let basic : Cfg.basic -> int = function
    | Op op -> operation op
    | Reloadretaddr -> 1
    | Pushtrap { lbl_handler } -> hash_combine 2 (Label.hash lbl_handler)
    | Poptrap { lbl_handler } -> hash_combine 3 (Label.hash lbl_handler)
    | Prologue -> 4
    | Epilogue -> 5
    | Stack_check { max_frame_size_bytes } ->
      hash_combine 6 max_frame_size_bytes

  let terminator term =
    let repr = Obj.repr term in
    if Obj.is_int repr then -(Obj.magic repr : int) else Obj.tag repr

  let rec basic_instruction_cell :
      Cfg.basic Cfg.instruction DLL.cell option -> fuel:int -> acc:int -> int =
   fun cell ~fuel ~acc ->
    if fuel <= 0
    then acc
    else
      match cell with
      | None -> acc
      | Some cell ->
        let instr_hash = basic (DLL.value cell).desc in
        basic_instruction_cell (DLL.next cell) ~fuel:(pred fuel)
          ~acc:(hash_combine acc instr_hash)

  let basic_instruction_list : Cfg.basic Cfg.instruction DLL.t -> int =
   fun l ->
    (* CR-someday xclerc for xclerc: also use the list length? *)
    basic_instruction_cell (DLL.hd_cell l) ~fuel:4 ~acc:0

  let basic_block : Cfg.basic_block -> int =
   fun block ->
    let body_hash = basic_instruction_list block.body in
    let term_hash = terminator block.terminator.desc in
    hash_combine body_hash term_hash
end

module Equivalent : sig
  val basic_block :
    equal_reg:(Reg.t -> Reg.t -> bool) ->
    Cfg.basic_block ->
    Cfg.basic_block ->
    bool
end = struct
  let instruction : type a.
      equal_desc:(a -> a -> bool) ->
      equal_reg:(Reg.t -> Reg.t -> bool) ->
      a Cfg.instruction ->
      a Cfg.instruction ->
      bool =
   fun ~equal_desc ~equal_reg left right ->
    (* CR xclerc for xclerc: consider using other fields *)
    match left, right with
    | ( { desc = left_desc;
          id = _;
          arg = left_arg;
          res = left_res;
          dbg = left_dbg;
          fdo = _;
          live = _;
          stack_offset = left_stack_offset;
          available_before = _;
          available_across = _
        },
        { desc = right_desc;
          id = _;
          arg = right_arg;
          res = right_res;
          dbg = right_dbg;
          fdo = _;
          live = _;
          stack_offset = right_stack_offset;
          available_before = _;
          available_across = _
        } ) ->
      Int.equal left_stack_offset right_stack_offset
      && Misc.Stdlib.Array.equal equal_reg left_arg right_arg
      && Misc.Stdlib.Array.equal equal_reg left_res right_res
      && equal_desc left_desc right_desc
      (* CR-someday xclerc for xclerc: consider definin equal in `Debuginfo` *)
      && Debuginfo.compare left_dbg right_dbg = 0

  let basic_block :
      equal_reg:(Reg.t -> Reg.t -> bool) ->
      Cfg.basic_block ->
      Cfg.basic_block ->
      bool =
   fun ~equal_reg left right ->
    (* CR xclerc for xclerc: consider using other fields *)
    match left, right with
    | ( { start = _;
          body = left_body;
          terminator = left_terminator;
          predecessors = _;
          stack_offset = left_stack_offset;
          exn = left_exn;
          can_raise = _;
          is_trap_handler = left_is_trap_handler;
          cold = _
        },
        { start = _;
          body = right_body;
          terminator = right_terminator;
          predecessors = _;
          stack_offset = right_stack_offset;
          exn = right_exn;
          can_raise = _;
          is_trap_handler = right_is_trap_handler;
          cold = _
        } ) ->
      Int.equal left_stack_offset right_stack_offset
      && Bool.equal left_is_trap_handler right_is_trap_handler
      && Option.equal Label.equal left_exn right_exn
      && instruction ~equal_desc:Cfg.equal_terminator ~equal_reg left_terminator
           right_terminator
      && DLL.equal
           (fun (left : Cfg.basic Cfg.instruction)
                (right : Cfg.basic Cfg.instruction) ->
             instruction ~equal_desc:Cfg.equal_basic ~equal_reg left right)
           left_body right_body
end

let run :
    equal_reg:(Reg.t -> Reg.t -> bool) -> Cfg_with_layout.t -> Cfg_with_layout.t
    =
 fun ~equal_reg cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  (* step 1: put blocks in buckets according to their hash values *)
  let buckets : Cfg.basic_block list Int.Tbl.t =
    Int.Tbl.create (Label.Tbl.length cfg.blocks)
  in
  Cfg.iter_blocks cfg ~f:(fun label block ->
      (* CR-soon xclerc for xclerc: can the trap handler restriction be
         lifted? *)
      if (not block.is_trap_handler) && not (Label.equal label cfg.entry_label)
      then begin
        let block_hash = Quick_hash.basic_block block in
        let curr =
          match Int.Tbl.find_opt buckets block_hash with
          | Some l -> l
          | None -> []
        in
        Int.Tbl.replace buckets block_hash (block :: curr)
      end);
  (* step 2: compute rewrites, as a substition over labels *)
  let rewrites : Label.t Label.Tbl.t = Label.Tbl.create 17 in
  Int.Tbl.iter
    (fun _hash_value blocks ->
      let (_ : Cfg.basic_block list) =
        (* since the identification of rewrites is quadratic, we just skip
           buckets with too many blocks *)
        (* CR-someday xclerc for xclerc: double check the threshold value is good in practice *)
        if List.length blocks > 128
        then []
        else
          List.fold_left blocks ~init:[]
            ~f:(fun seen (block : Cfg.basic_block) ->
              match
                List.find_opt seen ~f:(fun b ->
                    Equivalent.basic_block ~equal_reg block b)
              with
              | None -> block :: seen
              | Some repr ->
                Label.Tbl.replace rewrites block.start repr.Cfg.start;
                seen)
      in
      ())
    buckets;
  (* step 3: apply the rewrites, if any *)
  match Label.Tbl.length rewrites with
  | 0 -> cfg_with_layout
  | _ ->
    Cfg.iter_blocks cfg ~f:(fun _label block ->
        Cfg.replace_successor_labels cfg ~normal:true ~exn:false block
          ~f:(fun successor_label ->
            match Label.Tbl.find_opt rewrites successor_label with
            | None -> successor_label
            | Some new_successor_label -> new_successor_label));
    Cfg.register_predecessors_for_all_blocks cfg;
    (* CR-someday xclerc for xclerc: calling `Cfg_simplify.run` is probably
       overkill (we just want to remove unreachable blocks), and also
       unnecessary if the next step in the pipeline does the simplification. For
       now, we simplify to be on the safe side. *)
    Cfg_simplify.run cfg_with_layout

let run_before_register_allocation cfg_with_layout =
  let equal_reg x y = Reg.same x y in
  run ~equal_reg cfg_with_layout

let run_after_register_allocation cfg_with_layout =
  let equal_reg x y =
    Reg.same_loc_fatal_on_unknown
      ~fatal_message:
        "Cfg_merge_blocks.run_after_register_allocation should only be run \
         after register allocation"
      x y
  in
  run ~equal_reg cfg_with_layout
