(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2026 Jane Street Group LLC                                       *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Oxcaml_utils.Doubly_linked_list
module Int = Numbers.Int

(* Once a duplicate function is identified, three rewrite strategies are
   plausible: 1. Detect and report only: emit a diagnostic listing the
   equivalence classes but leave the CFGs untouched. Useful for measurement
   before committing to a rewriting strategy. 2. Symbol alias: collapse the
   duplicate's symbol to the representative's via an assembler alias (e.g. `.set
   foo, bar`). Zero instruction overhead, but the duplicate no longer has its
   own entry point and its address coincides with the representative's, which
   matters if anything compares function addresses for equality. 3. Tail-call
   thunk: keep the duplicate's symbol and entry point but replace its body with
   a single tail call to the representative. Costs one jump per call into the
   duplicate, but preserves distinct addresses. This is the strategy implemented
   here. *)

(* CR xclerc: the per-bucket cap mirrors [Cfg_merge_blocks]. *)
let max_bucket_size = 128

(* Cap on the number of representatives per bucket; beyond this we stop
   comparing to keep the cost bounded. *)
type repr =
  { fd_cmm : Cmm.fundecl;
    cfg_with_layout : Cfg_with_layout.t
  }

let buckets : repr list Int.Tbl.t = Int.Tbl.create 64

let reset_unit_info () = Int.Tbl.reset buckets

let same_loc r1 r2 =
  Reg.same_loc_fatal_on_unknown
    ~fatal_message:"Cfg_merge_functions: unknown register location." r1 r2

let equal_fun_args left right = Misc.Stdlib.Array.equal same_loc left right

let equal_poll_attribute (l : Lambda.poll_attribute) (r : Lambda.poll_attribute)
    =
  match l, r with
  | Error_poll, Error_poll | Default_poll, Default_poll -> true
  | (Error_poll | Default_poll), _ -> false

let equal_stack_slots left right =
  Stack_class.Tbl.fold left ~init:true ~f:(fun stack_class n acc ->
      acc && Int.equal n (Stack_class.Tbl.find right stack_class))

(* CR xclerc: the function-level guard below is intentionally conservative: it
   requires equality of every [Cfg.t] field that could conceivably matter. In
   particular, [fun_codegen_options] is compared per constructor, which means
   two otherwise-equivalent functions that disagree on (say) a [Cold] attribute
   or carry a [Check_zero_alloc] with distinct source locations will fail to
   merge. The choice should be revisited once we have data on what gets
   rejected. *)
let equal_fun_level_fields (left : Cfg.t) (right : Cfg.t) =
  equal_fun_args left.fun_args right.fun_args
  && Cmm.equal_machtype left.fun_ret_type right.fun_ret_type
  && Bool.equal left.fun_contains_calls right.fun_contains_calls
  && equal_poll_attribute left.fun_poll right.fun_poll
  && equal_stack_slots left.fun_num_stack_slots right.fun_num_stack_slots
  && List.equal Cfg.equal_codegen_option left.fun_codegen_options
       right.fun_codegen_options

let equivalent ~(incoming : Cmm.fundecl) ~(incoming_cfg : Cfg_with_layout.t)
    ~(repr : repr) =
  let incoming_cfg_t = Cfg_with_layout.cfg incoming_cfg in
  let repr_cfg_t = Cfg_with_layout.cfg repr.cfg_with_layout in
  if not (equal_fun_level_fields incoming_cfg_t repr_cfg_t)
  then false
  else
    let subst = Cfg_equiv_subst.make () in
    (* Seed self-recursion: a `Direct incoming_sym` in [incoming]'s body must be
       considered equivalent to a `Direct repr_sym` in [repr]'s body. *)
    Cfg_equiv_subst.add_symbol subst incoming.fun_name repr.fd_cmm.fun_name;
    (* CR xclerc: [ignore_dbg]/[ignore_name_for_debugger] default to [true]:
       debug information must not prevent merging. Worth revisiting once we care
       about debugger fidelity for merged functions. *)
    Cfg_equiv.equiv_cfg_with_layout ~ignore_name_for_debugger:true
      ~ignore_dbg:true subst incoming_cfg repr.cfg_with_layout

let build_thunk ~(fd_cmm : Cmm.fundecl) ~(repr : repr) : Cfg_with_layout.t =
  let repr_sym = repr.fd_cmm.fun_name in
  let fun_args = (Cfg_with_layout.cfg repr.cfg_with_layout).fun_args in
  let loc_arg = Array.copy fun_args in
  let next_instruction_id = InstructionId.make_sequence () in
  let cfg =
    Cfg.create ~fun_name:fd_cmm.fun_name.sym_name ~fun_args:loc_arg
      ~fun_codegen_options:
        (Cfg.of_cmm_codegen_option fd_cmm.fun_codegen_options)
      ~fun_dbg:fd_cmm.fun_dbg ~fun_contains_calls:true
      ~fun_num_stack_slots:(Stack_class.Tbl.make 0) ~fun_poll:fd_cmm.fun_poll
      ~next_instruction_id ~fun_ret_type:fd_cmm.fun_ret_type
      ~allowed_to_be_irreducible:false
  in
  cfg.register_locations_are_set <- true;
  let terminator : Cfg.terminator Cfg.instruction =
    { desc = Cfg.Tailcall_func (Cfg.Direct repr_sym);
      arg = loc_arg;
      res = [||];
      dbg = Debuginfo.none;
      fdo = Fdo_info.none;
      live = Reg.Set.empty;
      stack_offset = 0;
      id = InstructionId.get_and_incr next_instruction_id;
      available_before = Unreachable;
      available_across = Unreachable
    }
  in
  let entry_block =
    Cfg.make_empty_block ~label:(Cfg.entry_label cfg) terminator
  in
  entry_block.stack_offset <- 0;
  entry_block.can_raise <- false;
  Cfg.add_block_exn cfg entry_block;
  let layout = DLL.make_empty () in
  DLL.add_end layout entry_block.start;
  Cfg_with_layout.create cfg ~layout

let run fd_cmm cfg_with_layout =
  let hash = Cfg_quick_hash.cfg_with_layout cfg_with_layout in
  let bucket =
    match Int.Tbl.find_opt buckets hash with None -> [] | Some l -> l
  in
  let rec find_match = function
    | [] -> None
    | repr :: rest ->
      if equivalent ~incoming:fd_cmm ~incoming_cfg:cfg_with_layout ~repr
      then Some repr
      else find_match rest
  in
  if List.length bucket >= max_bucket_size
  then
    (* Bucket saturated: do not even attempt the comparison, but do not register
       this function either (we would never compare against it anyway). *)
    cfg_with_layout
  else
    match find_match bucket with
    | Some repr -> build_thunk ~fd_cmm ~repr
    | None ->
      Int.Tbl.replace buckets hash ({ fd_cmm; cfg_with_layout } :: bucket);
      cfg_with_layout
