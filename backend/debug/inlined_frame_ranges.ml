(******************************************************************************
 *                                  OxCaml                                    *
 *                       Mark Shinwell, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Int_replace_polymorphic_compare
module L = Linear

(* CR mshinwell/xclerc/poechsel:

   1. maybe "set" isn't the correct algebraic structure for the Compute_ranges
   interface

   2. maybe a list-based structure could be used here instead (not for variable
   ranges) *)

module Inlined_frames = struct
  module Key = struct
    module T0 = struct
      type t = Debuginfo.t

      let print = Debuginfo.print_compact_extended

      let compare t1 t2 =
        (* It needs to be the case that [compare shorter longer] returns -1 (and
           vice-versa) when [shorter] is a prefix of [longer] whilst also having
           fewer frames. See dwarf_inlined_frames:create_up_to_root. *)
        let items1 = Debuginfo.to_items t1 in
        let items2 = Debuginfo.to_items t2 in
        let rec loop (items1 : Debuginfo.item list)
            (items2 : Debuginfo.item list) =
          match items1, items2 with
          | [], [] -> 0
          | [], _ :: _ -> -1
          | _ :: _, [] -> 1
          | i1 :: items1, i2 :: items2 ->
            (* CR mshinwell: think about the [None] cases some more *)
            let c = Option.compare String.compare i1.dinfo_uid i2.dinfo_uid in
            if c <> 0
            then c
            else
              let c =
                Option.compare String.compare i1.dinfo_function_symbol
                  i2.dinfo_function_symbol
              in
              if c <> 0 then c else loop items1 items2
        in
        loop items1 items2

      let equal t1 t2 = compare t1 t2 = 0

      let hash = Hashtbl.hash

      let output _ _ = Misc.fatal_error "Not implemented"
    end

    include T0
    include Identifiable.Make (T0)

    type key = t

    module Raw_set = Set

    module Set = struct
      type t =
        | Ok of Raw_set.t
        | Unreachable

      let of_list keys = Ok (Raw_set.of_list keys)

      let union t1 t2 =
        match t1, t2 with
        | Unreachable, _ | _, Unreachable -> Unreachable
        | Ok s1, Ok s2 -> Ok (Raw_set.union s1 s2)

      let inter t1 t2 =
        match t1, t2 with
        | Unreachable, t | t, Unreachable -> t
        | Ok s1, Ok s2 -> Ok (Raw_set.inter s1 s2)

      let diff t1 t2 =
        match t1, t2 with
        | Unreachable, _ -> Unreachable
        | _, Unreachable -> Ok Raw_set.empty
        | Ok s1, Ok s2 -> Ok (Raw_set.diff s1 s2)

      let fold f t init =
        match t with Unreachable -> init | Ok s -> Raw_set.fold f s init

      let print ppf t =
        match t with
        | Unreachable -> Format.pp_print_string ppf "Unreachable"
        | Ok s -> Raw_set.print ppf s
    end

    let parent t =
      match List.rev t with [] | [_] -> None | _ :: t -> Some (List.rev t)

    let all_parents _t = []
  end

  module Index = struct
    include Key
    module Set = Raw_set
  end

  module Subrange_state : Compute_ranges_intf.S_subrange_state = struct
    type t = unit

    let create () = ()

    let advance_over_instruction () _ = ()
  end

  module Subrange_info :
    Compute_ranges_intf.S_subrange_info
      with type key := Key.t
      with type subrange_state := Subrange_state.t = struct
    type t = unit

    let create _var _subrange_state ~fun_contains_calls:_ ~fun_num_stack_slots:_
        =
      ()

    let print ppf () = Format.pp_print_string ppf "()"
  end

  module Range_info :
    Compute_ranges_intf.S_range_info
      with type key := Key.t
      with type index := Index.t = struct
    type t = unit

    let create _fundecl key ~start_insn:_ = Some (key, ())

    let print ppf () = Format.pp_print_string ppf "()"
  end

  (* Given a non-empty Debuginfo.t, return the list of all non-empty prefixes:
     the value itself and each of its parents (the path with progressively fewer
     deeper frames). *)
  let dbg_and_parents dbg =
    let items = Debuginfo.to_items dbg in
    match items with
    | [] -> []
    | _ :: _ ->
      let rec parents (t : Debuginfo.item list) =
        match List.rev t with
        | [] | [_] -> []
        | _ :: rest ->
          let prefix = List.rev rest in
          Debuginfo.of_items prefix :: parents prefix
      in
      dbg :: parents items

  let is_spill_or_reload (insn : L.instruction) =
    match insn.desc with
    | Lop (Spill | Reload) -> true
    | Lprologue | Lepilogue_open | Lepilogue_close | Lend | Lop _ | Lcall_op _
    | Lreloadretaddr | Lreturn | Llabel _ | Lbranch _ | Lcondbranch _
    | Lcondbranch3 _ | Lswitch _ | Lentertrap | Ladjust_stack_offset _
    | Lpushtrap _ | Lpoptrap _ | Lraise _ | Lstackcheck _ ->
      false
  [@@ocaml.warning "-4"]

  let available_before (_fundecl : L.fundecl) (insn : L.instruction) =
    (* Inlined-frame keys are derived solely from the instruction's own [dbg]
       (and all of its parents): an instruction physically located inside an
       inlined frame keeps that frame alive. Since each instruction identifies
       exactly one frame at each inlining depth, the ranges of sibling frames
       may interleave but can never overlap, as required by the DWARF
       specification for DW_TAG_inlined_subroutine DIEs.

       In particular, the ranges are _not_ extended to cover the scopes of
       phantom variables bound in inlined frames: such scopes extend to the end
       of the enclosing function's body and would cause sibling frames' ranges
       to overlap. (The consequence is that a fully optimized-out inlined
       function, none of whose instructions remain, gets no
       DW_TAG_inlined_subroutine DIE; the phantom lets corresponding to its
       parameters are correspondingly restricted -- see
       [Available_ranges_phantom_vars].)

       Spill and reload instructions are register-allocator bookkeeping, not
       code lowered from any inlined body; typically they preserve the
       _enclosing_ function's state across a call to which the surrounding
       debuginfo relates. They are treated as belonging to no inlined frame (an
       empty key set, closing any open ranges), so that the address ranges of
       inlined frames never cover them. This matters in particular for reloads
       of the enclosing function's variables after a call within an inlined
       body: such reloads must not be attributed to the inlined function (e.g.
       by a profiler). Note that returning [None] would not suffice, since that
       would leave existing open ranges untouched. *)
    if is_spill_or_reload insn
    then Some (Key.Set.Ok Key.Raw_set.empty)
    else
      match Debuginfo.to_items insn.dbg with
      | [] -> None
      | _ :: _ ->
        Some (Key.Set.Ok (Key.Raw_set.of_list (dbg_and_parents insn.dbg)))

  let available_across fundecl insn =
    (* A single [Linear] instruction never spans inlined frames. *)
    available_before fundecl insn

  let must_restart_ranges_upon_any_change () = false
end

module Subrange_state = Inlined_frames.Subrange_state
module Subrange_info = Inlined_frames.Subrange_info
module Range_info = Inlined_frames.Range_info
include Compute_ranges.Make (Inlined_frames)
