(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Mark Shinwell, Jane Street Europe                *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import
module Ece = Effects_and_coeffects
module K = Flambda_kind

module C = struct
  include Cmm_helpers
  include Cmm_builtins
  include To_cmm_shared
end

let fail_if_probe apply =
  match Apply.probe apply with
  | None -> ()
  | Some _ ->
    Misc.fatal_errorf
      "[Apply] terms with a [probe] (i.e. that call a tracing probe) must \
       always be direct applications of an OCaml function:@ %a"
      Apply.print apply

(* Unboxed products may be passed to and returned from external calls. Such a
   product follows the calling convention that C would use for a struct whose
   members are the components of the product, in order, with any nested unboxed
   products corresponding to nested structs. (Products from [@unpacked] external
   declarations do not appear here: closure conversion unarizes those into
   separate scalar arguments.)

   The backend (see [Proc.loc_external_arguments] and
   [Proc.loc_external_results]) assigns one register or stack slot to each
   element of the Cmm-level argument and result descriptions, with no notion of
   aggregates. To implement the C struct convention on top of this, each unboxed
   product is split here into register-sized "units", mirroring how the C ABIs
   assign whole registers to pieces of structs:

   - On x86-64 (the System V ABI, used on both Linux and macOS), a struct of at
   most 16 bytes is split into its 8-byte pieces ("eightbytes"). Each piece is
   passed in an SSE register if every component within it is floating-point, and
   otherwise in an integer register.

   - On arm64 (both the AAPCS64 and Apple variants, which differ only in respect
   of stack layout), a homogeneous floating-point/vector aggregate -- at most
   four members, all of the same machine type -- is passed with each member in
   its own SIMD register. Any other composite of at most 16 bytes is passed in
   integer registers, each holding one 8-byte piece ("doubleword") of the
   struct's in-memory representation, including the bit patterns of any
   floating-point components.

   Where a unit holds more than one component, or holds a floating-point
   component in an integer register, this pass emits the necessary bit-level
   packing and unpacking operations (shifts, masks and reinterpret casts) around
   the call; the backend just sees one scalar per unit. (On the result side,
   note that arm64 homogeneous aggregate returns of up to four members require
   d0-d3 to be available in [Proc.loc_external_results].)

   The remaining unsupported cases, which cause fatal errors, are products that
   C passes in memory:

   - structs larger than 16 bytes (excepting arm64 homogeneous aggregates, which
   may reach 64 bytes), which arm64 passes by reference to a caller-allocated
   copy and x86-64 passes on the stack;

   - structs demoted to memory because insufficient argument registers remain
   for them (registers are tracked across the argument list to detect this);

   - struct returns in memory.

   Supporting those cases genuinely requires the interface to the backend to
   change: the [Cmm.exttype] values given to [Proc.loc_external_arguments]
   describe each unit separately, with no indication of which units together
   form an aggregate. The backend can therefore neither lay out an aggregate on
   the stack as a single entity, nor implement pass-by-reference, nor leave
   registers unused when a whole aggregate fails to fit in the remaining ones (C
   ABIs never split an aggregate between registers and memory). Likewise, memory
   returns require the caller to allocate a temporary and pass its address as a
   hidden parameter (in the first integer argument register on x86-64,
   displacing the other arguments; in x8 on arm64, which the backend does not
   currently model at all), the components then being read back from the
   temporary after the call; there is currently no way to allocate such a stack
   temporary around a Cmm [Cextcall].

   CR mshinwell: lift the restrictions on memory-passed unboxed products by
   extending Cmm and the backend as described above.

   CR mshinwell: for as long as the restrictions remain, it would be better to
   reject the unsupported shapes at type-checking time with a located error,
   rather than via the fatal errors below (which additionally fire as soon as an
   offending external is merely declared in a natively-compiled module, even if
   the declaration is usable in bytecode). See the CR in
   [Primitive.Repr_check.check_c_stub] for the difficulties involved.

   Since the layout of a C struct is sensitive to nesting (through the alignment
   and size-rounding of nested structs), products are laid out here following
   the structure recorded in the [`Complex] argument and return arities of the
   [Apply] expression. *)

type extcall_reg_class =
  | Int_regs
  | Float_regs

type extcall_abi =
  | Amd64_system_v
  | Aarch64

let extcall_abi_supporting_unboxed_products apply =
  match Target_system.architecture () with
  | X86_64 ->
    if Target_system.is_windows ()
    then
      Misc.fatal_errorf
        "Unboxed products in external calls are not supported on Windows:@ %a"
        Apply.print apply
    else Amd64_system_v
  | AArch64 -> Aarch64
  | IA32 | ARM | POWER | Z | Riscv ->
    Misc.fatal_errorf
      "Unboxed products in external calls are only supported on x86-64 and \
       arm64:@ %a"
      Apply.print apply

(* The register classes and sizes here must agree with [C.exttype_of_kind],
   [Cmm.machtype_of_exttype] and the calling convention functions in the
   relevant [Proc] modules. All of these kinds have natural alignment equal to
   their size. *)
let extcall_reg_class_and_size apply (kind : K.With_subkind.t) =
  match K.With_subkind.kind kind with
  | Value | Naked_number (Naked_immediate | Naked_int64 | Naked_nativeint) ->
    Int_regs, 8
  | Naked_number Naked_int32 -> Int_regs, 4
  | Naked_number Naked_int16 -> Int_regs, 2
  | Naked_number Naked_int8 -> Int_regs, 1
  | Naked_number Naked_mask ->
    (* Masks are passed to external calls as 64-bit integers; see
       [C.exttype_of_kind]. *)
    Int_regs, 8
  | Naked_number Naked_float -> Float_regs, 8
  | Naked_number Naked_float32 -> Float_regs, 4
  | Naked_number Naked_vec128 -> Float_regs, 16
  | Naked_number Naked_vec256 -> Float_regs, 32
  | Naked_number Naked_vec512 -> Float_regs, 64
  | Region | Rec_info ->
    Misc.fatal_errorf "External calls cannot involve components of kind %a:@ %a"
      K.With_subkind.print kind Apply.print apply

type extcall_component =
  { kind : K.With_subkind.t;
    reg_class : extcall_reg_class;
    size : int;
        (* in bytes; note that the natural alignment of each component is equal
           to its size *)
    offset : int
        (* in bytes, relative to the start of the corresponding C struct (after
           [extcall_layout_of_product]) or to the start of the containing unit
           (after [extcall_units_for_product]) *)
  }

(* Lay out an unboxed product exactly as C would lay out the corresponding
   (possibly nested) struct. Returns the flattened components, labelled with
   their byte offsets from the start of the outermost struct, together with that
   struct's total size. Void components correspond to no C struct members and
   contribute nothing. *)
let extcall_layout_of_product apply components =
  (* Note that all alignments here are powers of two (see
     [extcall_reg_class_and_size]), as [Misc.align] requires. *)
  (* Returns the leaf components in reverse order, with offsets relative to the
     start of the struct corresponding to [components], together with that
     struct's size (before rounding) and its alignment. *)
  let rec layout_members : type uc.
      extcall_component list ->
      int ->
      int ->
      uc Flambda_arity.Component_for_creation.t list ->
      extcall_component list * int * int =
   fun rev_acc ofs align components ->
    match components with
    | [] -> rev_acc, ofs, align
    | Singleton kind :: components ->
      let reg_class, size = extcall_reg_class_and_size apply kind in
      let ofs = Misc.align ofs size in
      layout_members
        ({ kind; reg_class; size; offset = ofs } :: rev_acc)
        (ofs + size) (max align size) components
    | Unboxed_product inner :: components ->
      (* A nested struct is aligned to the largest alignment of its members and
         its size is rounded up to that alignment. *)
      let rev_inner, inner_size, inner_align = layout_members [] 0 1 inner in
      let ofs = Misc.align ofs inner_align in
      let rev_acc =
        List.map
          (fun component -> { component with offset = component.offset + ofs })
          rev_inner
        @ rev_acc
      in
      let inner_size = Misc.align inner_size inner_align in
      layout_members rev_acc (ofs + inner_size) (max align inner_align)
        components
  in
  let rev_components, size, align = layout_members [] 0 1 components in
  List.rev rev_components, Misc.align size align

(* A unit is a part of an unboxed product that occupies exactly one register in
   the C calling convention: one 8-byte piece of the corresponding C struct (an
   "eightbyte" in x86-64 System V terminology), or one member of an arm64
   homogeneous floating-point/vector aggregate. The [offset]s of the components
   of a unit are relative to the start of the unit. *)
type extcall_unit =
  { unit_class : extcall_reg_class;
        (* the class of register that the C ABI uses for the unit *)
    unit_components : extcall_component list (* always non-empty *)
  }

let extcall_unit_reg_counts units =
  List.fold_left
    (fun (num_int, num_float) { unit_class; unit_components = _ } ->
      match unit_class with
      | Int_regs -> num_int + 1, num_float
      | Float_regs -> num_int, num_float + 1)
    (0, 0) units

(* A homogeneous floating-point (or short vector) aggregate, in arm64
   terminology: at most four components, all of the same kind, that kind being a
   floating-point or 128-bit vector kind. (Within class [Float_regs] the size of
   a component uniquely determines its kind, so it suffices to compare
   sizes.) *)
let is_arm64_homogeneous_float_aggregate components =
  List.compare_length_with components 4 <= 0
  && List.for_all
       (fun { reg_class; size; _ } ->
         match reg_class with Float_regs -> size <= 16 | Int_regs -> false)
       components
  &&
  match components with
  | [] -> false
  | { size = first_size; _ } :: rest ->
    List.for_all (fun { size; _ } -> size = first_size) rest

(* Group laid-out components into the 8-byte pieces of the struct that they
   inhabit, rebasing their offsets to be relative to the relevant piece. (Since
   every component is naturally aligned, no component of size at most 8
   straddles a piece boundary. A vector component would straddle pieces, but
   cannot occur here: a vector plus any other component forms a struct larger
   than 16 bytes, which will already have been rejected.) *)
let extcall_group_into_pieces apply components =
  List.iter
    (fun { kind; size; _ } ->
      if size > 8
      then
        Misc.fatal_errorf
          "Vector components of unboxed products cannot be passed to or \
           returned from external C calls except within homogeneous aggregates \
           on arm64 (component of kind %a):@ %a"
          K.With_subkind.print kind Apply.print apply)
    components;
  let rec group components =
    match components with
    | [] -> []
    | { offset; _ } :: _ ->
      let piece = offset / 8 in
      let in_piece, rest =
        (* The components are sorted by increasing offset, so this partition
           splits off a prefix. *)
        List.partition (fun { offset; _ } -> offset / 8 = piece) components
      in
      List.map
        (fun component ->
          { component with offset = component.offset - (8 * piece) })
        in_piece
      :: group rest
  in
  group components

(* Determine the units of the given unboxed product, whose laid-out flattened
   components and total size are given. Fatal error if the relevant C ABI would
   pass the product in memory (see the comment at the top of this file). *)
let extcall_units_for_product apply abi ~what components ~size =
  match abi with
  | Aarch64 ->
    if is_arm64_homogeneous_float_aggregate components
    then
      List.map
        (fun component ->
          { unit_class = Float_regs;
            unit_components = [{ component with offset = 0 }]
          })
        components
    else if size <= 16
    then
      (* Composites of at most 16 bytes that are not homogeneous
         floating-point/vector aggregates are passed and returned in one or two
         integer registers, each holding the corresponding doubleword of the
         struct's in-memory representation. This includes any floating-point
         components, whose bit patterns thus move through integer registers. *)
      List.map
        (fun unit_components -> { unit_class = Int_regs; unit_components })
        (extcall_group_into_pieces apply components)
    else
      Misc.fatal_errorf
        "Cannot compile unboxed product %s of external C call: the \
         corresponding C struct is of size %d bytes and is not a homogeneous \
         floating-point/vector aggregate of at most four members, so the arm64 \
         ABIs pass it in memory; this cannot currently be compiled (see \
         comments in to_cmm_extcall.ml):@ %a"
        what size Apply.print apply
  | Amd64_system_v ->
    if size <= 16
    then
      List.map
        (fun unit_components ->
          let unit_class =
            (* An eightbyte is passed in an SSE register iff all of the
               components within it are floating-point. *)
            if
              List.for_all
                (fun { reg_class; _ } ->
                  match reg_class with Float_regs -> true | Int_regs -> false)
                unit_components
            then Float_regs
            else Int_regs
          in
          { unit_class; unit_components })
        (extcall_group_into_pieces apply components)
    else
      Misc.fatal_errorf
        "Cannot compile unboxed product %s of external C call: the \
         corresponding C struct is of size %d bytes, which the x86-64 System V \
         ABI passes in memory; this cannot currently be compiled (see comments \
         in to_cmm_extcall.ml):@ %a"
        what size Apply.print apply

(* The bit pattern of a component, as an untagged integer in the low [8 * size]
   bits of a word (the bits above being unspecified). *)
let extcall_component_bits apply ~dbg component cmm =
  match K.With_subkind.kind component.kind with
  | Naked_number
      ( Naked_immediate | Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64
      | Naked_nativeint | Naked_mask ) ->
    cmm
  | Naked_number Naked_float -> C.float_as_int64 ~dbg cmm
  | Naked_number Naked_float32 ->
    Cmm.Cop (Cmm.Creinterpret_cast Cmm.Int32_of_float32, [cmm], dbg)
  | Value ->
    (* Values are always word-sized, hence never share their unit with another
       component, and so are passed directly rather than being bit-packed. *)
    Misc.fatal_errorf "Value components are never bit-packed:@ %a" Apply.print
      apply
  | Naked_number (Naked_vec128 | Naked_vec256 | Naked_vec512)
  | Region | Rec_info ->
    Misc.fatal_errorf "Components of kind %a are never bit-packed:@ %a"
      K.With_subkind.print component.kind Apply.print apply

(* Pack the given components, whose offsets are relative to a single 8-byte
   unit, into one word, as if the relevant part of the C struct had been loaded
   into an integer register. *)
let extcall_pack_int_bits apply ~dbg components_with_args =
  let bits_of (component, arg) =
    let bits = extcall_component_bits apply ~dbg component arg in
    let bits =
      (* No mask is needed when the component reaches the top of the unit: the
         left shift below (or the width of the unit itself) discards the
         unspecified bits above the component. *)
      if component.offset + component.size >= 8
      then bits
      else C.zero_extend ~bits:(8 * component.size) ~dbg bits
    in
    if component.offset = 0
    then bits
    else C.lsl_int bits (Cmm.Cconst_int (8 * component.offset, dbg)) dbg
  in
  match components_with_args with
  | [] ->
    Misc.fatal_errorf "No components to pack for external call:@ %a" Apply.print
      apply
  | first :: rest ->
    List.fold_left
      (fun acc component_with_arg ->
        C.or_int acc (bits_of component_with_arg) dbg)
      (bits_of first) rest

(* Build the actual Cmm argument for one unit, with its [exttype]. *)
let extcall_pack_unit apply ~dbg (unit : extcall_unit) components_with_args :
    Cmm.expression * Cmm.exttype =
  match unit.unit_class, components_with_args with
  | _, [] ->
    Misc.fatal_errorf "External call argument unit has no components:@ %a"
      Apply.print apply
  | Float_regs, [(component, arg)] ->
    arg, C.exttype_of_kind (K.With_subkind.kind component.kind)
  | Float_regs, (_ :: _ :: _ as components_with_args) ->
    (* Multiple float32 components packed into a single SSE register (x86-64).
       The bit pattern is conveyed as a double. *)
    ( C.int64_as_float ~dbg
        (extcall_pack_int_bits apply ~dbg components_with_args),
      Cmm.XFloat )
  | Int_regs, [(component, arg)] when component.offset = 0 -> (
    match component.reg_class with
    | Int_regs -> arg, C.exttype_of_kind (K.With_subkind.kind component.kind)
    | Float_regs -> (
      (* A floating-point component passed in an integer register (arm64
         composites that are not homogeneous aggregates). *)
      let bits = extcall_component_bits apply ~dbg component arg in
      match component.size with
      | 8 -> bits, Cmm.XInt64
      | 4 -> bits, Cmm.XInt32
      | _ ->
        Misc.fatal_errorf
          "Unexpected floating-point component size %d in integer unit:@ %a"
          component.size Apply.print apply))
  | Int_regs, components_with_args ->
    extcall_pack_int_bits apply ~dbg components_with_args, Cmm.XInt64

(* The Cmm machtype in which the value of a unit of an external call's return
   value is received. *)
let extcall_unit_machtype apply { unit_class; unit_components } =
  match unit_class, unit_components with
  | _, [] ->
    Misc.fatal_errorf "External call return unit has no components:@ %a"
      Apply.print apply
  | Float_regs, [component] -> C.machtype_of_kind component.kind
  | Float_regs, _ :: _ :: _ ->
    (* Multiple float32 components packed into a single SSE register (x86-64);
       the bit pattern is conveyed as a double. *)
    Cmm.typ_float
  | Int_regs, [component] ->
    if K.equal (K.With_subkind.kind component.kind) K.value
    then Cmm.typ_val
    else Cmm.typ_int
  | Int_regs, _ :: _ :: _ -> Cmm.typ_int

(* Extract the value of one component of an external call's return value from
   the value of its unit. [sign_extend_direct] is applied to components that
   arrive in a register of the correct class without any packing, to effect the
   usual sign extension of small integer results. *)
let extcall_unpack_result_component apply ~dbg ~sign_extend_direct
    (unit : extcall_unit) unit_value (component : extcall_component) =
  let shifted value =
    if component.offset = 0
    then value
    else C.asr_int value (Cmm.Cconst_int (8 * component.offset, dbg)) dbg
  in
  let single =
    match unit.unit_components with [_] -> true | [] | _ :: _ :: _ -> false
  in
  match unit.unit_class, component.reg_class with
  | Float_regs, Float_regs ->
    if single
    then unit_value
    else
      (* Multiple float32 components packed into a single SSE register
         (x86-64). *)
      Cmm.Cop
        ( Cmm.Creinterpret_cast Cmm.Float32_of_int32,
          [shifted (C.float_as_int64 ~dbg unit_value)],
          dbg )
  | Int_regs, Float_regs -> (
    (* A floating-point component returned in an integer register (arm64
       composites that are not homogeneous aggregates). *)
    match component.size with
    | 8 -> C.int64_as_float ~dbg unit_value
    | 4 ->
      Cmm.Cop
        (Cmm.Creinterpret_cast Cmm.Float32_of_int32, [shifted unit_value], dbg)
    | _ ->
      Misc.fatal_errorf
        "Unexpected floating-point component size %d in integer unit:@ %a"
        component.size Apply.print apply)
  | Int_regs, Int_regs ->
    if single && component.offset = 0
    then sign_extend_direct component.kind unit_value
    else if component.size >= 8
    then shifted unit_value
    else C.sign_extend ~bits:(8 * component.size) ~dbg (shifted unit_value)
  | Float_regs, Int_regs ->
    Misc.fatal_errorf
      "Integer component of kind %a cannot inhabit a floating-point unit:@ %a"
      K.With_subkind.print component.kind Apply.print apply

(* Prepare the arguments of an external call: emit any packing and
   reinterpret-cast operations required for unboxed product arguments (see the
   comment at the top of this file) and compute the [exttype]s describing what
   is actually passed. [params] gives the argument arity components, one per
   parameter, and [args] the flat list of unarized Cmm arguments.

   Remaining available registers are tracked so that any unboxed product that
   the C ABI would demote to memory, for lack of registers, can be rejected. *)
let extcall_args_with_types apply ~dbg
    (params : [`Complex] Flambda_arity.Component_for_creation.t list) args =
  (* Fast path: when no parameter is an unboxed product, no laying-out is needed
     and [args] passes through unchanged. *)
  let rec exttypes_of_scalar_params rev_exttypes
      (params : [`Complex] Flambda_arity.Component_for_creation.t list) =
    match params with
    | [] -> Some (List.rev rev_exttypes)
    | Singleton kind :: params ->
      exttypes_of_scalar_params
        (C.exttype_of_kind (K.With_subkind.kind kind) :: rev_exttypes)
        params
    | Unboxed_product _ :: _ -> None
  in
  match exttypes_of_scalar_params [] params with
  | Some exttypes -> args, exttypes
  | None ->
    let laid_out_params =
      List.map (fun param -> extcall_layout_of_product apply [param]) params
    in
    let involves_unboxed_products =
      (* Unboxed products all of whose components are void correspond to no C
         arguments at all, so require no particular ABI support. *)
      List.exists2
        (fun (param : [`Complex] Flambda_arity.Component_for_creation.t)
             (components, _size) ->
          match param, components with
          | Unboxed_product _, _ :: _ -> true
          | (Unboxed_product _ | Singleton _), _ -> false)
        params laid_out_params
    in
    if not involves_unboxed_products
    then
      ( args,
        List.concat_map
          (fun (components, _size) ->
            List.map
              (fun { kind; _ } -> C.exttype_of_kind (K.With_subkind.kind kind))
              components)
          laid_out_params )
    else begin
      (* The ABI check must happen even if every unboxed product involved
         occupies at most one register: the ABIs supported here are exactly
         those that pass such products as if they were scalars. (The Win64 ABI,
         for example, does not: it distinguishes a struct containing a single
         double from a double.) *)
      let abi = extcall_abi_supporting_unboxed_products apply in
      let init_free_int_regs, init_free_float_regs =
        match abi with
        | Amd64_system_v ->
          (* rdi, rsi, rdx, rcx, r8, r9; xmm0-7 *)
          6, 8
        | Aarch64 ->
          (* x0-x7; d0-d7 *)
          8, 8
      in
      (* [args] contains one argument per laid-out component, in order. (The
         number of unarized components per parameter always equals the number of
         laid-out components, both being the non-void leaves in order.) *)
      let args_per_param =
        Flambda_arity.group_by_parameter (Flambda_arity.create params) args
      in
      let (_ : int * int), rev_args_with_types =
        List.fold_left2
          (fun ((free_int_regs, free_float_regs), rev_args_with_types)
               (components, size) param_args ->
            match components, param_args with
            | [], [] ->
              (* Void arguments take no registers. *)
              (free_int_regs, free_float_regs), rev_args_with_types
            | [component], [arg] ->
              (* A single component is passed exactly as if it were a scalar:
                 for register assignment purposes the C ABIs in question do not
                 distinguish between a struct containing a single scalar member
                 and that scalar itself. (Scalars may be passed on the stack,
                 unlike the components of unboxed products; see above.) *)
              let free_int_regs, free_float_regs =
                match component.reg_class with
                | Int_regs -> max 0 (free_int_regs - 1), free_float_regs
                | Float_regs -> free_int_regs, max 0 (free_float_regs - 1)
              in
              ( (free_int_regs, free_float_regs),
                (arg, C.exttype_of_kind (K.With_subkind.kind component.kind))
                :: rev_args_with_types )
            | (_ :: _ :: _ as components), param_args ->
              let units =
                extcall_units_for_product apply abi ~what:"argument" components
                  ~size
              in
              let num_int_units, num_float_units =
                extcall_unit_reg_counts units
              in
              if
                num_int_units > free_int_regs
                || num_float_units > free_float_regs
              then
                Misc.fatal_errorf
                  "Cannot compile unboxed product argument of external C call: \
                   too few argument registers remain for the corresponding C \
                   struct, which would therefore be passed in memory; this \
                   cannot currently be compiled (see comments in \
                   to_cmm_extcall.ml):@ %a"
                  Apply.print apply;
              let rec build units param_args rev_acc =
                match units with
                | [] -> (
                  match param_args with
                  | [] -> rev_acc
                  | _ :: _ ->
                    Misc.fatal_errorf
                      "Argument/unit mismatch for external call:@ %a"
                      Apply.print apply)
                | unit :: units ->
                  let components_with_args, param_args =
                    Misc.Stdlib.List.map2_prefix
                      (fun component arg -> component, arg)
                      unit.unit_components param_args
                  in
                  let arg, exttype =
                    extcall_pack_unit apply ~dbg unit components_with_args
                  in
                  build units param_args ((arg, exttype) :: rev_acc)
              in
              ( ( free_int_regs - num_int_units,
                  free_float_regs - num_float_units ),
                build units param_args rev_args_with_types )
            | [], _ :: _ | [_], ([] | _ :: _ :: _) ->
              Misc.fatal_errorf "Argument/arity mismatch for external call:@ %a"
                Apply.print apply)
          ((init_free_int_regs, init_free_float_regs), [])
          laid_out_params args_per_param
      in
      List.split (List.rev rev_args_with_types)
    end

(* Determine the units for an unboxed product return value (which always has at
   least two unarized components here).

   No register availability checks are needed for returns: any product accepted
   by [extcall_units_for_product] fits within the result registers assigned by
   [Proc.loc_external_results]. (There can be at most two integer units, since
   products not passed in memory are at most 16 bytes, matching rax/rdx
   respectively x0/x1; homogeneous floating-point/vector aggregates on arm64
   have at most four members, matching d0-d3; and at most two SSE units can
   arise on x86-64, again by the size limit, matching xmm0/xmm1.) *)
let extcall_return_units apply return_arity =
  let abi = extcall_abi_supporting_unboxed_products apply in
  let component =
    match Flambda_arity.components return_arity with
    | [component] -> component
    | components ->
      (* Closure conversion always produces a single component for unboxed
         product returns, but be liberal here (e.g. for terms built via the
         Flambda parser): treat multiple top-level components as a flat
         product. *)
      Flambda_arity.Component_for_creation.Unboxed_product components
  in
  let components, size = extcall_layout_of_product apply [component] in
  extcall_units_for_product apply abi ~what:"return value" components ~size

let translate_external_call env res ~free_vars apply ~callee_simple ~args
    ~return_arity ~return_ty dbg ~needs_caml_c_call ~is_c_builtin ~effects
    ~coeffects =
  fail_if_probe apply;
  let callee =
    match callee_simple with
    | None ->
      Misc.fatal_errorf
        "Application expression did not provide callee for C call:@ %a"
        Apply.print apply
    | Some callee_simple -> (
      match Simple.must_be_symbol callee_simple with
      | Some (sym, _) -> (To_cmm_result.symbol res sym).sym_name
      | None ->
        Misc.fatal_errorf "Expected a function symbol instead of:@ %a"
          Simple.print callee_simple)
  in
  let returns = Apply.returns apply in
  (* Note that void has been erased in return arities by this point. (Unlike
     parameter arities; see the phantom type parameters on the arity fields in
     [Apply_expr.t], for example.) *)
  let return_kinds = Flambda_arity.unarize return_arity in
  let return_units =
    match return_kinds with
    | [] -> None
    | [_] ->
      (* An unboxed product return occupying at most one register is returned
         exactly as the corresponding scalar would be on the ABIs supported
         here, but not universally (the Win64 ABI, for example, distinguishes a
         struct containing a single double from a double), so the ABI must still
         be checked. *)
      let return_is_product =
        List.exists
          (fun (component : _ Flambda_arity.Component_for_creation.t) ->
            match component with
            | Unboxed_product _ -> true
            | Singleton _ -> false)
          (Flambda_arity.components return_arity)
      in
      if return_is_product
      then ignore (extcall_abi_supporting_unboxed_products apply : extcall_abi);
      None
    | _ :: _ :: _ -> Some (extcall_return_units apply return_arity)
  in
  let return_ty, unit_tys =
    match return_units with
    | None ->
      (* [unit_tys] is only used when [return_units] is [Some]. *)
      C.Extended_machtype.to_machtype return_ty, [||]
    | Some units ->
      let unit_tys = List.map (extcall_unit_machtype apply) units in
      Array.concat unit_tys, Array.of_list unit_tys
  in
  let args, ty_args =
    extcall_args_with_types apply ~dbg
      (Flambda_arity.components (Apply.args_arity apply))
      args
  in
  let effects = To_cmm_effects.transl_c_call_effects effects in
  let coeffects = To_cmm_effects.transl_c_call_coeffects coeffects in
  let { extcall; builtin_sign_extends } : Cmm_builtins.t =
    C.extcall ~dbg ~alloc:needs_caml_c_call ~is_c_builtin ~effects ~coeffects
      ~returns ~ty_args callee return_ty args
  in
  (* Returned small integer values need to be sign-extended because it's not
     clear whether C code that returns a small integer returns one that is sign
     extended or not. There is no need to wrap other return arities. *)
  let maybe_sign_extend kind dbg cmm =
    if builtin_sign_extends
    then cmm
    else
      match Flambda_kind.With_subkind.kind kind with
      | Naked_number Naked_int8 -> C.sign_extend ~bits:8 ~dbg cmm
      | Naked_number Naked_int16 -> C.sign_extend ~bits:16 ~dbg cmm
      | Naked_number Naked_int32 -> C.sign_extend ~bits:32 ~dbg cmm
      | Naked_number
          ( Naked_float | Naked_immediate | Naked_int64 | Naked_nativeint
          | Naked_vec128 | Naked_vec256 | Naked_vec512 | Naked_mask
          | Naked_float32 )
      | Value | Rec_info | Region ->
        cmm
  in
  let wrap return_values =
    match return_kinds, return_units with
    | [], None ->
      (* CR mshinwell: this statement would seem to be wrong if we permit void
         returns from extcalls *)
      (* Extcalls of arity 0 are allowed (these never return). *)
      return_values
    | [kind], None -> maybe_sign_extend kind dbg return_values
    | _ :: _ :: _, Some units ->
      let unit_value =
        match units with
        | [_] ->
          (* A single unit is not wrapped in a [Ctuple]. *)
          fun _index -> return_values
        | [] | _ :: _ :: _ ->
          fun index ->
            C.tuple_field return_values ~component_tys:unit_tys index dbg
      in
      let components_in_order =
        List.concat
          (List.mapi
             (fun index unit ->
               List.map
                 (fun component -> index, unit, component)
                 unit.unit_components)
             units)
      in
      if List.compare_lengths components_in_order return_kinds <> 0
      then
        Misc.fatal_errorf
          "Return value components do not match return arity of external \
           call:@ %a"
          Apply.print apply;
      C.make_tuple
        (List.map
           (fun (index, unit, component) ->
             extcall_unpack_result_component apply ~dbg
               ~sign_extend_direct:(fun kind cmm ->
                 maybe_sign_extend kind dbg cmm)
               unit (unit_value index) component)
           components_in_order)
    | [], Some _ | [_], Some _ | _ :: _ :: _, None ->
      Misc.fatal_errorf "Inconsistent return units for external call:@ %a"
        Apply.print apply
  in
  let extcall_ident = Ident.create_local "extcall" in
  let extcall_var = Backend_var.With_provenance.create extcall_ident in
  let cmm =
    C.letin extcall_var ~defining_expr:extcall ~body:(wrap (Cvar extcall_ident))
  in
  cmm, free_vars, env, res, Ece.all
