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
module Nonempty_list = Misc.Nonempty_list

module C = struct
  include Cmm_helpers
  include Cmm_builtins
  include To_cmm_shared
end

(* An unboxed product passed to or returned from an external call follows the
   calling convention of the C struct whose members are the product's
   components, in order, with nested products as nested structs. ([@unpacked]
   products never reach here: closure conversion unarizes them into separate
   scalar arguments.)

   The backend assigns one register or stack slot to each element of a
   [Cextcall]'s argument and result descriptions and has no notion of
   aggregates. So this file lays each product out as C would ([Layout]), splits
   it into register-sized "units" following the ABI rules ([Unit.of_product]),
   and emits the bit-level packing and unpacking around the call ([Unit.pack],
   [Unit.unpack_component]). The backend then sees one scalar per unit.

   Supported ABIs: x86-64 System V (Linux and macOS) and arm64 (AAPCS64 and
   Apple's variant). Products that C passes in memory -- structs over 16 bytes
   (other than arm64 homogeneous aggregates), structs demoted to memory for lack
   of argument registers, and struct returns in memory -- are rejected with an
   [Error]. Supporting them requires the backend interface to change: the
   [Cmm.exttype]s given to [Proc.loc_external_arguments] describe units
   individually, so the backend can neither lay out an aggregate on the stack as
   one entity, nor pass by reference, nor leave registers unused when a whole
   aggregate does not fit; and memory returns need a stack temporary around the
   [Cextcall] plus a hidden address parameter (x8 on arm64, which the backend
   does not model).

   CR mshinwell: lift the memory-passing restrictions as described above.

   CR mshinwell: until then it would be better to reject the unsupported shapes
   at type-checking time; see the CR in [Primitive.Repr_check.check_c_stub].

   Since nesting affects C struct layout (alignment and size-rounding of nested
   structs), products are laid out following the structure recorded in the
   [`Complex] arities of the [Apply] expression. *)

(* Errors for declarations that are well-typed but cannot currently be compiled,
   reported at the location of the [Apply]. *)
module Error = struct
  type what =
    | Argument
    | Return_value

  type memory_reason =
    | Larger_than_16_bytes
    | Larger_than_16_bytes_and_not_homogeneous
    | Too_few_registers_remaining

  type t =
    | Unsupported_on_windows
    | Unsupported_architecture
    | Passed_in_memory of
        { what : what;
          size : int;
          reason : memory_reason
        }

  exception Error of t * Apply.t

  let raise t apply = Stdlib.raise (Error (t, apply))

  let callee_name apply =
    match Apply.callee apply with
    | Some callee -> (
      match Simple.must_be_symbol callee with
      | Some (sym, _) -> Symbol.linkage_name_as_string sym
      | None -> "<unknown>")
    | None -> "<unknown>"

  let print_what ppf what =
    Format_doc.pp_print_string ppf
      (match what with
      | Argument -> "argument"
      | Return_value -> "return value")

  let print_memory_reason ppf reason =
    Format_doc.pp_print_string ppf
      (match reason with
      | Larger_than_16_bytes -> "it is larger than 16 bytes"
      | Larger_than_16_bytes_and_not_homogeneous ->
        "it is larger than 16 bytes and not a homogeneous floating-point or \
         vector aggregate of at most four members"
      | Too_few_registers_remaining ->
        "too few argument registers remain for it")

  let report ppf (t, apply) =
    match t with
    | Unsupported_on_windows ->
      Format_doc.fprintf ppf
        "Unboxed products in external calls are not supported on Windows (call \
         to %s)."
        (callee_name apply)
    | Unsupported_architecture ->
      Format_doc.fprintf ppf
        "Unboxed products in external calls are only supported on x86-64 and \
         arm64 (call to %s)."
        (callee_name apply)
    | Passed_in_memory { what; size; reason } ->
      Format_doc.fprintf ppf
        "@[<hov>The unboxed product %a of the external call to %s cannot be \
         compiled:@ the corresponding C struct (%d bytes) would be passed in \
         memory, since %a.@ Only unboxed products that C passes entirely in \
         registers are currently supported.@]"
        print_what what (callee_name apply) size print_memory_reason reason

  let () =
    Location.register_error_of_exn (function
      | Error (t, apply) ->
        let loc = Debuginfo.to_location (Apply.dbg apply) in
        Some (Location.error_of_printer ~loc report (t, apply))
      | _ -> None)
end

module Reg_class = struct
  type t =
    | Int_regs
    | Float_regs
end

(* Counts of int and float registers to keep track of free argument
   registers. *)
module Reg_count = struct
  type t =
    { int_regs : int;
      float_regs : int
    }

  let of_classes classes =
    List.fold_left
      (fun t (reg_class : Reg_class.t) ->
        match reg_class with
        | Int_regs -> { t with int_regs = t.int_regs + 1 }
        | Float_regs -> { t with float_regs = t.float_regs + 1 })
      { int_regs = 0; float_regs = 0 }
      classes

  let fits t ~free =
    t.int_regs <= free.int_regs && t.float_regs <= free.float_regs

  let subtract t1 t2 =
    { int_regs = t1.int_regs - t2.int_regs;
      float_regs = t1.float_regs - t2.float_regs
    }

  let take_one_if_available t (reg_class : Reg_class.t) =
    match reg_class with
    | Int_regs -> { t with int_regs = max 0 (t.int_regs - 1) }
    | Float_regs -> { t with float_regs = max 0 (t.float_regs - 1) }
end

module Abi = struct
  type t =
    | Amd64_system_v
    | Aarch64

  (* This check is needed even when every product involved occupies at most one
     register: the ABIs supported here pass such products exactly as the
     corresponding scalar, but others do not (Win64, for example, distinguishes
     a struct containing a single double from a double). *)
  let for_unboxed_products apply =
    match Target_system.architecture () with
    | X86_64 ->
      if Target_system.is_windows ()
      then Error.raise Unsupported_on_windows apply
      else Amd64_system_v
    | AArch64 -> Aarch64
    | IA32 | ARM | POWER | Z | Riscv ->
      Error.raise Unsupported_architecture apply

  (* Numbers of integer and floating-point argument registers. *)
  let num_arg_regs : t -> Reg_count.t = function
    | Amd64_system_v ->
      (* rdi, rsi, rdx, rcx, r8, r9; xmm0-7 *)
      { int_regs = 6; float_regs = 8 }
    | Aarch64 ->
      (* x0-x7; d0-d7 *)
      { int_regs = 8; float_regs = 8 }
end

(* A non-void leaf of an unboxed product, i.e. a member of the corresponding
   flattened C struct. *)
module Component = struct
  type t =
    { kind : K.With_subkind.t;
      reg_class : Reg_class.t;
      size : int;  (** in bytes; also the natural alignment *)
      offset : int
          (** in bytes, from the start of the outermost struct (after
              [Layout.of_arity]) or of the containing unit (after
              [Unit.of_product]) *)
    }

  (* The register class and size of the C type that [kind] is passed as. *)
  let reg_class_and_size (kind : K.With_subkind.t) : Reg_class.t * int =
    match C.exttype_of_kind (K.With_subkind.kind kind) with
    | XInt | XInt64 -> Int_regs, 8
    | XInt32 -> Int_regs, 4
    | XInt16 -> Int_regs, 2
    | XInt8 -> Int_regs, 1
    | XFloat -> Float_regs, 8
    | XFloat32 -> Float_regs, 4
    | XVec128 -> Float_regs, 16
    | XVec256 -> Float_regs, 32
    | XVec512 -> Float_regs, 64

  let is_float t =
    match t.reg_class with Float_regs -> true | Int_regs -> false

  let exttype t = C.exttype_of_kind (K.With_subkind.kind t.kind)

  (* The bit pattern of a component, as an untagged integer in the low [8 *
     size] bits of a word (the bits above being unspecified). *)
  let bits ~dbg t cmm =
    match K.With_subkind.kind t.kind with
    | Naked_number
        ( Naked_immediate | Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64
        | Naked_nativeint | Naked_mask ) ->
      cmm
    | Naked_number Naked_float -> C.float_as_int64 ~dbg cmm
    | Naked_number Naked_float32 ->
      Cmm.Cop (Cmm.Creinterpret_cast Cmm.Int32_of_float32, [cmm], dbg)
    | Value ->
      (* Values are word-sized, hence never share a unit, and are passed
         directly. *)
      Misc.fatal_error "To_cmm_extcall.Component.bits: value component"
    | Naked_number (Naked_vec128 | Naked_vec256 | Naked_vec512)
    | Region | Rec_info ->
      Misc.fatal_errorf "To_cmm_extcall.Component.bits: component of kind %a"
        K.With_subkind.print t.kind

  (* Small integer components and unspecified upper bits.

     [Primitive.Repr_check.check_c_stub] rejects scalar bits8/bits16 C stub
     parameters (unless the stub is a builtin or the parameter is
     [@unsafe_unextended]) because some C callees rely on the caller having
     extended them: Apple's arm64 ABI requires it, and clang on x86-64 assumes
     it although the psABI leaves the bits unspecified. Components of unboxed
     products are exempt. They are passed as C struct members, and no C compiler
     relies on the bits of a register or stack slot beyond a struct member:

     - The ABIs leave them unspecified. x86-64 psABI, section 3.2.3: "the excess
     bits [...] are unspecified", "the consumer side of those values needs to
     extend them"; section 3.1.2: padding is undefined. AAPCS64, parameter
     passing rule C.12: "the contents of any unused parts of the registers are
     unspecified". Apple's requirement to extend is stated for scalars only
     ("any argument with fewer than 32 bits").

     - clang attaches the [signext]/[zeroext] attributes its backends rely on
     only to integral scalar types ([classifyArgumentType] in
     clang/lib/CodeGen/Targets/X86.cpp and, for Darwin, AArch64.cpp); aggregates
     are coerced to bare iN or i64 units and the callee extends their members
     itself.

     - GCC never relies on caller extension ([default_promote_function_mode]
     leaves argument modes unchanged on both targets).

     The code generated by clang 21 and GCC 15/16 was also inspected: callees
     extend struct members explicitly (movswq, sxth, sbfx).

     [Unit.pack_int_bits] masks components that share a unit in any case, so
     unspecified bits only reach C where a small component occupies a unit on
     its own. Returned small components are sign-extended after extraction, as
     for scalar results.

     One difference between a one-member struct and its member does need
     handling: the stack slot on Apple arm64. Scalars there occupy only their
     natural size (see [Proc.loc_external_arguments]), but a struct always gets
     an 8-byte slot aligned to 8 (clang coerces it to i64; GCC's Darwin port
     rounds up aggregates "except for homogeneous ones which are handled as the
     enclosed type"). The other supported ABIs always use 8-byte slots. So an
     integer unit holding a single narrow component is presented to the backend
     as [XInt64]; the value itself is passed unchanged. Homogeneous aggregates
     of a single floating-point or vector member are passed exactly like that
     member on every supported ABI. *)
  let exttype_when_alone t : Cmm.exttype =
    match t.reg_class with
    | Int_regs -> if t.size < 8 then Cmm.XInt64 else exttype t
    | Float_regs -> exttype t
end

(* The layout of the C struct corresponding to an unboxed product. *)
module Layout = struct
  (* A struct being laid out: its members so far (leaf components in reverse
     order, with offsets from the start of the struct), its size before
     rounding, and its alignment. *)
  type state =
    { rev_acc : Component.t list;
      size : int;
      align : int
    }

  type t =
    { components : Component.t list;
      size : int
    }

  let init_state = { rev_acc = []; size = 0; align = 1 }

  let singleton (state : state) kind =
    let reg_class, size = Component.reg_class_and_size kind in
    let offset = Misc.align state.size size in
    let component : Component.t = { kind; reg_class; size; offset } in
    { rev_acc = component :: state.rev_acc;
      size = offset + size;
      align = max state.align size
    }

  (* Add [inner], a laid-out member, after the members in [outer], treating it
     as a nested struct: it is aligned to the largest alignment of its members
     and its size is rounded up to that alignment. (A scalar member is a
     one-member struct, whose layout is the same as the scalar's.) *)
  let unboxed_product ~(outer : state) ~(inner : state) =
    let offset = Misc.align outer.size inner.align in
    let rev_acc =
      List.map
        (fun (component : Component.t) ->
          { component with offset = component.offset + offset })
        inner.rev_acc
      @ outer.rev_acc
    in
    { rev_acc;
      size = offset + Misc.align inner.size inner.align;
      align = max outer.align inner.align
    }

  let of_state { rev_acc; size; align } =
    { components = List.rev rev_acc; size = Misc.align size align }

  (* The flattened components of the struct corresponding to a product, with
     offsets from the start of the outermost struct, and that struct's total
     size. Void components correspond to no C struct members and contribute
     nothing. *)
  let of_arity arity =
    Flambda_arity.fold ~init:init_state ~singleton ~unboxed_product
      ~component:unboxed_product arity
    |> of_state

  (* Like [of_arity], but laying out each parameter as a struct of its own. *)
  let of_param_arities arity =
    Flambda_arity.fold_per_parameter ~init:init_state ~singleton
      ~unboxed_product arity
    |> List.map of_state
end

(* Returned small integer values need to be sign-extended because it's not clear
   whether C code that returns a small integer returns one that is sign extended
   or not. *)
let sign_extend_small_int_result ~builtin_sign_extends kind ~dbg cmm =
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

(* A unit is a part of an unboxed product that occupies exactly one register in
   the C calling convention: one 8-byte piece of the corresponding C struct (an
   x86-64 "eightbyte" or arm64 doubleword), or one member of an arm64
   homogeneous floating-point/vector aggregate. Component offsets are relative
   to the start of the unit.

   A [Float_regs] unit with more than one component is an x86-64 SSE eightbyte
   holding packed float32s; its bit pattern is conveyed to and from the backend
   as a double. A [Float_regs] component in an [Int_regs] unit arises on arm64,
   where composites that are not homogeneous aggregates go through integer
   registers. *)
module Unit = struct
  type t =
    { reg_class : Reg_class.t;  (** the register class the C ABI uses *)
      components : Component.t Nonempty_list.t
    }

  let reg_counts units =
    List.map (fun { reg_class; components = _ } -> reg_class) units
    |> Reg_count.of_classes

  (* At most four components, all of the same floating-point or 128-bit vector
     kind. (Within [Float_regs] the size determines the kind.) *)
  let is_arm64_homogeneous_aggregate (components : Component.t list) =
    List.compare_length_with components 4 <= 0
    && List.for_all
         (fun (component : Component.t) ->
           match component.reg_class with
           | Float_regs -> component.size <= 16
           | Int_regs -> false)
         components
    &&
    match components with
    | [] -> false
    | first :: rest ->
      List.for_all
        (fun (component : Component.t) -> component.size = first.size)
        rest

  (* Group laid-out components into the 8-byte pieces of the struct they
     inhabit, rebasing their offsets to the piece. Since every component is
     naturally aligned, no component of size at most 8 straddles a piece
     boundary; a vector would, but a vector plus any other component exceeds 16
     bytes and has already been rejected. *)
  let group_into_pieces (components : Component.t list) =
    List.iter
      (fun (component : Component.t) ->
        if component.size > 8
        then
          Misc.fatal_errorf
            "To_cmm_extcall.Unit.group_into_pieces: component of size %d (kind \
             %a) does not fit in a piece"
            component.size K.With_subkind.print component.kind)
      components;
    let rec group (components : Component.t list) =
      match components with
      | [] -> []
      | first :: others ->
        let piece = first.offset / 8 in
        let in_piece, rest =
          (* The components are sorted by increasing offset, so this partition
             splits off a prefix. *)
          List.partition
            (fun (component : Component.t) -> component.offset / 8 = piece)
            others
        in
        let rebase (component : Component.t) =
          { component with offset = component.offset - (8 * piece) }
        in
        (rebase first :: List.map rebase in_piece : _ Nonempty_list.t)
        :: group rest
    in
    group components

  (* The units of a laid-out product. Raises [Error] if the ABI would pass the
     product in memory. *)
  let of_product apply (abi : Abi.t) ~what ({ components; size } : Layout.t) =
    match abi with
    | Aarch64 ->
      if is_arm64_homogeneous_aggregate components
      then
        List.map
          (fun (component : Component.t) ->
            { reg_class = Float_regs;
              components = [{ component with offset = 0 }]
            })
          components
      else if size <= 16
      then
        (* Other composites of at most 16 bytes go in integer registers, one per
           doubleword of the in-memory representation, floating-point components
           included. *)
        List.map
          (fun components -> { reg_class = Int_regs; components })
          (group_into_pieces components)
      else
        Error.raise
          (Passed_in_memory
             { what; size; reason = Larger_than_16_bytes_and_not_homogeneous })
          apply
    | Amd64_system_v ->
      if size <= 16
      then
        List.map
          (fun components ->
            (* An eightbyte is passed in an SSE register iff all of its
               components are floating-point. *)
            let reg_class : Reg_class.t =
              if
                List.for_all Component.is_float
                  (Nonempty_list.to_list components)
              then Float_regs
              else Int_regs
            in
            { reg_class; components })
          (group_into_pieces components)
      else
        Error.raise
          (Passed_in_memory { what; size; reason = Larger_than_16_bytes })
          apply

  (* Pair the components of a unit with their arguments, one each. *)
  let with_args (t : t) args : (Component.t * Cmm.expression) Nonempty_list.t =
    let (component :: components) = t.components in
    match args with
    | arg :: args when List.compare_lengths components args = 0 ->
      (component, arg) :: List.combine components args
    | [] | _ :: _ ->
      Misc.fatal_errorf
        "To_cmm_extcall.Unit.with_args: %d components but %d arguments"
        (List.length components + 1)
        (List.length args)

  (* Pack components sharing an 8-byte unit into one word, as if that part of
     the C struct had been loaded into an integer register. *)
  let pack_int_bits ~dbg
      (components_with_args : (Component.t * Cmm.expression) Nonempty_list.t) =
    let bits_of ((component : Component.t), arg) =
      let bits = Component.bits ~dbg component arg in
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
    let (first :: rest) = components_with_args in
    List.fold_left
      (fun acc component_with_arg ->
        C.or_int acc (bits_of component_with_arg) dbg)
      (bits_of first) rest

  (* The Cmm argument for one unit, given the arguments for its components, with
     its [exttype]. *)
  let pack ~dbg (t : t) args : Cmm.expression * Cmm.exttype =
    let components_with_args = with_args t args in
    match t.reg_class, components_with_args with
    | Float_regs, [(component, arg)] -> arg, Component.exttype component
    | Float_regs, _ :: _ :: _ ->
      ( C.int64_as_float ~dbg (pack_int_bits ~dbg components_with_args),
        Cmm.XFloat )
    | Int_regs, [((component : Component.t), arg)] when component.offset = 0
      -> (
      match component.reg_class with
      | Int_regs -> arg, Component.exttype_when_alone component
      | Float_regs -> (
        let bits = Component.bits ~dbg component arg in
        match component.size with
        | 8 -> bits, Cmm.XInt64
        | 4 -> bits, Cmm.XInt32
        | _ ->
          Misc.fatal_errorf
            "To_cmm_extcall.Unit.pack: floating-point component of size %d in \
             an integer unit"
            component.size))
    | Int_regs, _ -> pack_int_bits ~dbg components_with_args, Cmm.XInt64

  (* The machtype in which the value of a unit of a return value is received. *)
  let machtype { reg_class; components } =
    match reg_class, components with
    | Float_regs, [component] -> C.machtype_of_kind component.kind
    | Float_regs, _ :: _ :: _ -> Cmm.typ_float
    | Int_regs, [component] -> (
      match component.reg_class with
      | Int_regs -> C.machtype_of_kind component.kind (* val / int / mask *)
      | Float_regs ->
        Cmm.typ_int (* float bits in an integer register (arm64) *))
    | Int_regs, _ :: _ :: _ -> Cmm.typ_int

  (* Extract one component of a return value from the value of its unit.
     Components that arrive in a register of the correct class without any
     packing get the usual sign extension of small integer results. *)
  let unpack_component ~dbg ~builtin_sign_extends (t : t) unit_value
      (component : Component.t) =
    let shifted value =
      if component.offset = 0
      then value
      else C.asr_int value (Cmm.Cconst_int (8 * component.offset, dbg)) dbg
    in
    let single = match t.components with [_] -> true | _ :: _ :: _ -> false in
    match t.reg_class, component.reg_class with
    | Float_regs, Float_regs ->
      if single
      then unit_value
      else if component.size <> 4
      then
        (* Only float32s share an SSE eightbyte. *)
        Misc.fatal_errorf
          "To_cmm_extcall.Unit.unpack_component: component of size %d shares a \
           floating-point unit"
          component.size
      else
        Cmm.Cop
          ( Cmm.Creinterpret_cast Cmm.Float32_of_int32,
            [shifted (C.float_as_int64 ~dbg unit_value)],
            dbg )
    | Int_regs, Float_regs -> (
      match component.size with
      | 8 -> C.int64_as_float ~dbg unit_value
      | 4 ->
        Cmm.Cop
          (Cmm.Creinterpret_cast Cmm.Float32_of_int32, [shifted unit_value], dbg)
      | _ ->
        Misc.fatal_errorf
          "To_cmm_extcall.Unit.unpack_component: floating-point component of \
           size %d in an integer unit"
          component.size)
    | Int_regs, Int_regs ->
      if single && component.offset = 0
      then
        sign_extend_small_int_result ~builtin_sign_extends component.kind ~dbg
          unit_value
      else if component.size >= 8
      then shifted unit_value
      else C.sign_extend ~bits:(8 * component.size) ~dbg (shifted unit_value)
    | Float_regs, Int_regs ->
      Misc.fatal_errorf
        "To_cmm_extcall.Unit.unpack_component: integer component of kind %a in \
         a floating-point unit"
        K.With_subkind.print component.kind
end

(* Represents the head constructor of an [Flambda_arity.t] component. *)
module Arity_component_head = struct
  type t =
    | Singleton of K.With_subkind.t
    | Unboxed_product

  let of_arity arity =
    Flambda_arity.singleton_component_per_parameter arity
    |> List.map (function
      | Some kind -> Singleton kind
      | None -> Unboxed_product)
end

(* Translation of the arguments of an external call. *)
module Args = struct
  type param_head = Arity_component_head.t

  (* [Some exttypes] when no parameter is an unboxed product, in which case no
     laying-out is needed and the arguments pass through unchanged. *)
  let exttypes_if_all_scalar params =
    params
    |> Misc.Stdlib.List.map_option (fun (param : param_head) ->
        match param with Singleton kind -> Some kind | Unboxed_product -> None)
    |> Option.map
         (List.map (fun kind -> C.exttype_of_kind (K.With_subkind.kind kind)))

  (* Products all of whose components are void correspond to no C arguments at
     all, so require no particular ABI support. *)
  let involves_unboxed_products params laid_out_params =
    List.exists2
      (fun (param : param_head) ({ components; size = _ } : Layout.t) ->
        match param, components with
        | Unboxed_product, _ :: _ -> true
        | (Unboxed_product | Singleton _), _ -> false)
      params laid_out_params

  (* A product with a single non-void component is passed exactly as if it were
     that scalar: for register assignment the supported ABIs do not distinguish
     a struct containing a single scalar member from the member itself. (Such
     products may therefore be passed on the stack, unlike larger ones.) The
     stack slot may however differ; see [Component.exttype_when_alone]. *)
  let translate_single_component ~free_regs (param : param_head)
      (component : Component.t) arg =
    let free_regs =
      Reg_count.take_one_if_available free_regs component.reg_class
    in
    let exttype =
      match param with
      | Singleton _ -> Component.exttype component
      | Unboxed_product -> Component.exttype_when_alone component
    in
    free_regs, (arg, exttype)

  (* Raises [Error] if the product would be demoted to memory for lack of
     registers. Returns the remaining free registers and one argument per unit,
     in order. *)
  let translate_product apply ~dbg abi ~free_regs (layout : Layout.t) param_args
      =
    let units = Unit.of_product apply abi ~what:Argument layout in
    let unit_regs = Unit.reg_counts units in
    if not (Reg_count.fits unit_regs ~free:free_regs)
    then
      Error.raise
        (Passed_in_memory
           { what = Argument;
             size = layout.size;
             reason = Too_few_registers_remaining
           })
        apply;
    let rec build (units : Unit.t list) param_args rev_acc =
      match units with
      | [] -> (
        match param_args with
        | [] -> List.rev rev_acc
        | _ :: _ ->
          Misc.fatal_errorf
            "To_cmm_extcall.Args.translate_product: arguments left over after \
             the last unit:@ %a"
            Apply.print apply)
      | unit :: units ->
        let unit_args, param_args =
          Misc.Stdlib.List.split_at
            (List.length (Nonempty_list.to_list unit.components))
            param_args
        in
        build units param_args (Unit.pack ~dbg unit unit_args :: rev_acc)
    in
    Reg_count.subtract free_regs unit_regs, build units param_args []

  (* Returns the Cmm arguments and the [exttype]s describing what is actually
     passed. [params] has one component per parameter and [args] is the flat
     list of unarized Cmm arguments. Free registers are tracked across the
     argument list so that products the ABI would demote to memory can be
     rejected. *)
  let translate apply ~dbg args =
    let arity = Apply.args_arity apply in
    let param_heads = Arity_component_head.of_arity arity in
    match exttypes_if_all_scalar param_heads with
    | Some exttypes -> args, exttypes
    | None ->
      let laid_out_params = Layout.of_param_arities arity in
      if not (involves_unboxed_products param_heads laid_out_params)
      then
        ( args,
          List.concat_map
            (fun ({ components; size = _ } : Layout.t) ->
              List.map Component.exttype components)
            laid_out_params )
      else
        let abi = Abi.for_unboxed_products apply in
        (* [args] has one argument per laid-out component: both are the non-void
           leaves, in order. *)
        let args_per_param = Flambda_arity.group_by_parameter arity args in
        let (_ : Reg_count.t), rev_args_with_types =
          List.fold_left2
            (fun (free_regs, rev_acc) (param, (layout : Layout.t)) param_args ->
              match layout.components, param_args with
              | [], [] ->
                (* Void arguments take no registers. *)
                free_regs, rev_acc
              | [component], [arg] ->
                let free_regs, arg_with_type =
                  translate_single_component ~free_regs param component arg
                in
                free_regs, arg_with_type :: rev_acc
              | _ :: _ :: _, param_args ->
                let free_regs, args_with_types =
                  translate_product apply ~dbg abi ~free_regs layout param_args
                in
                free_regs, List.rev_append args_with_types rev_acc
              | [], _ :: _ | [_], ([] | _ :: _ :: _) ->
                Misc.fatal_errorf
                  "To_cmm_extcall.Args.translate: arguments do not match the \
                   arity:@ %a"
                  Apply.print apply)
            (Abi.num_arg_regs abi, [])
            (List.combine param_heads laid_out_params)
            args_per_param
        in
        List.split (List.rev rev_args_with_types)
end

(* Translation of the return value of an external call. *)
module Return = struct
  (* The units of a product return value with at least two unarized components.
     No register availability check is needed: anything [Unit.of_product]
     accepts fits the result registers assigned by [Proc.loc_external_results]
     (at most two integer units, matching rax/rdx and x0/x1; at most four arm64
     homogeneous aggregate members, matching d0-d3; at most two SSE units on
     x86-64, matching xmm0/xmm1). *)
  let units apply return_arity =
    let abi = Abi.for_unboxed_products apply in
    Unit.of_product apply abi ~what:Return_value (Layout.of_arity return_arity)

  let units_if_needed apply return_arity return_kinds =
    match return_kinds with
    | [] -> None
    | [_] ->
      let return_is_product =
        List.exists
          (fun (component : Arity_component_head.t) ->
            match component with
            | Unboxed_product -> true
            | Singleton _ -> false)
          (Arity_component_head.of_arity return_arity)
      in
      if return_is_product then ignore (Abi.for_unboxed_products apply : Abi.t);
      None
    | _ :: _ :: _ -> Some (units apply return_arity)

  (* Rebuild the unarized return values from the received units. *)
  let unpack apply ~dbg ~builtin_sign_extends (units : Unit.t list) ~unit_tys
      return_values return_kinds =
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
           (fun index (unit : Unit.t) ->
             List.map
               (fun component -> index, unit, component)
               (Nonempty_list.to_list unit.components))
           units)
    in
    if List.compare_lengths components_in_order return_kinds <> 0
    then
      Misc.fatal_errorf
        "To_cmm_extcall.Return.unpack: unit components do not match the return \
         arity:@ %a"
        Apply.print apply;
    C.make_tuple
      (List.map
         (fun (index, unit, component) ->
           Unit.unpack_component ~dbg ~builtin_sign_extends unit
             (unit_value index) component)
         components_in_order)
end

let callee_symbol_name res apply callee_simple =
  match callee_simple with
  | None ->
    Misc.fatal_errorf "To_cmm_extcall.callee_symbol_name: no callee:@ %a"
      Apply.print apply
  | Some callee_simple -> (
    match Simple.must_be_symbol callee_simple with
    | Some (sym, _) -> (To_cmm_result.symbol res sym).sym_name
    | None ->
      Misc.fatal_errorf
        "To_cmm_extcall.callee_symbol_name: callee is not a symbol: %a"
        Simple.print callee_simple)

let translate_external_call env res ~free_vars apply ~callee_simple ~args
    ~return_arity ~return_ty dbg ~needs_caml_c_call ~is_c_builtin ~effects
    ~coeffects =
  C.fail_if_apply_with_probe apply;
  let callee = callee_symbol_name res apply callee_simple in
  let returns = Apply.returns apply in
  (* Void has been erased in return arities by this point, unlike in parameter
     arities (see the phantom type parameters on the arity fields in
     [Apply_expr.t]). *)
  let return_kinds = Flambda_arity.unarize return_arity in
  let return_units = Return.units_if_needed apply return_arity return_kinds in
  let return_ty, unit_tys =
    match return_units with
    | None ->
      (* [unit_tys] is only used when [return_units] is [Some]. *)
      C.Extended_machtype.to_machtype return_ty, [||]
    | Some units ->
      let unit_tys = List.map Unit.machtype units in
      Array.concat unit_tys, Array.of_list unit_tys
  in
  let args, ty_args = Args.translate apply ~dbg args in
  let effects = To_cmm_effects.transl_c_call_effects effects in
  let coeffects = To_cmm_effects.transl_c_call_coeffects coeffects in
  let { extcall; builtin_sign_extends } : Cmm_builtins.t =
    C.extcall ~dbg ~alloc:needs_caml_c_call ~is_c_builtin ~effects ~coeffects
      ~returns ~ty_args callee return_ty args
  in
  let wrap return_values =
    match return_kinds, return_units with
    | [], None ->
      (* CR mshinwell: this statement would seem to be wrong if we permit void
         returns from extcalls *)
      (* Extcalls of arity 0 are allowed (these never return). *)
      return_values
    | [kind], None ->
      sign_extend_small_int_result ~builtin_sign_extends kind ~dbg return_values
    | _ :: _ :: _, Some units ->
      Return.unpack apply ~dbg ~builtin_sign_extends units ~unit_tys
        return_values return_kinds
    | [], Some _ | [_], Some _ | _ :: _ :: _, None ->
      Misc.fatal_errorf
        "To_cmm_extcall.translate_external_call: return units do not match the \
         return arity:@ %a"
        Apply.print apply
  in
  let extcall_ident = Ident.create_local "extcall" in
  let extcall_var = Backend_var.With_provenance.create extcall_ident in
  let cmm =
    C.letin extcall_var ~defining_expr:extcall ~body:(wrap (Cvar extcall_ident))
  in
  cmm, free_vars, env, res, Ece.all
