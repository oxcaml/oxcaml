(******************************************************************************
 *                                  OxCaml                                    *
 *                                Jane Street                                 *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

[@@@ocaml.warning "+a-30-40-41-42"]

module Apply = Flambda.Apply
module K = Flambda_kind
module Nonempty_list = Misc.Nonempty_list

module List = struct
  include List
  include Misc.Stdlib.List
end

(* An unboxed product passed to or returned from an external call follows the
   calling convention of the C struct whose members are the product's
   components, in order, with nested products as nested structs. ([@unpacked]
   products are unarized by closure conversion and never reach here.)

   The backend assigns one register or stack slot to each element of a
   [Cextcall]'s argument and result descriptions and has no notion of
   aggregates. So this file simulates the calling conventions manually by:

   - Laying each product out as C would in [Struct_layout]

   - Splitting it into [Unit.t]s occupying one register or stack slot each
   ([Unit.in_registers], [Unit.on_stack]), following the ABI rules

   - Simulating the ABI's register assignment and producing the [Cextcall]'s
   argument list ([Arg_slots]), with reordering + dummy arguments to match the
   backend's per-slot assignment.

   The bit-level packing of components into slots, and unpacking of return
   values, is left to [To_cmm_extcall].

   Supported ABIs: x86-64 System V (Linux and macOS) and arm64 (AAPCS64 and
   Apple's variant). We support all product shapes for which the ABI doesn't
   involve passing pointers. In particular:

   # x86-64:

   For our purposes, the ABI classifies structs of size > 16 bytes as MEMORY.
   These structs are always passed via the stack by value. Otherwise, the struct
   can be passed via registers, if there are enough registers remaining (stack
   otherwise). MEMORY-class structs are returned via a buffer the caller
   provides, so we don't support > 16 byte returns.

   # arm64:

   Arguments and returns must be <= 16 bytes, since unlike x86-64, "big"
   arguments are also passed via pointers. Additionally, homogeneous aggregates
   (HFA/HVAs), up to 4 floats/vectors of the same type, can be passed and
   returned in registers or on the stack whatever their size. *)

(* CR mshinwell: lift the by-reference restrictions as described above. Until
   then it would be better to reject the unsupported shapes at type-checking
   time; see the CR in [Primitive.Repr_check.check_c_stub]. *)

(* Errors for declarations that are well-typed but cannot currently be compiled,
   reported at the location of the [Apply]. *)
module Error = struct
  module What = struct
    type t =
      | Argument
      | Return_value
  end

  module Memory_reason = struct
    type t =
      | Larger_than_16_bytes
      | Larger_than_16_bytes_and_not_homogeneous

    let print ppf t =
      Format_doc.pp_print_string ppf
        (match t with
        | Larger_than_16_bytes -> "it is larger than 16 bytes"
        | Larger_than_16_bytes_and_not_homogeneous ->
          "it is larger than 16 bytes and not a homogeneous floating-point or \
           vector aggregate of at most four members")
  end

  type t =
    | Unsupported_on_windows
    | Unsupported_architecture
    | Passed_in_memory of
        { what : What.t;
          size : int;
          reason : Memory_reason.t
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
    | Passed_in_memory { what = Argument; size; reason } ->
      Format_doc.fprintf ppf
        "@[<hov>The unboxed product argument of the external call to %s cannot \
         be compiled:@ the corresponding C struct (%d bytes) would be passed \
         by reference, since %a.@ Only unboxed product arguments that C passes \
         by value are currently supported.@]"
        (callee_name apply) size Memory_reason.print reason
    | Passed_in_memory { what = Return_value; size; reason } ->
      Format_doc.fprintf ppf
        "@[<hov>The unboxed product return value of the external call to %s \
         cannot be compiled:@ the corresponding C struct (%d bytes) would be \
         returned in memory, since %a.@ Only unboxed products that C returns \
         in registers are currently supported.@]"
        (callee_name apply) size Memory_reason.print reason

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

  let equal t1 t2 =
    match t1, t2 with
    | Int_regs, Int_regs | Float_regs, Float_regs -> true
    | Int_regs, Float_regs | Float_regs, Int_regs -> false

  let is_float = function Int_regs -> false | Float_regs -> true

  let of_exttype (exttype : Cmm.exttype) : t =
    match exttype with
    | XInt | XInt8 | XInt16 | XInt32 | XInt64 | XDummy_int -> Int_regs
    | XFloat | XFloat32 | XVec128 | XVec256 | XVec512 | XDummy_float ->
      Float_regs
end

(* Counts of int and float registers to keep track of free argument
   registers. *)
module Reg_count = struct
  type t =
    { int_regs : int;
      float_regs : int
    }

  let zero = { int_regs = 0; float_regs = 0 }

  let get t (reg_class : Reg_class.t) =
    match reg_class with Int_regs -> t.int_regs | Float_regs -> t.float_regs

  let set t (reg_class : Reg_class.t) n =
    match reg_class with
    | Int_regs -> { t with int_regs = n }
    | Float_regs -> { t with float_regs = n }

  let of_classes classes =
    List.fold_left
      (fun t reg_class -> set t reg_class (get t reg_class + 1))
      zero classes

  let fits t ~free =
    t.int_regs <= free.int_regs && t.float_regs <= free.float_regs

  let subtract t1 t2 =
    { int_regs = t1.int_regs - t2.int_regs;
      float_regs = t1.float_regs - t2.float_regs
    }

  let has_free t reg_class = get t reg_class > 0

  let take_one t reg_class = set t reg_class (get t reg_class - 1)

  let exhaust t reg_class = set t reg_class 0
end

module Abi = struct
  type t =
    | Amd64_system_v
    | Aarch64

  let for_unboxed_products apply =
    match Target_system.architecture () with
    | X86_64 ->
      if Target_system.is_windows ()
      then
        Error.raise Unsupported_on_windows apply
        (* Win64 would need some special treatment. For example, it
           distinguishes a struct containing a single double from a double. *)
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

  (* What register class the ABI assigns to part of a struct with component
     types [reg_classes] *)
  let reg_class_of_components t reg_classes : Reg_class.t =
    match t with
    | Amd64_system_v ->
      (* INTEGER class overrides SSE *)
      if List.for_all Reg_class.is_float reg_classes
      then Float_regs
      else Int_regs
    | Aarch64 ->
      (* All structs (except homogeneous aggregates!) passed via registers use
         integer registers *)
      Int_regs
end

(* A non-void leaf of an unboxed product, i.e. a member of the corresponding
   flattened C struct. *)
module Component = struct
  type t =
    { kind : K.With_subkind.t;
      reg_class : Reg_class.t;
      size : int;  (** In bytes; also the natural alignment *)
      offset : int
          (** In bytes, from the start of the outermost struct (in a
              [Struct_layout.t]) or of the containing unit (in a [Unit.t] /
              [Reg_unit.t]) *)
    }

  let reg_class_and_size (kind : K.With_subkind.t) : Reg_class.t * int =
    match To_cmm_shared.exttype_of_kind (K.With_subkind.kind kind) with
    | XInt | XInt64 -> Int_regs, 8
    | XInt32 -> Int_regs, 4
    | XInt16 -> Int_regs, 2
    | XInt8 -> Int_regs, 1
    | XFloat -> Float_regs, 8
    | XFloat32 -> Float_regs, 4
    | XVec128 -> Float_regs, 16
    | XVec256 -> Float_regs, 32
    | XVec512 -> Float_regs, 64
    | XDummy_int | XDummy_float ->
      Misc.fatal_error "Extcall_abi.Component.reg_class_and_size: dummy"

  let exttype t = To_cmm_shared.exttype_of_kind (K.With_subkind.kind t.kind)

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

     [To_cmm_extcall.Pack.int_bits] masks components that share a unit in any
     case, so unspecified bits only reach C where a small component occupies a
     unit on its own. Returned small components are sign-extended after
     extraction, as for scalar results.

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
    | Int_regs -> if t.size < 8 then XInt64 else exttype t
    | Float_regs -> exttype t
end

(* The flattened layout of the C struct corresponding to an unboxed product.
   Nested products are laid out as nested structs, following the structure of
   the [`Complex] arities of the [Apply]. Void components contribute nothing. *)
module Struct_layout = struct
  type t =
    { components : Component.t list; (* Offsets from the start of the struct *)
      size : int; (* Rounded up to a multiple of [align] *)
      align : int
    }

  (* A struct being laid out as a fold over the arity. *)
  module Acc = struct
    type t =
      { rev_components : Component.t list;
            (** Leaf components so far, with offsets from the start of the
                struct *)
        size : int;  (** In bytes; before rounding *)
        align : int
      }

    let init = { rev_components = []; size = 0; align = 1 }

    let singleton (t : t) kind =
      let reg_class, size = Component.reg_class_and_size kind in
      let offset = Misc.align t.size size in
      let component : Component.t = { kind; reg_class; size; offset } in
      { rev_components = component :: t.rev_components;
        size = offset + size;
        align = max t.align size
      }

    (* Add [inner], a laid-out member, after the members in [outer]. *)
    let unboxed_product ~(outer : t) ~(inner : t) =
      let offset = Misc.align outer.size inner.align in
      let rev_components =
        List.map
          (fun (component : Component.t) ->
            { component with offset = component.offset + offset })
          inner.rev_components
        @ outer.rev_components
      in
      { rev_components;
        size = offset + Misc.align inner.size inner.align;
        align = max outer.align inner.align
      }
  end

  let of_acc ({ rev_components; size; align } : Acc.t) =
    { components = List.rev rev_components;
      size = Misc.align size align;
      align
    }

  let of_arity arity =
    Flambda_arity.fold ~init:Acc.init ~singleton:Acc.singleton
      ~unboxed_product:Acc.unboxed_product ~component:Acc.unboxed_product arity
    |> of_acc

  (* Like [of_arity], but laying out each parameter as a struct of its own. *)
  let of_arity_per_argument arity =
    Flambda_arity.fold_per_parameter ~init:Acc.init ~singleton:Acc.singleton
      ~unboxed_product:Acc.unboxed_product arity
    |> List.map of_acc

  (* At most four components, all of the same float or vector kind. *)
  let is_arm64_homogeneous_aggregate { components; size = _; align = _ } =
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
end

(* A unit is part of a struct that occupies one register or one stack slot, with
   the components inside it (offsets from the start of the unit). It is one of

   * an 8-byte part of the struct's memory image

   * a vector member

   * a member of an arm64 homogeneous aggregate

   * padding (no components) *)
module Unit = struct
  type t =
    { components : Component.t list;
      size : int  (** In bytes *)
    }

  let num_components t = List.length t.components

  (* The struct's memory image in units, with vectors as units of their own.

     Note that only padding inside and at the end of the struct is handled here.
     Padding before the struct is inserted by [Stack_slots] (x86-64 only). *)
  let memory_image ({ components; size; align = _ } : Struct_layout.t) : t list
      =
    let is_vector (component : Component.t) = component.size > 8 in
    let inside_vector offset =
      List.exists
        (fun (component : Component.t) ->
          is_vector component && component.offset < offset
          && offset < component.offset + component.size)
        components
    in
    let unit offset : t =
      let inside =
        List.filter
          (fun (component : Component.t) ->
            offset <= component.offset && component.offset < offset + 8)
          components
        |> List.map (fun (component : Component.t) ->
            { component with offset = component.offset - offset })
      in
      match inside with
      | [component] when is_vector component ->
        { components = [component]; size = component.size }
      | inside ->
        List.iter
          (fun (component : Component.t) ->
            if is_vector component
            then
              Misc.fatal_errorf
                "Extcall_abi.Unit.memory_image: vector component of kind %a \
                 shares a unit"
                K.With_subkind.print component.kind)
          inside;
        { components = inside; size = 8 }
    in
    let num_units = (size + 7) / 8 in
    List.init num_units (fun i -> 8 * i)
    |> List.filter (fun offset -> not (inside_vector offset))
    |> List.map unit

  (* One unit per member. *)
  let members ({ components; size = _; align = _ } : Struct_layout.t) : t list =
    List.map
      (fun (component : Component.t) ->
        { components = [{ component with offset = 0 }]; size = component.size })
      components

  let in_registers (abi : Abi.t) (layout : Struct_layout.t) : t list =
    match abi with
    | Aarch64 when Struct_layout.is_arm64_homogeneous_aggregate layout ->
      (* arm64 passes homogeneous aggregates one member per register (AAPCS64
         rule C.2). *)
      members layout
    | Aarch64 | Amd64_system_v -> memory_image layout

  let on_stack (abi : Abi.t) (layout : Struct_layout.t) : t list =
    match abi with
    | Aarch64
      when Struct_layout.is_arm64_homogeneous_aggregate layout
           && Target_system.is_macos () ->
      (* Apple's arm64 ABI gives each member of a float32 homogeneous aggregate
         a 4-byte slot, like a float32 scalar. *)
      members layout
    | Aarch64 | Amd64_system_v -> memory_image layout

  let single_component { components; size } =
    match components with
    | [component] when component.size = size -> Some component
    | [] | [_] | _ :: _ :: _ -> None

  let exttype t : Cmm.exttype =
    match single_component t with
    | Some component -> Component.exttype component
    | None -> XInt64
end

(* A register unit is a [Unit.t] that the C ABI passes in one register (an
   x86-64 "eightbyte", an arm64 doubleword, or a member of an arm64 homogeneous
   floating-point/vector aggregate), with its register class. *)
module Reg_unit = struct
  type t =
    { reg_class : Reg_class.t;  (** Used by the C ABI. *)
      components : Component.t Nonempty_list.t
          (** Offsets from the start of the unit. *)
    }

  let num_components t = List.length (Nonempty_list.to_list t.components)

  let reg_counts units =
    List.map (fun { reg_class; components = _ } -> reg_class) units
    |> Reg_count.of_classes

  (* The register class of every unit in a list, which must all agree. *)
  let reg_class (units : t list) =
    match units with
    | [] -> Misc.fatal_error "Extcall_abi.Reg_unit.reg_class: no units"
    | first :: rest ->
      if
        not
          (List.for_all
             (fun unit -> Reg_class.equal unit.reg_class first.reg_class)
             rest)
      then
        Misc.fatal_error
          "Extcall_abi.Reg_unit.reg_class: units of different classes";
      first.reg_class

  let exttype { reg_class; components } : Cmm.exttype =
    match reg_class, components with
    | Float_regs, [component] -> Component.exttype component
    | Float_regs, _ :: _ :: _ ->
      (* An x86-64 SSE eightbyte of packed float32s *)
      XFloat
    | Int_regs, [component] when component.offset = 0 -> (
      match component.reg_class with
      | Int_regs -> Component.exttype_when_alone component
      | Float_regs -> (
        (* arm64 passes all parts of a struct in int regs *)
        match component.size with
        | 8 -> XInt64
        | 4 -> XInt32
        | _ ->
          Misc.fatal_errorf
            "Extcall_abi.Reg_unit.exttype: floating-point component of size %d \
             in an integer unit"
            component.size))
    | Int_regs, _ -> XInt64

  let of_layout apply (abi : Abi.t) ~what (layout : Struct_layout.t) =
    let reg_class_of_components : Component.t list -> Reg_class.t =
      match abi with
      | Aarch64 when Struct_layout.is_arm64_homogeneous_aggregate layout ->
        fun _ -> Float_regs
      | Aarch64 | Amd64_system_v ->
        (if layout.size > 16
         then
           let reason : Error.Memory_reason.t =
             match abi with
             | Aarch64 -> Larger_than_16_bytes_and_not_homogeneous
             | Amd64_system_v -> Larger_than_16_bytes
           in
           Error.raise
             (Passed_in_memory { what; size = layout.size; reason })
             apply);
        fun components ->
          List.map
            (fun ({ reg_class; _ } : Component.t) -> reg_class)
            components
          |> Abi.reg_class_of_components abi
    in
    List.map
      (fun ({ components; size = _ } : Unit.t) ->
        match components with
        | [] ->
          Misc.fatal_error
            "Extcall_abi.Reg_unit.of_layout: padding in a struct passed in \
             registers"
        | first :: rest ->
          { reg_class = reg_class_of_components components;
            components = first :: rest
          })
      (Unit.in_registers abi layout)
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

  let is_unboxed_product t =
    match t with Unboxed_product -> true | Singleton _ -> false

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
      | Singleton _ -> "singleton"
      | Unboxed_product -> "unboxed product")
end

(* How one C argument is passed. *)
module Arg = struct
  type t =
    | Void  (** No C argument *)
    | Scalar of
        { component : Component.t;
          exttype : Cmm.exttype
        }
    | Reg_units of Reg_unit.t list
    | Units of Unit.t list

  (* One slot per element of the [Cextcall]'s argument list. *)
  let exttypes t : Cmm.exttype list =
    match t with
    | Void -> []
    | Scalar { component = _; exttype } -> [exttype]
    | Reg_units reg_units -> List.map Reg_unit.exttype reg_units
    | Units units -> List.map Unit.exttype units
end

(* The placement of one argument in the simulation of the ABI's register
   assignment. *)
module Placement = struct
  type t =
    | In_registers of Arg.t
    | On_stack of
        { arg : Arg.t;
          align : int
              (** Of the C struct; 1 for a scalar, whose slot's own alignment
                  suffices *)
        }

  let arg t =
    match t with In_registers arg | On_stack { arg; align = _ } -> arg

  (* A product with a single non-void component is passed exactly as if it were
     that scalar: for register assignment the supported ABIs do not distinguish
     a struct containing a single scalar member from the member itself. The
     stack slot may however differ; see [Component.exttype_when_alone]. *)
  let of_single_component ~free (arg_head : Arity_component_head.t)
      (component : Component.t) =
    let exttype =
      match arg_head with
      | Singleton _ -> Component.exttype component
      | Unboxed_product -> Component.exttype_when_alone component
    in
    let arg : Arg.t = Scalar { component; exttype } in
    if Reg_count.has_free free component.reg_class
    then Reg_count.take_one free component.reg_class, In_registers arg
    else free, On_stack { arg; align = 1 }

  let fit_in_registers ~free reg_units =
    let needed = Reg_unit.reg_counts reg_units in
    if Reg_count.fits needed ~free
    then Some (Reg_count.subtract free needed)
    else None

  (* AAPCS64 rules C.3 and C.13: when a composite does not fit in the remaining
     registers of its class, no later argument uses that class either. The
     x86-64 psABI instead leaves the registers for later arguments. *)
  let free_after_spill (abi : Abi.t) ~free reg_units =
    match abi with
    | Aarch64 -> Reg_count.exhaust free (Reg_unit.reg_class reg_units)
    | Amd64_system_v -> free

  (* Returns remaining free registers + a placement *)
  let of_product apply (abi : Abi.t) ~free (layout : Struct_layout.t) =
    let on_stack free =
      ( free,
        On_stack
          { arg = Units (Unit.on_stack abi layout); align = layout.align } )
    in
    match abi with
    | Amd64_system_v when layout.size > 16 ->
      (* Class MEMORY; see [Reg_unit.of_layout]. *)
      on_stack free
    | Amd64_system_v | Aarch64 -> (
      let reg_units = Reg_unit.of_layout apply abi ~what:Argument layout in
      match fit_in_registers ~free reg_units with
      | Some free -> free, In_registers (Reg_units reg_units)
      | None -> on_stack (free_after_spill abi ~free reg_units))

  let create apply abi ~free arg_head (layout : Struct_layout.t) =
    match layout.components with
    | [] ->
      (* Void arguments take no registers. *)
      free, In_registers Void
    | [component] -> of_single_component ~free arg_head component
    | _ :: _ :: _ -> of_product apply abi ~free layout
end

(* An element of the [Cextcall]'s argument list. *)
module Slot = struct
  type t =
    | Arg of
        { arg : int;  (** Index of the C argument *)
          slot : int  (** Index into [Arg.slot_exttypes] *)
        }
    | Dummy of Reg_class.t  (** Uses up a register the ABI leaves unused *)
    | Padding  (** An 8-byte stack slot whose contents are unspecified *)

  let exttype (args : Arg.t array) t : Cmm.exttype =
    match t with
    | Arg { arg; slot } -> List.nth (Arg.exttypes args.(arg)) slot
    | Dummy Int_regs -> XDummy_int
    | Dummy Float_regs -> XDummy_float
    | Padding -> XInt64

  let reg_class args t = Reg_class.of_exttype (exttype args t)

  (* The size of the stack slot [Proc.loc_external_arguments] gives on
     x86-64. *)
  let amd64_stack_size args t =
    match exttype args t with
    | XInt | XInt8 | XInt16 | XInt32 | XInt64 | XFloat | XFloat32 -> 8
    | XVec128 -> 16
    | XVec256 -> 32
    | XVec512 -> 64
    | XDummy_int | XDummy_float ->
      Misc.fatal_error "Extcall_abi.Slot.amd64_stack_size: dummy"
end

(* The slots of the stack-bound arguments, in order.

   [Proc.loc_external_arguments] aligns each stack slot to its own size only,
   but the x86-64 psABI places a struct at the struct's alignment, which can
   exceed its first slot's: [{ int64; vec128 }] is 16-aligned yet starts with an
   8-byte slot. On x86-64 this replays the backend's slot assignment and inserts
   padding slots in front of such structs.

   arm64 needs none of this for now since <= 16 byte structs or homogeneous
   aggregates don't cause such alignment issues. *)
module Stack_slots = struct
  module Acc = struct
    type t =
      { offset : int;
        rev_slots : Slot.t list
      }

    let init = { offset = 0; rev_slots = [] }

    let add args t slot =
      let size = Slot.amd64_stack_size args slot in
      let offset = Misc.align t.offset size in
      { offset = offset + size; rev_slots = slot :: t.rev_slots }

    let align args t ~to_ =
      let padding = (Misc.align t.offset to_ - t.offset) / 8 in
      List.fold_left
        (fun t (_ : int) -> add args t Padding)
        t (List.init padding Fun.id)
  end

  let create (abi : Abi.t) (args : Arg.t array) (placements : Placement.t list)
      =
    let acc =
      List.fold_left
        (fun (acc : Acc.t) (index, (placement : Placement.t)) ->
          match placement with
          | In_registers _ -> acc
          | On_stack { arg; align } ->
            let acc =
              match abi with
              | Aarch64 -> acc
              | Amd64_system_v -> Acc.align args acc ~to_:align
            in
            List.mapi
              (fun slot (_ : Cmm.exttype) -> Slot.Arg { arg = index; slot })
              (Arg.exttypes arg)
            |> List.fold_left (Acc.add args) acc)
        Acc.init
        (List.mapi (fun index placement -> index, placement) placements)
    in
    List.rev acc.rev_slots
end

(* The [Cextcall]'s argument list for a call with unboxed product arguments, and
   what each element of it is. *)
module Arg_slots = struct
  type t =
    { args : Arg.t array;  (** Indexed by C argument *)
      slots : Slot.t list
    }

  let register_slots placements =
    placements
    |> List.mapi (fun index (placement : Placement.t) ->
        match placement with
        | In_registers arg ->
          List.mapi
            (fun slot (_ : Cmm.exttype) -> Slot.Arg { arg = index; slot })
            (Arg.exttypes arg)
        | On_stack _ -> [])
    |> List.concat

  (* [Proc.loc_external_arguments] assigns any register of the right class that
     is still free, so the registers left unused by the ABI are used up by dummy
     arguments when a stack-bound slot could otherwise be given one. *)
  let dummies abi args ~register_slots ~stack_slots =
    let free =
      let init_regs = Abi.num_arg_regs abi in
      let used_regs =
        Reg_count.of_classes (List.map (Slot.reg_class args) register_slots)
      in
      Reg_count.subtract init_regs used_regs
    in
    let dummies_for (reg_class : Reg_class.t) =
      if
        List.exists
          (fun slot -> Reg_class.equal (Slot.reg_class args slot) reg_class)
          stack_slots
      then
        List.init (Reg_count.get free reg_class) (fun _ -> Slot.Dummy reg_class)
      else []
    in
    dummies_for Int_regs @ dummies_for Float_regs

  let with_unboxed_products apply abi arg_heads arg_layouts =
    let (_ : Reg_count.t), placements =
      List.fold_left_map
        (fun free (arg_head, layout) ->
          Placement.create apply abi ~free arg_head layout)
        (Abi.num_arg_regs abi)
        (List.combine arg_heads arg_layouts)
    in
    let args = Array.of_list (List.map Placement.arg placements) in
    let register_slots = register_slots placements in
    let stack_slots = Stack_slots.create abi args placements in
    let dummies = dummies abi args ~register_slots ~stack_slots in
    { args; slots = register_slots @ dummies @ stack_slots }

  (* Void also returns [false] since we ignore them *)
  let involves_unboxed_products arg_heads arg_layouts =
    List.exists2
      (fun (arg_head : Arity_component_head.t)
           ({ components; size = _; align = _ } : Struct_layout.t) ->
        match arg_head, components with
        | Unboxed_product, _ :: _ -> true
        | (Unboxed_product | Singleton _), _ -> false)
      arg_heads arg_layouts

  let scalars_only arg_heads arg_layouts =
    let arg (arg_head : Arity_component_head.t)
        ({ components; size = _; align = _ } : Struct_layout.t) : Arg.t =
      match components with
      | [] -> Void
      | [component] ->
        Scalar { component; exttype = Component.exttype component }
      | _ :: _ :: _ ->
        Misc.fatal_errorf
          "Extcall_abi.Arg_slots.scalars_only: product argument %a"
          Arity_component_head.print arg_head
    in
    let args = Array.of_list (List.map2 arg arg_heads arg_layouts) in
    let slots =
      Array.mapi
        (fun arg (a : Arg.t) ->
          List.mapi
            (fun slot (_ : Cmm.exttype) -> Slot.Arg { arg; slot })
            (Arg.exttypes a))
        args
      |> Array.to_list |> List.concat
    in
    { args; slots }

  let create apply =
    let arity = Apply.args_arity apply in
    let arg_heads = Arity_component_head.of_arity arity in
    let arg_layouts = Struct_layout.of_arity_per_argument arity in
    if involves_unboxed_products arg_heads arg_layouts
    then
      with_unboxed_products apply
        (Abi.for_unboxed_products apply)
        arg_heads arg_layouts
    else scalars_only arg_heads arg_layouts
end

(* How the return value of an external call is passed back. *)
module Return = struct
  type t =
    | No_value  (** A void return in C *)
    | Scalar of K.With_subkind.t
        (** Including products with a single non-void component *)
    | Reg_units of Reg_unit.t list

  let reg_units apply return_arity return_kinds =
    let abi = Abi.for_unboxed_products apply in
    let reg_units =
      Reg_unit.of_layout apply abi ~what:Return_value
        (Struct_layout.of_arity return_arity)
    in
    let num_components =
      List.fold_left
        (fun n reg_unit -> n + Reg_unit.num_components reg_unit)
        0 reg_units
    in
    if List.compare_length_with return_kinds num_components <> 0
    then
      Misc.fatal_errorf
        "Extcall_abi.Return.reg_units: unit components do not match the return \
         arity:@ %a"
        Apply.print apply;
    reg_units

  let create apply return_arity =
    match Flambda_arity.unarize return_arity with
    | [] -> No_value
    | [kind] ->
      if
        List.exists Arity_component_head.is_unboxed_product
          (Arity_component_head.of_arity return_arity)
      then ignore (Abi.for_unboxed_products apply : Abi.t);
      Scalar kind
    | _ :: _ :: _ as return_kinds ->
      Reg_units (reg_units apply return_arity return_kinds)
end
