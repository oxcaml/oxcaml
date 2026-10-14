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
  let return_ty = C.Extended_machtype.to_machtype return_ty in
  let component_tys =
    (* Two notes:

       1. void has been erased in return arities by this point

       2. All of the [machtype_component]s are singleton arrays. *)
    Array.map (fun machtype -> [| machtype |]) return_ty
  in
  let ty_args =
    List.map C.exttype_of_kind
      (Flambda_arity.unarize (Apply.args_arity apply)
      |> List.map K.With_subkind.kind)
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
    let kinds = Flambda_arity.unarize return_arity in
    (* As per the comment above, [return_arity] does not mention void
       components. (Unlike parameter arities; see the phantom type parameters on
       the arity fields in [Apply_expr.t], for example.) *)
    assert (List.compare_length_with kinds (Array.length component_tys) = 0);
    match kinds with
    | [] ->
      (* CR mshinwell: this statement would seem to be wrong if we permit void
         returns from extcalls *)
      (* Extcalls of arity 0 are allowed (these never return). *)
      return_values
    | [kind] -> maybe_sign_extend kind dbg return_values
    | [_; _] as kinds ->
      (* CR xclerc: we currently support only pairs as unboxed return values. *)
      (* CR mshinwell: we also currently only support 64 bit integer and float
         values (in addition to things of kind [Value] which count as 64-bit
         integers for this purpose), since on (at least) x86-64 the calling
         convention differs for smaller widths. *)
      List.iter
        (fun kind ->
          match Flambda_kind.With_subkind.kind kind with
          | Value
          | Naked_number
              (Naked_immediate | Naked_int64 | Naked_nativeint | Naked_float) ->
            ()
          | Naked_number (Naked_float32 | Naked_vec128) -> (
            match Target_system.architecture () with
            | AArch64 -> ()
            | X86_64 ->
              Misc.fatal_errorf
                "Cannot compile unboxed product return from external C call \
                 with a component of kind %a"
                Flambda_kind.With_subkind.print kind
            | IA32 | ARM | POWER | Z | Riscv ->
              Misc.fatal_error "Only x86-64 and arm64 are supported")
          | Naked_number
              ( Naked_int8 | Naked_int16 | Naked_int32 | Naked_vec256
              | Naked_vec512 | Naked_mask )
          | Region | Rec_info ->
            Misc.fatal_errorf
              "Cannot compile unboxed product return from external C call with \
               a component of kind %a"
              Flambda_kind.With_subkind.print kind)
        kinds;
      (* CR mshinwell: Digest page 35 of this doc:

         https://github.com/ARM-software/abi-aa/releases/download/2024Q3/aapcs64.pdf

         and figure out what happens for mixed int/float struct returns (it
         looks like the floats may be returned in int regs)

         jvanburen: that seems to be what clang does:
         https://godbolt.org/z/snzEoME9h *)
      (match Target_system.architecture () with
      | X86_64 -> ()
      | AArch64 ->
        let kinds = Flambda_kind.With_subkind.Set.of_list kinds in
        if Flambda_kind.With_subkind.Set.cardinal kinds <> 1
        then
          Misc.fatal_errorf
            "Cannot compile unboxed product return from external C call on \
             arm64 unless the components of the product are of the same kind:@ \
             %a"
            Apply.print apply
      | IA32 | ARM | POWER | Z | Riscv ->
        Misc.fatal_error "Only x86-64 and arm64 are supported");
      let get_unarized_return_value exp n =
        C.tuple_field exp ~component_tys n dbg
      in
      C.make_tuple
        (List.mapi
           (fun i kind ->
             maybe_sign_extend kind dbg
               (get_unarized_return_value return_values i))
           kinds)
    | _ ->
      Misc.fatal_errorf
        "C functions are currently limited to a single return value or a pair \
         of return values"
  in
  let extcall_ident = Ident.create_local "extcall" in
  let extcall_var = Backend_var.With_provenance.create extcall_ident in
  let cmm =
    C.letin extcall_var ~defining_expr:extcall ~body:(wrap (Cvar extcall_ident))
  in
  cmm, free_vars, env, res, Ece.all
