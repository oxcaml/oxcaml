(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

open Lambda

module Or_missing = struct
  type 'a t =
    | Present of 'a
    | Missing

  let of_option = function Some a -> Present a | None -> Missing

  let[@inline] map t ~f =
    match t with
    | Present a -> Present ((f [@inlined hint]) a)
    | Missing -> Missing

  let[@inline] bind t ~f =
    match t with Present a -> (f [@inlined hint]) a | Missing -> Missing

  module Syntax = struct
    let[@inline] ( let* ) t f = bind t ~f

    let[@inline] ( |>> ) t f = map t ~f
  end
end

open Or_missing.Syntax

module rec Types : sig
  type closure =
    { clo_params : Slambdaident.t array;
      clo_body : slambda;
      clo_env : Env.t
    }

  type value =
    | SLVhalves of slambda_halves
    | SLVlayout of layout
    | SLVrecord of value Or_missing.t array
    | SLVclosure of closure
end =
  Types

and Env : sig
  type t

  val empty : t

  val add : t -> Slambdaident.t -> Types.value Or_missing.t -> t

  val add_present : t -> Slambdaident.t -> Types.value -> t

  val find : t -> Slambdaident.t -> Types.value Or_missing.t
end = struct
  module Map = Slambdaident.Map

  type t = Types.value Map.t

  let empty = Map.empty

  let add t id v =
    match (v : Types.value Or_missing.t) with
    | Present v -> Map.add id v t
    | Missing -> (* Possibly unnecessary but be safe anyway *) Map.remove id t

  let add_present t id v = add t id (Present v)

  let find t id = Map.find_opt id t |> Or_missing.of_option
end

open Types

let errf fmt = Misc.fatal_errorf ("slambda eval: " ^^ fmt)

type _ value_type =
  | Thalves : slambda_halves value_type
  | Tlayout : layout value_type
  | Trecord : value Or_missing.t array value_type
  | Tclosure : closure value_type

let describe_value_type (type a) : a value_type -> string = function
  | Thalves -> "program"
  | Tlayout -> "layout value"
  | Trecord -> "record"
  | Tclosure -> "template"

type value_type_packed = TP : _ value_type -> value_type_packed

let typeof = function
  | SLVhalves _ -> TP Thalves
  | SLVlayout _ -> TP Tlayout
  | SLVrecord _ -> TP Trecord
  | SLVclosure _ -> TP Tclosure

let expect_err ?reason ~expected ~actual =
  let pp_reason ppf () =
    match reason with
    | Some reason -> Format.fprintf ppf " (%s)" reason
    | None -> ()
  in
  errf "expected %s%a but found %s"
    (describe_value_type expected)
    pp_reason ()
    (describe_value_type actual)

let expect (type a) ?reason (vty : a value_type) (v : value) : a =
  match vty, v with
  | Thalves, SLVhalves halves -> halves
  | Tlayout, SLVlayout layout -> layout
  | Trecord, SLVrecord record -> record
  | Tclosure, SLVclosure closure -> closure
  | _, _ ->
    let (TP actual_vty) = typeof v in
    expect_err ?reason ~expected:vty ~actual:actual_vty

let expect_not_missing (a : 'a Or_missing.t) : 'a =
  match a with Present a -> a | Missing -> errf "unexpected missing value"

let rec eval_slam env slam : value Or_missing.t =
  match slam with
  | SLhalves { sval_comptime; sval_runtime } ->
    let sval_runtime = eval_lam env sval_runtime in
    Present (SLVhalves { sval_comptime; sval_runtime })
  | SLlayout layout -> Present (SLVlayout layout)
  | SLglobal _ -> errf "cross-module eval not implemented"
  | SLvar id -> eval_var env id
  | SLlet { slet_name; slet_value; slet_body } ->
    let value = eval_slam env slet_value in
    let env_body = Env.add env slet_name value in
    eval_slam env_body slet_body
  | SLmissing -> Missing
  | SLrecord slams ->
    let values = Array.map (eval_slam env) (Array.of_list slams) in
    Present (SLVrecord values)
  | SLfield (slam, i) ->
    let* fields = eval_slam env slam |>> expect Trecord in
    fields.(i)
  | SLproj_comptime slam ->
    let* halves = eval_slam env slam |>> expect Thalves in
    eval_slam env halves.sval_comptime
  | SLtemplate { sfun_params; sfun_body } ->
    Present
      (SLVclosure
         { clo_params = sfun_params; clo_body = sfun_body; clo_env = env })
  | SLinstantiate { sapp_func; sapp_arguments } ->
    let* closure = eval_slam env sapp_func |>> expect Tclosure in
    let eval_arg arg = eval_slam env arg |> expect_not_missing in
    let args = Array.map eval_arg sapp_arguments in
    let { clo_params; clo_body; clo_env } = closure in
    let env_body =
      Misc.Stdlib.Array.fold_left2 Env.add_present clo_env clo_params args
    in
    eval_slam env_body clo_body

and eval_var env id = Env.find env id

and eval_lam env lam =
  match lam with
  | Lconst const -> Lconst (eval_structured_const env const)
  | Lapply
      { ap_func;
        ap_args;
        ap_result_layout;
        ap_region_close;
        ap_mode;
        ap_loc;
        ap_tailcall;
        ap_inlined;
        ap_specialised;
        ap_probe
      } ->
    let ap_func = eval_lam env ap_func in
    let ap_args = List.map (eval_lam env) ap_args in
    let ap_result_layout = eval_layout env ap_result_layout in
    Lapply
      { ap_func;
        ap_args;
        ap_result_layout;
        ap_region_close;
        ap_mode;
        ap_loc;
        ap_tailcall;
        ap_inlined;
        ap_specialised;
        ap_probe
      }
  | Lfunction lfunction -> Lfunction (eval_lfunction env lfunction)
  | Llet (kind, layout, id, uid, rhs, body) ->
    let layout = eval_layout env layout in
    let rhs = eval_lam env rhs in
    let body = eval_lam env body in
    Llet (kind, layout, id, uid, rhs, body)
  | Lmutlet (layout, id, uid, rhs, body) ->
    let layout = eval_layout env layout in
    let rhs = eval_lam env rhs in
    let body = eval_lam env body in
    Lmutlet (layout, id, uid, rhs, body)
  | Lletrec (bindings, body) ->
    let eval_binding { id; debug_uid; def } =
      let def = eval_lfunction env def in
      { id; debug_uid; def }
    in
    let bindings = List.map eval_binding bindings in
    let body = eval_lam env body in
    Lletrec (bindings, body)
  | Lprim (prim, args, loc) ->
    let prim = eval_prim env prim in
    let args = List.map (eval_lam env) args in
    Lprim (prim, args, loc)
  | Lswitch (lam, switch, loc, layout) ->
    let { sw_numconsts; sw_consts; sw_numblocks; sw_blocks; sw_failaction } =
      switch
    in
    let lam = eval_lam env lam in
    let sw_consts = List.map (fun (i, lam) -> i, eval_lam env lam) sw_consts in
    let sw_blocks = List.map (fun (i, lam) -> i, eval_lam env lam) sw_blocks in
    let sw_failaction = Option.map (eval_lam env) sw_failaction in
    let switch =
      { sw_numconsts; sw_consts; sw_numblocks; sw_blocks; sw_failaction }
    in
    let layout = eval_layout env layout in
    Lswitch (lam, switch, loc, layout)
  | Lstringswitch (lam, branches, failaction, loc, layout) ->
    let lam = eval_lam env lam in
    let branches = List.map (fun (i, lam) -> i, eval_lam env lam) branches in
    let failaction = Option.map (eval_lam env) failaction in
    let layout = eval_layout env layout in
    Lstringswitch (lam, branches, failaction, loc, layout)
  | Lstaticraise (label, args) ->
    let args = List.map (eval_lam env) args in
    Lstaticraise (label, args)
  | Lstaticcatch (body, (label, params), handler_body, pop_region, layout) ->
    let body = eval_lam env body in
    let params =
      List.map (fun (id, uid, layout) -> id, uid, eval_layout env layout) params
    in
    let handler_body = eval_lam env handler_body in
    let layout = eval_layout env layout in
    Lstaticcatch (body, (label, params), handler_body, pop_region, layout)
  | Ltrywith (body, id, debug_uid, handler_body, layout) ->
    let body = eval_lam env body in
    let handler_body = eval_lam env handler_body in
    let layout = eval_layout env layout in
    Ltrywith (body, id, debug_uid, handler_body, layout)
  | Lifthenelse (lam, iftrue, iffalse, layout) ->
    let lam = eval_lam env lam in
    let iftrue = eval_lam env iftrue in
    let iffalse = eval_lam env iffalse in
    let layout = eval_layout env layout in
    Lifthenelse (lam, iftrue, iffalse, layout)
  | Lsequence (lam1, lam2) ->
    let lam1 = eval_lam env lam1 in
    let lam2 = eval_lam env lam2 in
    Lsequence (lam1, lam2)
  | Lwhile { wh_cond; wh_body } ->
    let wh_cond = eval_lam env wh_cond in
    let wh_body = eval_lam env wh_body in
    Lwhile { wh_cond; wh_body }
  | Lfor { for_id; for_debug_uid; for_loc; for_from; for_to; for_dir; for_body }
    ->
    let for_from = eval_lam env for_from in
    let for_to = eval_lam env for_to in
    let for_body = eval_lam env for_body in
    Lfor { for_id; for_debug_uid; for_loc; for_from; for_to; for_dir; for_body }
  | Lassign (id, lam) -> Lassign (id, eval_lam env lam)
  | Lsend (kind, met, obj, args, region_close, mode, loc, layout) ->
    let met = eval_lam env met in
    let obj = eval_lam env obj in
    let args = List.map (eval_lam env) args in
    let layout = eval_layout env layout in
    Lsend (kind, met, obj, args, region_close, mode, loc, layout)
  | Levent (lam, ev) -> Levent (eval_lam env lam, ev)
  | Lifused (id, lam) -> Lifused (id, eval_lam env lam)
  | Lregion (lam, layout) ->
    let lam = eval_lam env lam in
    let layout = eval_layout env layout in
    Lregion (lam, layout)
  | Lexclave lam -> Lexclave (eval_lam env lam)
  | Lsplice (_loc, slam) ->
    let halves = eval_slam env slam |> expect_not_missing |> expect Thalves in
    halves.sval_runtime
  | Lvar _id | Lmutvar _id -> lam

and eval_structured_const env const =
  match const with
  | Const_mixed_block (n, shape, consts) ->
    let shape = eval_mixed_block_shape env shape in
    let consts = List.map (eval_structured_const env) consts in
    Const_mixed_block (n, shape, consts)
  | Const_block (n, consts) ->
    let consts = List.map (eval_structured_const env) consts in
    Const_block (n, consts)
  | Const_base _ | Const_float_array _ | Const_immstring _ | Const_float_block _
  | Const_null ->
    const

and eval_block_shape env shape =
  match shape with
  | All_value -> All_value
  | Shape shape -> Shape (eval_mixed_block_shape env shape)

and eval_mixed_block_shape :
    'a. Env.t -> 'a mixed_block_element array -> 'a mixed_block_element array =
 fun env shape -> Array.map (eval_mixed_block_element env) shape

and eval_mixed_block_element :
    'a. Env.t -> 'a mixed_block_element -> 'a mixed_block_element =
 fun env element ->
  match element with
  | Splice_variable id ->
    eval_var env (id |> Slambdaident.of_ident)
    |> expect_not_missing |> expect Tlayout |> mixed_block_element_of_layout
  | Product elements ->
    Product (Array.map (eval_mixed_block_element env) elements)
  | Value _ | Float_boxed _ | Float64 | Float32 | Bits8 | Bits16 | Bits32
  | Bits64 | Vec128 | Vec256 | Vec512 | Word | Untagged_immediate ->
    element

and eval_layout env layout =
  match layout with
  | Psplicevar id ->
    eval_var env (id |> Slambdaident.of_ident)
    |> expect_not_missing |> expect Tlayout
  | Punboxed_product layouts ->
    Punboxed_product (List.map (eval_layout env) layouts)
  | Ptop | Pvalue _ | Punboxed_float _ | Punboxed_or_untagged_integer _
  | Punboxed_vector _ | Pbottom ->
    layout

and eval_lfunction env { kind; params; return; body; attr; loc; mode; ret_mode }
    =
  let eval_lparam { name; debug_uid; layout; attributes; mode } =
    let layout = eval_layout env layout in
    { name; debug_uid; layout; attributes; mode }
  in
  let params = List.map eval_lparam params in
  let return = eval_layout env return in
  let body = eval_lam env body in
  lfunction' ~kind ~params ~return ~body ~attr ~loc ~mode ~ret_mode

and eval_prim env prim =
  match prim with
  | Pmakeblock (n, mut, shape, mode) ->
    Pmakeblock (n, mut, eval_block_shape env shape, mode)
  | Pmixedfield (is, shape, sem) ->
    Pmixedfield (is, eval_mixed_block_shape env shape, sem)
  | Psetmixedfield (is, shape, init_or_assign) ->
    Psetmixedfield (is, eval_mixed_block_shape env shape, init_or_assign)
  | Pmake_unboxed_product layouts ->
    Pmake_unboxed_product (List.map (eval_layout env) layouts)
  | Punboxed_product_field (i, layouts) ->
    Punboxed_product_field (i, List.map (eval_layout env) layouts)
  | Pmake_idx_mixed_field (shape, i, path) ->
    Pmake_idx_mixed_field (eval_mixed_block_shape env shape, i, path)
  | Pmake_idx_array (kind, index_kind, element, path) ->
    Pmake_idx_array
      (kind, index_kind, eval_mixed_block_element env element, path)
  | Pidx_deepen (element, path) ->
    Pidx_deepen (eval_mixed_block_element env element, path)
  | Popaque layout -> Popaque (eval_layout env layout)
  | Pobj_magic layout -> Pobj_magic (eval_layout env layout)
  | Pget_idx (layout, mut) -> Pget_idx (eval_layout env layout, mut)
  | Pset_idx (layout, mode) -> Pset_idx (eval_layout env layout, mode)
  | Pget_ptr (layout, mut) -> Pget_ptr (eval_layout env layout, mut)
  | Pset_ptr (layout, mode) -> Pset_ptr (eval_layout env layout, mode)
  | Pbytes_to_string | Pbytes_of_string | Pignore | Pgetglobal _ | Pgetpredef _
  | Pmakefloatblock _ | Pmakeufloatblock _ | Pmakelazyblock _ | Pfield _
  | Pfield_computed _ | Psetfield _ | Psetfield_computed _ | Pfloatfield _
  | Psetfloatfield _ | Psetufloatfield _ | Pufloatfield _ | Pduprecord _
  | Parray_element_size_in_bytes _ | Pmake_idx_field _ | Pwith_stack
  | Pwith_stack_bind | Pperform | Presume | Preperform | Pccall _ | Praise _
  | Psequand | Psequor | Pnot | Pphys_equal _ | Pscalar _ | Poffsetref _
  | Pstringlength | Pstringrefu | Pstringrefs | Pbyteslength | Pbytesrefu
  | Pbytessetu | Pbytesrefs | Pbytessets | Pmakearray _ | Pmakearray_dynamic _
  | Pduparray _ | Parrayblit _ | Parraylength _ | Parrayrefu _ | Parraysetu _
  | Parrayrefs _ | Parraysets _ | Pisint _ | Pisnull | Pisout | Pbigarrayref _
  | Pbigarrayset _ | Pbigarraydim _ | Pstring_load_i8 _ | Pstring_load_i16 _
  | Pstring_load_16 _ | Pstring_load_32 _ | Pstring_load_f32 _
  | Pstring_load_64 _ | Pstring_load_vec _ | Pbytes_load_i8 _
  | Pbytes_load_i16 _ | Pbytes_load_16 _ | Pbytes_load_32 _ | Pbytes_load_f32 _
  | Pbytes_load_64 _ | Pbytes_load_vec _ | Pbytes_set_8 _ | Pbytes_set_16 _
  | Pbytes_set_32 _ | Pbytes_set_f32 _ | Pbytes_set_64 _ | Pbytes_set_vec _
  | Pbigstring_load_i8 _ | Pbigstring_load_i16 _ | Pbigstring_load_16 _
  | Pbigstring_load_32 _ | Pbigstring_load_f32 _ | Pbigstring_load_64 _
  | Pbigstring_load_vec _ | Pbigstring_set_8 _ | Pbigstring_set_16 _
  | Pbigstring_set_32 _ | Pbigstring_set_f32 _ | Pbigstring_set_64 _
  | Pbigstring_set_vec _ | Pfloatarray_load_vec _ | Pfloat_array_load_vec _
  | Pint_array_load_vec _ | Punboxed_float_array_load_vec _
  | Punboxed_float32_array_load_vec _ | Puntagged_int8_array_load_vec _
  | Puntagged_int16_array_load_vec _ | Punboxed_int32_array_load_vec _
  | Punboxed_int64_array_load_vec _ | Punboxed_nativeint_array_load_vec _
  | Pfloatarray_set_vec _ | Pfloat_array_set_vec _ | Pint_array_set_vec _
  | Punboxed_float_array_set_vec _ | Punboxed_float32_array_set_vec _
  | Puntagged_int8_array_set_vec _ | Puntagged_int16_array_set_vec _
  | Punboxed_int32_array_set_vec _ | Punboxed_int64_array_set_vec _
  | Punboxed_nativeint_array_set_vec _ | Pctconst _ | Pint_as_pointer _
  | Patomic_load_field _ | Patomic_set_field _ | Patomic_exchange_field _
  | Patomic_compare_exchange_field _ | Patomic_compare_set_field _
  | Patomic_fetch_add_field | Patomic_add_field | Patomic_sub_field
  | Patomic_land_field | Patomic_lor_field | Patomic_lxor_field
  | Pprobe_is_enabled _ | Pobj_dup | Punbox_unit | Punbox_vector _
  | Pbox_vector _ | Pjoin_vec256 | Psplit_vec256
  | Preinterpret_boxed_vector_as_tuple _ | Preinterpret_tuple_as_boxed_vector _
  | Preinterpret_unboxed_int64_as_tagged_int63
  | Preinterpret_tagged_int63_as_unboxed_int64 | Parray_to_iarray
  | Parray_of_iarray | Pget_header _ | Ppeek _ | Ppoke _ | Pdls_get | Ptls_get
  | Pdomain_index | Ppoll | Pcpu_relax ->
    prim

(* Helpers for asserting that slambda is trivial. *)

exception Found_a_splice

let rec assert_layout_contains_no_splices : Lambda.layout -> unit = function
  | Psplicevar _ -> raise Found_a_splice
  | Ptop | Pbottom | Pvalue _ | Punboxed_float _
  | Punboxed_or_untagged_integer _ | Punboxed_vector _ ->
    ()
  | Punboxed_product layouts ->
    List.iter assert_layout_contains_no_splices layouts

let rec assert_mixed_block_element_contains_no_splices : type a.
    a Lambda.mixed_block_element -> unit = function
  | Splice_variable _ -> raise Found_a_splice
  | Value _ | Float_boxed _ | Float64 | Float32 | Bits8 | Bits16 | Bits32
  | Bits64 | Vec128 | Vec256 | Vec512 | Word | Untagged_immediate ->
    ()
  | Product elements ->
    Array.iter assert_mixed_block_element_contains_no_splices elements

let assert_mixed_block_shape_contains_no_splices shape =
  Array.iter assert_mixed_block_element_contains_no_splices shape

let assert_primitive_contains_no_splices (prim : Lambda.primitive) =
  match prim with
  | Popaque layout | Pobj_magic layout ->
    assert_layout_contains_no_splices layout
  | Pget_idx (layout, _)
  | Pset_idx (layout, _)
  | Pget_ptr (layout, _)
  | Pset_ptr (layout, _) ->
    assert_layout_contains_no_splices layout
  | Pmake_unboxed_product layouts | Punboxed_product_field (_, layouts) ->
    List.iter assert_layout_contains_no_splices layouts
  | Pmakeblock (_, _, Shape shape, _) ->
    assert_mixed_block_shape_contains_no_splices shape
  | Pmixedfield (_, shape, _) ->
    Array.iter assert_mixed_block_element_contains_no_splices shape
  | Psetmixedfield (_, shape, _) ->
    assert_mixed_block_shape_contains_no_splices shape
  | Pmake_idx_mixed_field (shape, _, _) ->
    assert_mixed_block_shape_contains_no_splices shape
  | Pmake_idx_array (_, _, element, _) | Pidx_deepen (element, _) ->
    assert_mixed_block_element_contains_no_splices element
  | _ -> ()

let assert_function_contains_no_splices { Lambda.params; return; _ } =
  List.iter
    (fun { Lambda.layout; _ } -> assert_layout_contains_no_splices layout)
    params;
  assert_layout_contains_no_splices return

let rec assert_no_splices (lam : Lambda.lambda) =
  (match lam with
  | Lvar _ | Lmutvar _ | Lconst _ -> ()
  | Lapply { ap_result_layout; _ } ->
    assert_layout_contains_no_splices ap_result_layout
  | Lfunction func -> assert_function_contains_no_splices func
  | Llet (_, layout, _, _, _, _) -> assert_layout_contains_no_splices layout
  | Lmutlet (layout, _, _, _, _) -> assert_layout_contains_no_splices layout
  | Lletrec (_, _) -> ()
  | Lprim (prim, _, _) -> assert_primitive_contains_no_splices prim
  | Lswitch (_, _, _, layout) -> assert_layout_contains_no_splices layout
  | Lstringswitch (_, _, _, _, layout) ->
    assert_layout_contains_no_splices layout
  | Lstaticraise _ -> ()
  | Lstaticcatch (_, (_, bindings), _, _, layout) ->
    List.iter
      (fun (_, _, layout) -> assert_layout_contains_no_splices layout)
      bindings;
    assert_layout_contains_no_splices layout
  | Ltrywith (_, _, _, _, layout) -> assert_layout_contains_no_splices layout
  | Lifthenelse (_, _, _, layout) -> assert_layout_contains_no_splices layout
  | Lsequence _ | Lwhile _ | Lfor _ | Lassign _ -> ()
  | Lsend (_, _, _, _, _, _, _, layout) ->
    assert_layout_contains_no_splices layout
  | Levent _ | Lifused _ -> ()
  | Lregion (_, layout) -> assert_layout_contains_no_splices layout
  | Lexclave _ -> ()
  | Lsplice _ -> raise Found_a_splice);
  Lambda.iter_head_constructor assert_no_splices lam

let do_eval slam =
  eval_slam Env.empty slam |> expect_not_missing
  |> expect Thalves ~reason:"toplevel module"

let eval slam =
  Profile.record_call "static_eval" (fun () ->
      let halves = do_eval slam in
      (try assert_no_splices halves.sval_runtime
       with Found_a_splice ->
         Misc.fatal_error
           "Encountered a splice in the program after slambda eval");
      halves)
