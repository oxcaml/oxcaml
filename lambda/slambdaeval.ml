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

  type halves =
    { slv_comptime : value Or_missing.t;
      slv_runtime : lambda
    }

  and value =
    | SLVhalves of halves
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

include Types

let errf fmt = Misc.fatal_errorf ("slambda eval: " ^^ fmt)

type _ value_type =
  | Thalves : halves value_type
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
    let slv_comptime = eval_slam env sval_comptime in
    let slv_runtime = eval_lam env sval_runtime in
    Present (SLVhalves { slv_comptime; slv_runtime })
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
    halves.slv_comptime
  | SLtemplate { sfun_params; sfun_body } ->
    Present
      (SLVclosure
         { clo_params = sfun_params; clo_body = sfun_body; clo_env = env })
  | SLinstantiate { sapp_func; sapp_arguments } ->
    let closure =
      eval_slam env sapp_func |> expect_not_missing |> expect Tclosure
    in
    let eval_arg arg = eval_slam env arg |> expect_not_missing in
    let args = Array.map eval_arg sapp_arguments in
    let { clo_params; clo_body; clo_env } = closure in
    let env_body =
      Misc.Stdlib.Array.fold_left2 Env.add_present clo_env clo_params args
    in
    eval_slam env_body clo_body

and eval_var env id = Env.find env id

and eval_lam env lam = Lambda.map (eval_lam_shallow env) lam

and eval_lam_shallow env lam =
  match lam with
  | Lconst old_const ->
    let new_const = eval_structured_const env old_const in
    if new_const == old_const then lam else Lconst new_const
  | Lapply
      { ap_func;
        ap_args;
        ap_result_layout = old_result_layout;
        ap_region_close;
        ap_mode;
        ap_loc;
        ap_tailcall;
        ap_inlined;
        ap_specialised;
        ap_probe
      } ->
    let new_result_layout = eval_layout env old_result_layout in
    if new_result_layout == old_result_layout
    then lam
    else
      Lapply
        { ap_func;
          ap_args;
          ap_result_layout = new_result_layout;
          ap_region_close;
          ap_mode;
          ap_loc;
          ap_tailcall;
          ap_inlined;
          ap_specialised;
          ap_probe
        }
  | Lfunction old_lfunction ->
    let new_lfunction = eval_lfunction_shallow env old_lfunction in
    if new_lfunction == old_lfunction then lam else Lfunction new_lfunction
  | Llet (kind, old_layout, id, uid, rhs, body) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout
    then lam
    else Llet (kind, new_layout, id, uid, rhs, body)
  | Lmutlet (old_layout, id, uid, rhs, body) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout
    then lam
    else Lmutlet (new_layout, id, uid, rhs, body)
  | Lletrec (old_bindings, body) ->
    let eval_binding ({ id; debug_uid; def = old_def } as binding) =
      let new_def = eval_lfunction_shallow env old_def in
      if new_def == old_def then binding else { id; debug_uid; def = new_def }
    in
    let new_bindings = Misc.Stdlib.List.map_sharing eval_binding old_bindings in
    if new_bindings == old_bindings then lam else Lletrec (new_bindings, body)
  | Lprim (old_prim, args, loc) ->
    let new_prim = eval_prim env old_prim in
    if new_prim == old_prim then lam else Lprim (new_prim, args, loc)
  | Lswitch (scrutinee, switch, loc, old_layout) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout
    then lam
    else Lswitch (scrutinee, switch, loc, new_layout)
  | Lstringswitch (body, branches, failaction, loc, old_layout) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout
    then lam
    else Lstringswitch (body, branches, failaction, loc, new_layout)
  | Lstaticcatch
      (body, (label, old_params), handler_body, pop_region, old_layout) ->
    let new_params =
      Misc.Stdlib.List.map_sharing
        (fun ((id, uid, old_layout) as param) ->
          let new_layout = eval_layout env old_layout in
          if new_layout == old_layout then param else id, uid, new_layout)
        old_params
    in
    let new_layout = eval_layout env old_layout in
    if new_params == old_params && new_layout == old_layout
    then lam
    else
      Lstaticcatch
        (body, (label, new_params), handler_body, pop_region, new_layout)
  | Ltrywith (body, id, debug_uid, handler_body, old_layout) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout
    then lam
    else Ltrywith (body, id, debug_uid, handler_body, new_layout)
  | Lifthenelse (cond, iftrue, iffalse, old_layout) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout
    then lam
    else Lifthenelse (cond, iftrue, iffalse, new_layout)
  | Lsend (kind, met, obj, args, region_close, mode, loc, old_layout) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout
    then lam
    else Lsend (kind, met, obj, args, region_close, mode, loc, new_layout)
  | Lregion (body, old_layout) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout then lam else Lregion (body, new_layout)
  | Lsplice (_loc, slam) ->
    let halves = eval_slam env slam |> expect_not_missing |> expect Thalves in
    halves.slv_runtime
  | Lvar _ | Lmutvar _
  | Lstaticraise (_, _)
  | Lsequence (_, _)
  | Lwhile { wh_cond = _; wh_body = _ }
  | Lfor
      { for_id = _;
        for_debug_uid = _;
        for_loc = _;
        for_from = _;
        for_to = _;
        for_dir = _;
        for_body = _
      }
  | Lassign (_, _)
  | Levent (_, _)
  | Lifused (_, _)
  | Lexclave _ ->
    lam

and eval_structured_const env const =
  match const with
  | Const_mixed_block (n, old_shape, old_consts) ->
    let new_shape = eval_mixed_block_shape env old_shape in
    let new_consts =
      Misc.Stdlib.List.map_sharing (eval_structured_const env) old_consts
    in
    if new_shape == old_shape && new_consts == old_consts
    then const
    else Const_mixed_block (n, new_shape, new_consts)
  | Const_block (n, old_consts) ->
    let new_consts =
      Misc.Stdlib.List.map_sharing (eval_structured_const env) old_consts
    in
    if new_consts == old_consts then const else Const_block (n, new_consts)
  | Const_base _ | Const_float_array _ | Const_immstring _ | Const_float_block _
  | Const_null ->
    const

and eval_block_shape env block_shape =
  match block_shape with
  | All_value -> block_shape
  | Shape old_shape ->
    let new_shape = eval_mixed_block_shape env old_shape in
    if new_shape == old_shape then block_shape else Shape new_shape

and eval_mixed_block_shape :
    'a. Env.t -> 'a mixed_block_element array -> 'a mixed_block_element array =
 fun env shape ->
  Misc.Stdlib.Array.map_sharing (eval_mixed_block_element env) shape

and eval_mixed_block_element :
    'a. Env.t -> 'a mixed_block_element -> 'a mixed_block_element =
 fun env element ->
  match element with
  | Splice_variable id ->
    eval_var env (id |> Slambdaident.of_ident)
    |> expect_not_missing |> expect Tlayout |> mixed_block_element_of_layout
  | Product old_elements ->
    let new_elements =
      Misc.Stdlib.Array.map_sharing (eval_mixed_block_element env) old_elements
    in
    if new_elements == old_elements then element else Product new_elements
  | Value _ | Float_boxed _ | Float64 | Float32 | Bits8 | Bits16 | Bits32
  | Bits64 | Vec128 | Vec256 | Vec512 | Word | Untagged_immediate ->
    element

and eval_layout env layout =
  match layout with
  | Psplicevar id ->
    eval_var env (id |> Slambdaident.of_ident)
    |> expect_not_missing |> expect Tlayout
  | Punboxed_product old_layouts ->
    let new_layouts =
      Misc.Stdlib.List.map_sharing (eval_layout env) old_layouts
    in
    if new_layouts == old_layouts then layout else Punboxed_product new_layouts
  | Ptop | Pvalue _ | Punboxed_float _ | Punboxed_or_untagged_integer _
  | Punboxed_vector _ | Pbottom ->
    layout

and eval_lfunction_shallow env
    ({ kind;
       params = old_params;
       return = old_return;
       body;
       attr;
       loc;
       mode;
       ret_mode
     } as lfunction) =
  let eval_lparam
      ({ name; debug_uid; layout = old_layout; attributes; mode } as lparam) =
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout
    then lparam
    else { name; debug_uid; layout = new_layout; attributes; mode }
  in
  let new_params = Misc.Stdlib.List.map_sharing eval_lparam old_params in
  let new_return = eval_layout env old_return in
  if new_params == old_params && new_return == old_return
  then lfunction
  else
    lfunction' ~kind ~params:new_params ~return:new_return ~body ~attr ~loc
      ~mode ~ret_mode

and eval_prim env prim =
  match prim with
  | Pmakeblock (n, mut, old_shape, mode) ->
    let new_shape = eval_block_shape env old_shape in
    if new_shape == old_shape then prim else Pmakeblock (n, mut, new_shape, mode)
  | Pmixedfield (is, old_shape, sem) ->
    let new_shape = eval_mixed_block_shape env old_shape in
    if new_shape == old_shape then prim else Pmixedfield (is, new_shape, sem)
  | Psetmixedfield (is, old_shape, init_or_assign) ->
    let new_shape = eval_mixed_block_shape env old_shape in
    if new_shape == old_shape
    then prim
    else Psetmixedfield (is, new_shape, init_or_assign)
  | Pmake_unboxed_product old_layouts ->
    let new_layouts =
      Misc.Stdlib.List.map_sharing (eval_layout env) old_layouts
    in
    if new_layouts == old_layouts
    then prim
    else Pmake_unboxed_product new_layouts
  | Punboxed_product_field (i, old_layouts) ->
    let new_layouts =
      Misc.Stdlib.List.map_sharing (eval_layout env) old_layouts
    in
    if new_layouts == old_layouts
    then prim
    else Punboxed_product_field (i, new_layouts)
  | Pmake_idx_mixed_field (old_shape, i, path) ->
    let new_shape = eval_mixed_block_shape env old_shape in
    if new_shape == old_shape
    then prim
    else Pmake_idx_mixed_field (new_shape, i, path)
  | Pmake_idx_array (kind, index_kind, old_element, path) ->
    let new_element = eval_mixed_block_element env old_element in
    if new_element == old_element
    then prim
    else Pmake_idx_array (kind, index_kind, new_element, path)
  | Pidx_deepen (old_element, path) ->
    let new_element = eval_mixed_block_element env old_element in
    if new_element == old_element then prim else Pidx_deepen (new_element, path)
  | Popaque old_layout ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout then prim else Popaque new_layout
  | Pobj_magic old_layout ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout then prim else Pobj_magic new_layout
  | Pget_idx (old_layout, mut) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout then prim else Pget_idx (new_layout, mut)
  | Pset_idx (old_layout, mode) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout then prim else Pset_idx (new_layout, mode)
  | Pget_ptr (old_layout, mut) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout then prim else Pget_ptr (new_layout, mut)
  | Pset_ptr (old_layout, mode) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout then prim else Pset_ptr (new_layout, mode)
  | Pbytes_to_string | Pbytes_of_string | Pignore | Pgetglobal _ | Pgetpredef _
  | Pmakefloatblock _ | Pmakeufloatblock _ | Pmakelazyblock _ | Pfield _
  | Pfield_computed _ | Psetfield _ | Psetfield_computed _ | Pfloatfield _
  | Psetfloatfield _ | Psetufloatfield _ | Pufloatfield _ | Pduprecord _
  | Parray_element_size_in_bytes _ | Pmake_idx_field _ | Pwith_stack
  | Pwith_stack_bind | Pwith_stack_preemptible | Pwith_stack_bind_preemptible
  | Pperform | Presume | Preperform | Pccall _ | Praise _
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
      (try assert_no_splices halves.slv_runtime
       with Found_a_splice ->
         Misc.fatal_error
           "Encountered a splice in the program after slambda eval");
      halves)
