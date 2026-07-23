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
module Fmt = Format_doc

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
    | SLVclosure of Template_store.id

  val print_closure : Fmt.formatter -> closure -> unit

  val print_value_or_missing : Fmt.formatter -> value Or_missing.t -> unit
end = struct
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
    | SLVclosure of Template_store.id

  let print_closure ppf { clo_params; clo_body; clo_env = _ } =
    let print_params ppf =
      Array.iter
        (fun id ->
          Fmt.fprintf ppf "%a@ " (Fmt.deprecated Slambdaident.print) id)
        clo_params
    in
    Fmt.fprintf ppf "@[<2>(closure ⟨env⟩@ @[<2>%t->@]@ %a)@]" print_params
      (Fmt.deprecated Printlambda.slambda)
      clo_body

  let rec print_value ppf = function
    | SLVhalves { slv_comptime; slv_runtime } ->
      Fmt.fprintf ppf "@[<hv 2>{ c = %a;@ r = ⟪ %a ⟫ }@]" print_value_or_missing
        slv_comptime
        (Fmt.deprecated Printlambda.lambda)
        slv_runtime
    | SLVlayout layout ->
      Fmt.fprintf ppf "⟪%a⟫" (Fmt.deprecated Printlambda.layout) layout
    | SLVrecord fields ->
      let print_fields ppf =
        Array.iter
          (fun field -> Fmt.fprintf ppf "%a;@ " print_value_or_missing field)
          fields
      in
      Fmt.fprintf ppf "@[<hv 2>[@ %t]@]" print_fields
    | SLVclosure id -> Template_store.print_id ppf id

  and print_value_or_missing ppf = function
    | Or_missing.Missing -> Fmt.fprintf ppf "(missing)"
    | Or_missing.Present v -> print_value ppf v
end

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

and Template_store : sig
  type id

  type templates

  type t

  val empty : unit -> t

  val empty_templates : unit -> templates

  val add :
    t ->
    cu:Compilation_unit.t ->
    name:Slambdaident.t option ->
    Types.closure ->
    id

  (** Adds all templates from the second argument into the first. *)
  val add_all_templates : templates -> templates -> unit

  val add_foreign_templates : t -> templates -> unit

  val instantiate :
    t ->
    id ->
    Types.value array ->
    (Types.closure -> Types.value array -> lambda) ->
    Types.value Or_missing.t

  val templates : t -> templates

  val instantiations : t -> (Ident.t * lambda) list

  val print_id : Fmt.formatter -> id -> unit

  val print_templates : Fmt.formatter -> templates -> unit
end = struct
  type id = string

  type templates = Types.closure Misc.Stdlib.String.Tbl.t

  type t =
    { templates : templates;
      foreign_templates : templates;
      instantiations : lambda Ident.Tbl.t
    }

  let stamp = ref 0

  let empty () =
    { templates = Misc.Stdlib.String.Tbl.create 10;
      foreign_templates = Misc.Stdlib.String.Tbl.create 10;
      instantiations = Ident.Tbl.create 10
    }

  let empty_templates () = Misc.Stdlib.String.Tbl.create 0

  let add t ~cu ~name closure =
    let id =
      match name with
      | Some name ->
        Fmt.asprintf "%a_%s_%i" Compilation_unit.print cu
          (Slambdaident.name name) !stamp
      | None -> Fmt.asprintf "%a_%i" Compilation_unit.print cu !stamp
    in
    incr stamp;
    Misc.Stdlib.String.Tbl.add t.templates id closure;
    id

  let add_all_templates target source =
    Misc.Stdlib.String.Tbl.iter
      (fun id closure ->
        if Misc.Stdlib.String.Tbl.mem target id
        then Misc.fatal_errorf "duplicate template id: %s" id;
        Misc.Stdlib.String.Tbl.add target id closure)
      source

  let add_foreign_templates t templates =
    add_all_templates t.foreign_templates templates

  let find_template t id =
    try Misc.Stdlib.String.Tbl.find t.templates id
    with Not_found -> (
      try Misc.Stdlib.String.Tbl.find t.foreign_templates id
      with Not_found -> Misc.fatal_error ("Template not found: " ^ id))

  let symbol_arg_of_value_kind_non_null = function
    | Pintval -> "immediate"
    | Pgenval | Pboxedfloatval _ | Pboxedintval _ | Pvariant _ | Parrayval _
    | Pboxedvectorval _ ->
      "value"

  let rec symbol_arg_of_value_kind { raw_kind; nullable } =
    let kind = symbol_arg_of_value_kind_non_null raw_kind in
    let nullable =
      match nullable with Nullable -> "_or_null" | Non_nullable -> ""
    in
    kind ^ nullable

  and symbol_arg_of_unboxed_float = function
    | Unboxed_float64 -> "float64"
    | Unboxed_float32 -> "float32"

  and symbol_arg_of_unboxed_or_untagged_integer = function
    | Unboxed_int64 -> "int64"
    | Unboxed_nativeint -> "nativeint"
    | Unboxed_int32 -> "int32"
    | Untagged_int16 -> "int16"
    | Untagged_int8 -> "int8"
    | Untagged_int -> "int"

  and symbol_arg_of_unboxed_vector = function
    | Unboxed_vec128 -> "vec128"
    | Unboxed_vec256 -> "vec256"
    | Unboxed_vec512 -> "vec512"

  and symbol_arg_of_unboxed_product layouts =
    (* CR layout poly: this should be synced up with unarize. *)
    "(" ^ String.concat "_" (List.map symbol_arg_of_layout layouts) ^ ")"

  and symbol_arg_of_layout = function
    | Pvalue vk -> symbol_arg_of_value_kind vk
    | Punboxed_float uf -> symbol_arg_of_unboxed_float uf
    | Punboxed_or_untagged_integer ui ->
      symbol_arg_of_unboxed_or_untagged_integer ui
    | Punboxed_vector uv -> symbol_arg_of_unboxed_vector uv
    | Punboxed_product layouts -> symbol_arg_of_unboxed_product layouts
    | Ptop | Pbottom | Psplicevar _ ->
      Misc.fatal_error "Slambda_types.symbol_arg_of_layout: unexpected layout"

  let symbol_arg_of_value (v : Types.value) =
    match v with
    | SLVlayout l -> symbol_arg_of_layout l
    | SLVhalves _ | SLVrecord _ | SLVclosure _ ->
      Misc.fatal_error "Slambda_types.symbol_arg_of_value: unexpected value"

  let instantiate t id args f : Types.value Or_missing.t =
    let closure = find_template t id in
    let arg_names = Array.map symbol_arg_of_value args |> Array.to_list in
    let name = Ident.create_persistent (String.concat "_" (id :: arg_names)) in
    let _ = Ident.Tbl.memoize t.instantiations (fun _ -> f closure args) name in
    Present (SLVhalves { slv_comptime = Missing; slv_runtime = Lvar name })

  let templates t = t.templates

  let instantiations t = Ident.Tbl.to_list t.instantiations

  let print_id = Fmt.pp_print_string

  let print_templates ppf templates =
    if Misc.Stdlib.String.Tbl.length templates = 0
    then ()
    else begin
      Fmt.fprintf ppf "@ @[<hv>";
      Misc.Stdlib.String.Tbl.iter
        (fun id closure ->
          Fmt.fprintf ppf "@[<2>(%s@ %a)@]" id Types.print_closure closure)
        templates;
      Fmt.fprintf ppf "@]"
    end
end

include Types

type closure = Template_store.id

let print = print_value_or_missing

module CU_data = struct
  type t =
    { templates : Template_store.templates;
      cu : value Or_missing.t
    }

  type raw = File_sections.Idx.t

  let read raw ~sections = Obj.obj (File_sections.get sections raw)

  let write t ~sections = File_sections.Builder.add sections (Obj.repr t)

  let package ts =
    let templates = Template_store.empty_templates () in
    Array.iter
      (fun t -> Template_store.add_all_templates templates t.templates)
      ts;
    let cu = Or_missing.Present (SLVrecord (Array.map (fun t -> t.cu) ts)) in
    { templates; cu }

  let print ppf { templates; cu } =
    Fmt.fprintf ppf "@[<v 0>%a%a@]" print cu Template_store.print_templates
      templates
end

module Ctx = struct
  type t =
    { cu_static_data_getter : Compilation_unit.t -> CU_data.t option;
      cu_static_data : value Or_missing.t Compilation_unit.Tbl.t;
      store : Template_store.t
    }

  let create ~cu_static_data =
    { cu_static_data_getter = cu_static_data;
      cu_static_data = Compilation_unit.Tbl.create 0;
      store = Template_store.empty ()
    }

  let cu_static_data t cu =
    Compilation_unit.Tbl.memoize t.cu_static_data
      (fun cu ->
        match t.cu_static_data_getter cu with
        | Some { CU_data.templates; cu } ->
          Template_store.add_foreign_templates t.store templates;
          cu
        | None -> Or_missing.Missing)
      cu
end

let errf fmt = Misc.fatal_errorf ("slambda eval: " ^^ fmt)

type _ value_type =
  | Thalves : halves value_type
  | Tlayout : layout value_type
  | Trecord : value Or_missing.t array value_type
  | Tclosure : Template_store.id value_type

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

let rec eval_slam ?name (ctx : Ctx.t) env slam : value Or_missing.t =
  match slam with
  | SLhalves { sval_comptime; sval_runtime } ->
    let slv_comptime = eval_slam ?name ctx env sval_comptime in
    let slv_runtime = eval_lam ctx env sval_runtime in
    Present (SLVhalves { slv_comptime; slv_runtime })
  | SLlayout layout -> Present (SLVlayout (eval_layout env layout))
  | SLglobal cu -> Ctx.cu_static_data ctx cu
  | SLvar id -> eval_var env id
  | SLlet { slet_name; slet_value; slet_body } ->
    let value = eval_slam ~name:slet_name ctx env slet_value in
    let env_body = Env.add env slet_name value in
    eval_slam ?name ctx env_body slet_body
  | SLmissing -> Missing
  | SLrecord slams ->
    let values = Array.map (eval_slam ctx env) (Array.of_list slams) in
    Present (SLVrecord values)
  | SLfield (slam, i) ->
    let* fields = eval_slam ctx env slam |>> expect Trecord in
    fields.(i)
  | SLproj_comptime slam ->
    let* halves = eval_slam ?name ctx env slam |>> expect Thalves in
    halves.slv_comptime
  | SLtemplate { sfun_params; sfun_body } ->
    let closure =
      { clo_params = sfun_params; clo_body = sfun_body; clo_env = env }
    in
    let cu = Current_unit.get_cu_exn () in
    let closure_id = Template_store.add ctx.store ~cu ~name closure in
    Present (SLVclosure closure_id)
  | SLinstantiate { sapp_func; sapp_args } ->
    let closure =
      eval_slam ctx env sapp_func |> expect_not_missing |> expect Tclosure
    in
    let eval_arg arg = eval_slam ctx env arg |> expect_not_missing in
    let args = Array.map eval_arg sapp_args in
    Template_store.instantiate ctx.store closure args
      (fun { clo_params; clo_body; clo_env } args ->
        let env_body =
          Misc.Stdlib.Array.fold_left2 Env.add_present clo_env clo_params args
        in
        let { slv_comptime = _; slv_runtime } =
          eval_slam ctx env_body clo_body
          |> expect_not_missing |> expect Thalves
        in
        slv_runtime)

and eval_var env id = Env.find env id

and eval_lam ctx env lam = Lambda.map (eval_lam_shallow ctx env) lam

and eval_lam_shallow ctx env lam =
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
    let halves =
      eval_slam ctx env slam |> expect_not_missing |> expect Thalves
    in
    halves.slv_runtime
  | Lkindtemplate _ | Lkindinstantiate _ ->
    (* These constructors only exist in tlambda, fracturing has removed them
       (and replaced them with SLtemplate and SLinstantiate). *)
    Lambda.fatal_error_invalid_constructor lam
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
    eval_var env id |> expect_not_missing |> expect Tlayout
    |> mixed_block_element_of_layout
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
  | Psplicevar id -> eval_var env id |> expect_not_missing |> expect Tlayout
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
  | Pget_ext_ptr (old_layout, mut) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout then prim else Pget_ext_ptr (new_layout, mut)
  | Pset_ext_ptr (old_layout, mode) ->
    let new_layout = eval_layout env old_layout in
    if new_layout == old_layout then prim else Pset_ext_ptr (new_layout, mode)
  | Pbytes_to_string | Pbytes_of_string | Pignore | Pgetglobal _ | Pgetpredef _
  | Pmakefloatblock _ | Pmakeufloatblock _ | Pmakelazyblock _ | Pfield _
  | Pfield_computed _ | Psetfield _ | Psetfield_computed _ | Pfloatfield _
  | Psetfloatfield _ | Psetufloatfield _ | Pufloatfield _ | Pduprecord _
  | Parray_element_size_in_bytes _ | Pmake_idx_field _ | Pwith_stack
  | Pwith_stack_preemptible | Pperform | Pcontinue | Pdiscontinue
  | Pdiscontinue_with_backtrace | Preperform | Pccall _ | Praise _ | Psequand
  | Psequor | Pnot | Pphys_equal _ | Pscalar _ | Poffsetref _ | Pstringlength
  | Pstringrefu | Pstringrefs | Pbyteslength | Pbytesrefu | Pbytessetu
  | Pbytesrefs | Pbytessets | Pmakearray _ | Pmakearray_dynamic _ | Pduparray _
  | Parrayblit _ | Parraylength _ | Parrayrefu _ | Parraysetu _ | Parrayrefs _
  | Parraysets _ | Pisint _ | Pisnull | Pisout | Pbigarrayref _ | Pbigarrayset _
  | Pbigarraydim _ | Pstring_load_i8 _ | Pstring_load_i16 _ | Pstring_load_16 _
  | Pstring_load_32 _ | Pstring_load_f32 _ | Pstring_load_64 _
  | Pstring_load_vec _ | Pbytes_load_i8 _ | Pbytes_load_i16 _ | Pbytes_load_16 _
  | Pbytes_load_32 _ | Pbytes_load_f32 _ | Pbytes_load_64 _ | Pbytes_load_vec _
  | Pbytes_set_8 _ | Pbytes_set_16 _ | Pbytes_set_32 _ | Pbytes_set_f32 _
  | Pbytes_set_64 _ | Pbytes_set_vec _ | Pbigstring_load_i8 _
  | Pbigstring_load_i16 _ | Pbigstring_load_16 _ | Pbigstring_load_32 _
  | Pbigstring_load_f32 _ | Pbigstring_load_64 _ | Pbigstring_load_vec _
  | Pbigstring_set_8 _ | Pbigstring_set_16 _ | Pbigstring_set_32 _
  | Pbigstring_set_f32 _ | Pbigstring_set_64 _ | Pbigstring_set_vec _
  | Pfloatarray_load_vec _ | Pint_array_load_vec _
  | Punboxed_float_array_load_vec _ | Punboxed_float32_array_load_vec _
  | Puntagged_int8_array_load_vec _ | Puntagged_int16_array_load_vec _
  | Punboxed_int32_array_load_vec _ | Punboxed_int64_array_load_vec _
  | Punboxed_nativeint_array_load_vec _ | Pfloatarray_set_vec _
  | Pint_array_set_vec _ | Punboxed_float_array_set_vec _
  | Punboxed_float32_array_set_vec _ | Puntagged_int8_array_set_vec _
  | Puntagged_int16_array_set_vec _ | Punboxed_int32_array_set_vec _
  | Punboxed_int64_array_set_vec _ | Punboxed_nativeint_array_set_vec _
  | Pctconst _ | Pint_as_pointer _ | Patomic_load_field _ | Patomic_set_field _
  | Patomic_exchange_field _ | Patomic_compare_exchange_field _
  | Patomic_compare_set_field _ | Patomic_fetch_add_field | Patomic_add_field
  | Patomic_sub_field | Patomic_land_field | Patomic_lor_field
  | Patomic_lxor_field | Pprobe_is_enabled _ | Pobj_dup | Punbox_unit
  | Punbox_vector _ | Pbox_vector _ | Pjoin_vec256 | Psplit_vec256
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
  | Pset_ptr (layout, _)
  | Pget_ext_ptr (layout, _)
  | Pset_ext_ptr (layout, _) ->
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
  | Lsplice _ -> raise Found_a_splice
  | Lkindtemplate _ | Lkindinstantiate _ ->
    Lambda.fatal_error_invalid_constructor lam);
  Lambda.iter_head_constructor assert_no_splices lam

let do_eval (ctx : Ctx.t) slam =
  let { slv_comptime; slv_runtime } =
    eval_slam ctx Env.empty slam
    |> expect_not_missing
    |> expect Thalves ~reason:"toplevel module"
  in
  let lambda =
    List.fold_left
      (fun lam (id, def) ->
        Llet (Strict, layout_function, id, debug_uid_none, def, lam))
      slv_runtime
      (Template_store.instantiations ctx.store)
  in
  { slv_comptime; slv_runtime = lambda }

let eval ~cu_static_data slam =
  Profile.record_call "static_eval" (fun () ->
      let ctx = Ctx.create ~cu_static_data in
      let { slv_comptime; slv_runtime } = do_eval ctx slam in
      (try assert_no_splices slv_runtime
       with Found_a_splice ->
         Misc.fatal_error
           "Encountered a splice in the program after slambda eval");
      ( { CU_data.templates = Template_store.templates ctx.store;
          cu = slv_comptime
        },
        slv_runtime ))
