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

module Template_id = struct
  (* The owner+stamp combination is globally unique (at least within one linked
     unit). This fact is used to guarantee that the symbol names are unique. *)
  type t =
    { owner : Compilation_unit.t option;
      stamp : int;
      name : Ident.t option
    }

  let stamp = ref 0

  let create ~owner ~name =
    let t = { owner; stamp = !stamp; name } in
    incr stamp;
    t

  let print ppf t =
    Fmt.fprintf ppf "%a/%a/%i"
      (Fmt.pp_print_option Compilation_unit.print)
      t.owner
      (Fmt.pp_print_option (fun ppf id ->
           Fmt.pp_print_string ppf (Ident.name id)))
      t.name t.stamp

  let equal t1 t2 =
    t1.stamp = t2.stamp && Option.equal Compilation_unit.equal t1.owner t2.owner

  let hash t =
    match t.owner with
    | Some owner -> Hashtbl.hash (Compilation_unit.hash owner, t.stamp)
    | None -> t.stamp

  module Tbl = Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal

    let hash = hash
  end)
end

module rec Types : sig
  type template =
    | Kind of lkindtemplate
    | Static of ltemplate

  type closure =
    { clo_template : template;
      clo_env : Env.t
    }

  type halves =
    { slv_comptime : value Or_missing.t;
      slv_runtime : lambda
    }

  and record =
    { id : string;
      values : value Or_missing.t array
    }

  and value =
    | Vlayout of layout
    | Vrecord of record
    | Vclosure of Template_id.t

  val print_closure : Fmt.formatter -> closure -> unit

  val print_value_or_missing : Fmt.formatter -> value Or_missing.t -> unit
end = struct
  type template =
    | Kind of lkindtemplate
    | Static of ltemplate

  type closure =
    { clo_template : template;
      clo_env : Env.t
    }

  type halves =
    { slv_comptime : value Or_missing.t;
      slv_runtime : lambda
    }

  and record =
    { id : string;
      values : value Or_missing.t array
    }

  and value =
    | Vlayout of layout
    | Vrecord of record
    | Vclosure of Template_id.t

  let print_closure ppf { clo_template; clo_env = _ } =
    let body =
      match clo_template with
      | Kind template -> Lkindtemplate template
      | Static template -> Ltemplate template
    in
    Fmt.fprintf ppf "@[<2>(closure ⟨env⟩@ %a)@]"
      (Fmt.deprecated Printlambda.lambda)
      body

  let rec print_value ppf = function
    | Vlayout layout ->
      Fmt.fprintf ppf "⟪%a⟫" (Fmt.deprecated Printlambda.layout) layout
    | Vrecord { id; values } ->
      let print_fields ppf =
        Array.iter
          (fun field -> Fmt.fprintf ppf "@ %a;" print_value_or_missing field)
          values
      in
      Fmt.fprintf ppf "@[<hv 2>%s[%t@;<1 -2>]@]" id print_fields
    | Vclosure id -> Template_id.print ppf id

  and print_value_or_missing ppf = function
    | Or_missing.Missing -> Fmt.fprintf ppf "(missing)"
    | Or_missing.Present v -> print_value ppf v
end

and Env : sig
  type t

  val empty : t

  val add : t -> Slambdaident.t -> Types.value Or_missing.t -> t

  val find : t -> Slambdaident.t -> Types.value Or_missing.t
end = struct
  module Map = Slambdaident.Map

  type t = Types.value Map.t

  let empty = Map.empty

  let add t id v =
    match (v : Types.value Or_missing.t) with
    | Present v -> Map.add id v t
    | Missing -> (* Possibly unnecessary but be safe anyway *) Map.remove id t

  let find t id = Map.find_opt id t |> Or_missing.of_option
end

module Template_store = struct
  type t = Types.closure Template_id.Tbl.t

  let empty () = Template_id.Tbl.create 10

  let add t ~cu ~name closure =
    let id = Template_id.create ~owner:cu ~name in
    Template_id.Tbl.add t id closure;
    id

  let find_template t id = Template_id.Tbl.find_opt t id

  let print ppf t =
    if Template_id.Tbl.length t = 0
    then ()
    else begin
      Template_id.Tbl.iter
        (fun id closure ->
          Fmt.fprintf ppf "@ @[<2>(%a@ %a)@]" Template_id.print id
            Types.print_closure closure)
        t
    end
end

module Mangling : sig
  val symbol_arg_of_value : Types.value Or_missing.t -> string
end = struct
  let symbol_arg_of_value_kind_non_null = function
    | Pintval -> "immediate"
    | Pgenval | Pboxedfloatval _ | Pboxedintval _ | Pvariant _ | Parrayval _
    | Pboxedvectorval _ | Pboxedmaskval ->
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
    | Punboxed_mask -> "mask"
    | Ptop | Pbottom | Psplicevar _ ->
      Misc.fatal_error "Slambda.symbol_arg_of_layout: unexpected layout"

  let symbol_arg_of_value (v : Types.value Or_missing.t) =
    match v with
    | Missing -> "missing"
    | Present (Vlayout l) -> symbol_arg_of_layout l
    | Present (Vrecord { id; values = _ }) -> id
    | Present (Vclosure id) -> Fmt.asprintf "%a" Template_id.print id
end

module CU_data = struct
  type t =
    { templates : Template_store.t;
      cu : Types.value Or_missing.t
    }

  type raw = File_sections.Idx.t

  let empty () = { templates = Template_store.empty (); cu = Missing }

  let read raw ~sections = Obj.obj (File_sections.get sections raw)

  let write t ~sections = File_sections.Builder.add sections (Obj.repr t)

  let print ppf { templates; cu } =
    Fmt.fprintf ppf "@[<v 0>%a%a@]" Types.print_value_or_missing cu
      Template_store.print templates
end

module Ctx : sig
  type t

  (** [cu_static_data] is used to look up the [CU_data.t] for a give compilation
      unit, calls to it are memoized. *)
  val create : cu_static_data:(Compilation_unit.t -> CU_data.t) -> t

  (** Memoized fetch of the compile-time data for the given unit. *)
  val cu_static_data : t -> Compilation_unit.t -> Types.value Or_missing.t

  (** A template store, used to store the templates for the current unit. *)
  val store : t -> Template_store.t

  (** Instantiate a template. This is memoized so if this template has already
      been instantiated with these arguments it just returns the previously
      computed results, otherwise it uses [eval_apply] to evaluate the closure.
      The returned runtime half is a reference to the instantiated function. *)
  val instantiate :
    t ->
    eval_apply:(Types.closure -> Types.value Or_missing.t array -> Types.halves) ->
    Template_id.t ->
    Types.value Or_missing.t array ->
    Types.halves

  (** All of the template instantiations cached by [instantiate]. These are in
      dependency order; entries earlier in the list may depend on later ones. *)
  val instantiations : t -> (Ident.t * lambda) list

  (** Makes the given string unique in the context of this [Ctx.t] by adding a
      stamp to the end. It should already be unique across [Ctx.t]s, which is
      usually achievable by including the [Compilation_unit.t]. *)
  val uniqueify : t -> string -> string
end = struct
  type t =
    { cu_static_data : Compilation_unit.t -> CU_data.t;
      store : Template_store.t;
      instantiated_templates : Types.value Or_missing.t option Ident.Tbl.t;
      mutable instantiations : (Ident.t * lambda) list;
      uniqueify : int Misc.Stdlib.String.Tbl.t
    }

  let create ~cu_static_data =
    let cu_data_cache = Compilation_unit.Tbl.create 0 in
    { cu_static_data =
        (fun cu -> Compilation_unit.Tbl.memoize cu_data_cache cu_static_data cu);
      store = Template_store.empty ();
      instantiated_templates = Ident.Tbl.create 10;
      instantiations = [];
      uniqueify = Misc.Stdlib.String.Tbl.create 10
    }

  let cu_static_data t cu = (t.cu_static_data cu).cu

  let store t = t.store

  let instantiate t ~eval_apply (id : Template_id.t) args =
    let closure =
      match Template_store.find_template t.store id with
      | Some closure -> closure
      | None -> (
        let cu_data = Option.map t.cu_static_data id.owner in
        let closure =
          Option.bind cu_data (fun { CU_data.templates; _ } ->
              Template_store.find_template templates id)
        in
        match closure with
        | Some closure -> closure
        | None ->
          Misc.fatal_errorf_doc "Template not found: %a" Template_id.print id)
    in
    let arg_names =
      Array.map Mangling.symbol_arg_of_value args |> Array.to_list
    in
    let name =
      Fmt.asprintf "%a_%a" Template_id.print id
        (Fmt.pp_print_list
           ~pp_sep:(fun ppf () -> Fmt.pp_print_string ppf "_")
           Fmt.pp_print_string)
        arg_names
      |> Ident.create_persistent
    in
    let slv_comptime =
      match Ident.Tbl.find_opt t.instantiated_templates name with
      | Some (Some value) -> value
      | Some None ->
        Misc.fatal_errorf "Recursive template instantiation of %a" Ident.print
          name
      | None -> begin
        (* eval_apply might recursively call this function so mark this name as
           visited before calling it. *)
        Ident.Tbl.replace t.instantiated_templates name None;
        let { Types.slv_comptime; slv_runtime } = eval_apply closure args in
        Ident.Tbl.replace t.instantiated_templates name (Some slv_comptime);
        let instantiation =
          Lambda.subst
            (fun _ _ env -> env)
            ~freshen_bound_variables:true Ident.Map.empty slv_runtime
        in
        t.instantiations <- (name, instantiation) :: t.instantiations;
        slv_comptime
        end
    in
    { Types.slv_comptime; slv_runtime = Lvar name }

  let instantiations t = t.instantiations

  let uniqueify t id =
    let counter = Misc.Stdlib.String.Tbl.find_opt t.uniqueify id in
    let counter = Option.value counter ~default:0 in
    Misc.Stdlib.String.Tbl.replace t.uniqueify id (counter + 1);
    Fmt.asprintf "%s/%i" id counter
end

include Types

let errf fmt = Misc.fatal_errorf ("slambda eval: " ^^ fmt)

type _ value_type =
  | Tlayout : layout value_type
  | Trecord : record value_type
  | Tclosure : Template_id.t value_type

let describe_value_type (type a) : a value_type -> string = function
  | Tlayout -> "layout value"
  | Trecord -> "record"
  | Tclosure -> "template"

type value_type_packed = TP : _ value_type -> value_type_packed

let typeof = function
  | Vlayout _ -> TP Tlayout
  | Vrecord _ -> TP Trecord
  | Vclosure _ -> TP Tclosure

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
  | Tlayout, Vlayout layout -> layout
  | Trecord, Vrecord record -> record
  | Tclosure, Vclosure closure -> closure
  | _, _ ->
    let (TP actual_vty) = typeof v in
    expect_err ?reason ~expected:vty ~actual:actual_vty

let expect_not_missing (a : 'a Or_missing.t) : 'a =
  match a with Present a -> a | Missing -> errf "unexpected missing value"

let eval_var env id = Env.find env id

let rec eval_structured_const env const =
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
  | Bits64 | Vec128 | Vec256 | Vec512 | Mask | Word | Untagged_immediate ->
    element

and eval_layout env layout =
  match layout with
  | Psplicevar id -> eval_var env id |> expect_not_missing |> expect Tlayout
  | Punboxed_product old_layouts ->
    let new_layouts =
      Misc.Stdlib.List.map_sharing (eval_layout env) old_layouts
    in
    if new_layouts == old_layouts then layout else Punboxed_product new_layouts
  | Pvalue old_value_kind ->
    let new_value_kind = eval_value_kind env old_value_kind in
    if new_value_kind == old_value_kind then layout else Pvalue new_value_kind
  | Ptop | Punboxed_float _ | Punboxed_or_untagged_integer _ | Punboxed_vector _
  | Punboxed_mask | Pbottom ->
    layout

and eval_value_kind env ({ raw_kind = old_raw_kind; nullable } as value_kind) =
  let new_raw_kind = eval_raw_value_kind env old_raw_kind in
  if new_raw_kind == old_raw_kind
  then value_kind
  else { raw_kind = new_raw_kind; nullable }

and eval_raw_value_kind env value_kind =
  match value_kind with
  | Pvariant { consts; non_consts = old_non_consts } ->
    let new_non_consts =
      Misc.Stdlib.List.map_sharing
        (fun ((i, old_constructor_shape) as non_const) ->
          let new_constructor_shape =
            eval_constructor_shape env old_constructor_shape
          in
          if new_constructor_shape == old_constructor_shape
          then non_const
          else i, new_constructor_shape)
        old_non_consts
    in
    if new_non_consts == old_non_consts
    then value_kind
    else Pvariant { consts; non_consts = new_non_consts }
  | Pgenval | Pintval | Pboxedfloatval _ | Pboxedintval _ | Parrayval _
  | Pboxedvectorval _ | Pboxedmaskval ->
    value_kind

and eval_constructor_shape env constructor_shape =
  match constructor_shape with
  | Constructor_uniform old_value_kinds ->
    let new_value_kinds =
      Misc.Stdlib.List.map_sharing (eval_value_kind env) old_value_kinds
    in
    if new_value_kinds == old_value_kinds
    then constructor_shape
    else Constructor_uniform new_value_kinds
  | Constructor_mixed old_mixed_block_shape ->
    let new_mixed_block_shape =
      eval_mixed_block_shape env old_mixed_block_shape
    in
    if new_mixed_block_shape == old_mixed_block_shape
    then constructor_shape
    else Constructor_mixed new_mixed_block_shape

let eval_lparam env ({ name; debug_uid; layout; attributes; mode } as param) =
  let layout' = eval_layout env layout in
  if layout' == layout
  then param
  else { name; debug_uid; layout = layout'; attributes; mode }

let dynamic slv_runtime = { slv_comptime = Missing; slv_runtime }

let make_record ?name ctx fields : value Or_missing.t =
  if
    List.for_all
      (fun (field : value Or_missing.t) ->
        match field with Missing -> true | Present _ -> false)
      fields
  then Missing
  else
    let id =
      Fmt.asprintf "%a/%a"
        (Fmt.pp_print_option Compilation_unit.print)
        (Current_unit.get_cu ())
        (Fmt.pp_print_option Fmt.pp_print_string)
        (Option.map Ident.name name)
      |> Ctx.uniqueify ctx
    in
    Present (Vrecord { id; values = Array.of_list fields })

let project_field value pos =
  let* record = value |>> expect Trecord in
  record.values.(pos)

(* Capture expressions are evaluated at the template site. Instantiation uses
   only their names and layouts, rebinding the runtime names from the capture
   block; it must not evaluate those expressions again. *)
let close_function env captures env_mode capture_loc ~kind ~mode func body =
  let params = Misc.Stdlib.List.map_sharing (eval_lparam env) func.params in
  let return = eval_layout env func.return in
  let closure_id = Ident.create_local "closure" in
  let closure_param =
    { name = closure_id;
      debug_uid = debug_uid_none;
      layout = layout_template_env;
      attributes = default_param_attribute;
      mode = env_mode
    }
  in
  let captures =
    Ident.Map.bindings captures
    |> List.map (fun (id, (_, layout)) -> id, eval_layout env layout)
  in
  let shape =
    Misc.Stdlib.Array.of_list_map
      (fun (_, layout) -> mixed_block_element_of_layout layout)
      captures
  in
  let _, body =
    List.fold_left
      (fun (i, body) (id, layout) ->
        ( i + 1,
          Llet
            ( Alias,
              layout,
              id,
              debug_uid_none,
              Lprim
                ( Pmixedfield ([i], shape, Reads_agree),
                  [Lvar closure_id],
                  capture_loc ),
              body ) ))
      (0, body) captures
  in
  lfunction' ~kind ~params:(closure_param :: params) ~return ~body
    ~attr:func.attr ~loc:func.loc ~mode ~ret_mode:func.ret_mode
  |> lfunction_with_yielding func.yielding
  |> fun func -> Lfunction func

let rec eval_lam ?name ctx env lambda : halves =
  match lambda with
  | Lvar id ->
    { slv_comptime = eval_var env (Slambdaident.of_ident id);
      slv_runtime = lambda
    }
  | Lmutvar _ -> dynamic lambda
  | Lconst const ->
    let const' = eval_structured_const env const in
    dynamic (if const' == const then lambda else Lconst const')
  | Lapply ({ ap_func; ap_args; ap_result_layout; _ } as apply) ->
    let func = eval_dynamic ctx env ap_func in
    let args = eval_dynamic_list ctx env ap_args in
    let layout = eval_layout env ap_result_layout in
    dynamic
      (if func == ap_func && args == ap_args && layout == ap_result_layout
       then lambda
       else
         Lapply
           { apply with
             ap_func = func;
             ap_args = args;
             ap_result_layout = layout
           })
  | Lfunction func ->
    let func' = eval_lfunction ctx env func in
    dynamic (if func' == func then lambda else Lfunction func')
  | Llet (kind, layout, id, uid, def, body) ->
    let def' = eval_lam ~name:id ctx env def in
    let body_env = Env.add env (Slambdaident.of_ident id) def'.slv_comptime in
    let body' = eval_lam ?name ctx body_env body in
    let layout' = eval_layout env layout in
    { slv_comptime = body'.slv_comptime;
      slv_runtime =
        (if
           def'.slv_runtime == def && body'.slv_runtime == body
           && layout' == layout
         then lambda
         else Llet (kind, layout', id, uid, def'.slv_runtime, body'.slv_runtime))
    }
  | Lmutlet (layout, id, uid, def, body) ->
    let def' = eval_dynamic ctx env def in
    let body' = eval_lam ?name ctx env body in
    let layout' = eval_layout env layout in
    { slv_comptime = body'.slv_comptime;
      slv_runtime =
        (if def' == def && body'.slv_runtime == body && layout' == layout
         then lambda
         else Lmutlet (layout', id, uid, def', body'.slv_runtime))
    }
  | Lletrec (bindings, body) ->
    let bindings' =
      Misc.Stdlib.List.map_sharing
        (fun ({ def; _ } as binding) ->
          let def' = eval_lfunction ctx env def in
          if def == def' then binding else { binding with def = def' })
        bindings
    in
    let body' = eval_lam ?name ctx env body in
    { slv_comptime = body'.slv_comptime;
      slv_runtime =
        (if bindings' == bindings && body'.slv_runtime == body
         then lambda
         else Lletrec (bindings', body'.slv_runtime))
    }
  | Lprim (prim, args, loc) -> eval_prim ?name ctx env lambda prim args loc
  | Lswitch (arg, switch, loc, layout) ->
    let arg' = eval_dynamic ctx env arg in
    let consts = eval_cases ctx env switch.sw_consts in
    let blocks = eval_cases ctx env switch.sw_blocks in
    let failaction =
      Misc.Stdlib.Option.map_sharing (eval_dynamic ctx env) switch.sw_failaction
    in
    let switch' =
      if
        consts == switch.sw_consts && blocks == switch.sw_blocks
        && failaction == switch.sw_failaction
      then switch
      else
        { switch with
          sw_consts = consts;
          sw_blocks = blocks;
          sw_failaction = failaction
        }
    in
    let layout' = eval_layout env layout in
    dynamic
      (if arg' == arg && switch' == switch && layout' == layout
       then lambda
       else Lswitch (arg', switch', loc, layout'))
  | Lstringswitch (arg, cases, default, loc, layout) ->
    let arg' = eval_dynamic ctx env arg in
    let cases' = eval_cases ctx env cases in
    let default' =
      Misc.Stdlib.Option.map_sharing (eval_dynamic ctx env) default
    in
    let layout' = eval_layout env layout in
    dynamic
      (if
         arg' == arg && cases' == cases && default' == default
         && layout' == layout
       then lambda
       else Lstringswitch (arg', cases', default', loc, layout'))
  | Lstaticraise (label, args) ->
    let args' = eval_dynamic_list ctx env args in
    dynamic (if args' == args then lambda else Lstaticraise (label, args'))
  | Lstaticcatch (body, (label, params), handler, pop_region, layout) ->
    let body' = eval_dynamic ctx env body in
    let handler' = eval_dynamic ctx env handler in
    let params' =
      Misc.Stdlib.List.map_sharing
        (fun ((id, uid, layout) as param) ->
          let layout' = eval_layout env layout in
          if layout' == layout then param else id, uid, layout')
        params
    in
    let layout' = eval_layout env layout in
    dynamic
      (if
         body' == body && handler' == handler && params' == params
         && layout' == layout
       then lambda
       else Lstaticcatch (body', (label, params'), handler', pop_region, layout'))
  | Ltrywith (body, id, uid, handler, layout) ->
    let body' = eval_dynamic ctx env body in
    let handler' = eval_dynamic ctx env handler in
    let layout' = eval_layout env layout in
    dynamic
      (if body' == body && handler' == handler && layout' == layout
       then lambda
       else Ltrywith (body', id, uid, handler', layout'))
  | Lifthenelse (cond, ifso, ifnot, layout) ->
    let cond' = eval_dynamic ctx env cond in
    let ifso' = eval_dynamic ctx env ifso in
    let ifnot' = eval_dynamic ctx env ifnot in
    let layout' = eval_layout env layout in
    dynamic
      (if cond' == cond && ifso' == ifso && ifnot' == ifnot && layout' == layout
       then lambda
       else Lifthenelse (cond', ifso', ifnot', layout'))
  | Lsequence (left, right) ->
    let left' = eval_dynamic ctx env left in
    let right' = eval_lam ?name ctx env right in
    { slv_comptime = right'.slv_comptime;
      slv_runtime =
        (if left' == left && right'.slv_runtime == right
         then lambda
         else Lsequence (left', right'.slv_runtime))
    }
  | Lwhile { wh_cond; wh_body } ->
    let cond = eval_dynamic ctx env wh_cond in
    let body = eval_dynamic ctx env wh_body in
    dynamic
      (if cond == wh_cond && body == wh_body
       then lambda
       else Lwhile { wh_cond = cond; wh_body = body })
  | Lfor ({ for_from; for_to; for_body; _ } as loop) ->
    let from = eval_dynamic ctx env for_from in
    let to_ = eval_dynamic ctx env for_to in
    let body = eval_dynamic ctx env for_body in
    dynamic
      (if from == for_from && to_ == for_to && body == for_body
       then lambda
       else Lfor { loop with for_from = from; for_to = to_; for_body = body })
  | Lassign (id, value) ->
    let value' = eval_dynamic ctx env value in
    dynamic (if value' == value then lambda else Lassign (id, value'))
  | Lsend (kind, met, obj, args, region_close, mode, loc, layout, yielding) ->
    let met' = eval_dynamic ctx env met in
    let obj' = eval_dynamic ctx env obj in
    let args' = eval_dynamic_list ctx env args in
    let layout' = eval_layout env layout in
    dynamic
      (if met' == met && obj' == obj && args' == args && layout' == layout
       then lambda
       else
         Lsend
           (kind, met', obj', args', region_close, mode, loc, layout', yielding))
  | Levent (body, event) ->
    let body' = eval_lam ?name ctx env body in
    { body' with
      slv_runtime =
        (if body'.slv_runtime == body
         then lambda
         else Levent (body'.slv_runtime, event))
    }
  | Lifused (id, body) ->
    let body' = eval_lam ?name ctx env body in
    { body' with
      slv_runtime =
        (if body'.slv_runtime == body
         then lambda
         else Lifused (id, body'.slv_runtime))
    }
  | Lregion (body, layout) ->
    let body' = eval_lam ?name ctx env body in
    let layout' = eval_layout env layout in
    { body' with
      slv_runtime =
        (if body'.slv_runtime == body && layout' == layout
         then lambda
         else Lregion (body'.slv_runtime, layout'))
    }
  | Lexclave body ->
    let body' = eval_lam ?name ctx env body in
    { body' with
      slv_runtime =
        (if body'.slv_runtime == body
         then lambda
         else Lexclave body'.slv_runtime)
    }
  | Lkindtemplate template ->
    eval_template ?name ctx env (Kind template) template.ktmpl_env
      template.ktmpl_env_mode template.ktmpl_loc
  | Ltemplate template ->
    eval_template ?name ctx env (Static template) template.tmpl_env
      template.tmpl_func.mode template.tmpl_func.loc
  | Lkindinstantiate
      { kinst_func; kinst_args; kinst_result_layout; kinst_mode; kinst_loc } ->
    let func = eval_lam ctx env kinst_func in
    let args =
      Misc.Stdlib.Array.of_list_map
        (fun layout -> Or_missing.Present (Vlayout (eval_layout env layout)))
        kinst_args
    in
    let instantiated = instantiate ctx func.slv_comptime args in
    { slv_comptime = instantiated.slv_comptime;
      slv_runtime =
        Lapply
          { ap_func = instantiated.slv_runtime;
            ap_args = [func.slv_runtime];
            ap_result_layout = eval_layout env kinst_result_layout;
            ap_region_close = Rc_normal;
            ap_mode = kinst_mode;
            ap_yielding = Unyielding;
            ap_loc = kinst_loc;
            ap_tailcall = Default_tailcall;
            ap_inlined = Default_inlined;
            ap_specialised = Default_specialise;
            ap_probe = None
          }
    }
  | Linstantiate ({ ap_func; ap_args; ap_result_layout; _ } as apply) ->
    let func = eval_lam ctx env ap_func in
    let args_c, args_r = eval_args_reverse ctx env ap_args in
    let instantiated =
      instantiate ctx func.slv_comptime (Array.of_list args_c)
    in
    { slv_comptime = instantiated.slv_comptime;
      slv_runtime =
        Lapply
          { apply with
            ap_func = instantiated.slv_runtime;
            ap_args = func.slv_runtime :: args_r;
            ap_result_layout = eval_layout env ap_result_layout
          }
    }

and eval_lfunction ctx env
    ({ kind; params; return; body; attr; loc; mode; ret_mode; yielding } as func)
    =
  let body' = eval_dynamic ctx env body in
  let params' = Misc.Stdlib.List.map_sharing (eval_lparam env) params in
  let return' = eval_layout env return in
  if body' == body && params' == params && return' == return
  then func
  else
    lfunction' ~kind ~params:params' ~return:return' ~body:body' ~attr ~loc
      ~mode ~ret_mode
    |> lfunction_with_yielding yielding

and eval_dynamic ctx env lambda = (eval_lam ctx env lambda).slv_runtime

and eval_dynamic_list ctx env args =
  Misc.Stdlib.List.map_sharing (eval_dynamic ctx env) args

and eval_cases : 'a. Ctx.t -> Env.t -> ('a * lambda) list -> ('a * lambda) list
    =
 fun ctx env cases ->
  Misc.Stdlib.List.map_sharing
    (fun ((tag, body) as case) ->
      let body' = eval_dynamic ctx env body in
      if body' == body then case else tag, body')
    cases

(* Primitive fields and static arguments are expanded right-to-left, while
   preserving their original order in the residual code. *)
and eval_args_reverse ctx env args =
  let rec loop unchanged static runtime = function
    | [] -> static, if unchanged then args else runtime
    | arg :: rest ->
      let result = eval_lam ctx env arg in
      loop
        (unchanged && result.slv_runtime == arg)
        (result.slv_comptime :: static)
        (result.slv_runtime :: runtime)
        rest
  in
  loop true [] [] (List.rev args)

and eval_prim ?name ctx env lambda prim args loc =
  let wrong_arity expected =
    Misc.fatal_errorf "Slambda: %a takes exactly %d arguments, got %d"
      Printlambda.primitive prim expected (List.length args)
  in
  let args_c, args_r = eval_args_reverse ctx env args in
  let one_arg () = match args_c with [arg] -> arg | _ -> wrong_arity 1 in
  let result prim' slv_comptime =
    { slv_comptime;
      slv_runtime =
        (if prim' == prim && args_r == args
         then lambda
         else Lprim (prim', args_r, loc))
    }
  in
  let dynamic_prim prim' = result prim' Missing in
  match prim with
  | Pgetglobal (cu, Static) ->
    (match args with [] -> () | _ -> wrong_arity 0);
    result prim (Ctx.cu_static_data ctx cu)
  | Pmakeblock (n, mut, old_shape, mode) -> (
    let new_shape = eval_block_shape env old_shape in
    let prim' =
      if new_shape == old_shape
      then prim
      else Pmakeblock (n, mut, new_shape, mode)
    in
    match mut with
    | Immutable | Immutable_unique ->
      result prim' (make_record ?name ctx args_c)
    | Mutable -> dynamic_prim prim')
  | Pfield (pos, _, Reads_agree) ->
    let arg = one_arg () in
    result prim (project_field arg pos)
  | Pmixedfield (path, old_shape, sem) -> (
    let new_shape = eval_mixed_block_shape env old_shape in
    let prim' =
      if new_shape == old_shape then prim else Pmixedfield (path, new_shape, sem)
    in
    match sem with
    | Reads_agree ->
      let arg = one_arg () in
      result prim' (List.fold_left project_field arg path)
    | Reads_vary -> dynamic_prim prim')
  | Psetmixedfield (is, old_shape, init_or_assign) ->
    let new_shape = eval_mixed_block_shape env old_shape in
    dynamic_prim
      (if new_shape == old_shape
       then prim
       else Psetmixedfield (is, new_shape, init_or_assign))
  | Pmake_unboxed_product old_layouts ->
    let new_layouts =
      Misc.Stdlib.List.map_sharing (eval_layout env) old_layouts
    in
    dynamic_prim
      (if new_layouts == old_layouts
       then prim
       else Pmake_unboxed_product new_layouts)
  | Punboxed_product_field (i, old_layouts) ->
    let new_layouts =
      Misc.Stdlib.List.map_sharing (eval_layout env) old_layouts
    in
    dynamic_prim
      (if new_layouts == old_layouts
       then prim
       else Punboxed_product_field (i, new_layouts))
  | Pmake_idx_mixed_field (old_shape, i, path) ->
    let new_shape = eval_mixed_block_shape env old_shape in
    dynamic_prim
      (if new_shape == old_shape
       then prim
       else Pmake_idx_mixed_field (new_shape, i, path))
  | Pmake_idx_array (kind, index_kind, old_element, path) ->
    let new_element = eval_mixed_block_element env old_element in
    dynamic_prim
      (if new_element == old_element
       then prim
       else Pmake_idx_array (kind, index_kind, new_element, path))
  | Pidx_deepen (old_element, path) ->
    let new_element = eval_mixed_block_element env old_element in
    dynamic_prim
      (if new_element == old_element
       then prim
       else Pidx_deepen (new_element, path))
  | Popaque old_layout ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim (if new_layout == old_layout then prim else Popaque new_layout)
  | Pobj_magic old_layout ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then prim else Pobj_magic new_layout)
  | Pget_idx (old_layout, mut) ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then prim else Pget_idx (new_layout, mut))
  | Pset_idx (old_layout, mode) ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then prim else Pset_idx (new_layout, mode))
  | Pget_ptr (old_layout, mut) ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then prim else Pget_ptr (new_layout, mut))
  | Pset_ptr (old_layout, mode) ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then prim else Pset_ptr (new_layout, mode))
  | Pget_ext_ptr (old_layout, mut) ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then prim else Pget_ext_ptr (new_layout, mut))
  | Pset_ext_ptr (old_layout, mode) ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then prim else Pset_ext_ptr (new_layout, mode))
  | Patomic_load_idx { layout = old_layout } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then prim
       else Patomic_load_idx { layout = new_layout })
  | Patomic_set_idx { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then prim
       else Patomic_set_idx { layout = new_layout; mode })
  | Patomic_exchange_idx { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then prim
       else Patomic_exchange_idx { layout = new_layout; mode })
  | Patomic_compare_exchange_idx { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then prim
       else Patomic_compare_exchange_idx { layout = new_layout; mode })
  | Patomic_compare_set_idx { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then prim
       else Patomic_compare_set_idx { layout = new_layout; mode })
  | Patomic_load_ptr { layout = old_layout } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then prim
       else Patomic_load_ptr { layout = new_layout })
  | Patomic_set_ptr { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then prim
       else Patomic_set_ptr { layout = new_layout; mode })
  | Patomic_exchange_ptr { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then prim
       else Patomic_exchange_ptr { layout = new_layout; mode })
  | Patomic_compare_exchange_ptr { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then prim
       else Patomic_compare_exchange_ptr { layout = new_layout; mode })
  | Patomic_compare_set_ptr { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then prim
       else Patomic_compare_set_ptr { layout = new_layout; mode })
  | Pbytes_to_string | Pbytes_of_string | Pignore
  | Pgetglobal (_, Dynamic)
  | Pgetpredef _ | Pmakefloatblock _ | Pmakeufloatblock _ | Pmakelazyblock _
  | Pfield (_, _, Reads_vary)
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
  | Pstring_load_mask _ | Pbytes_load_32 _ | Pbytes_load_f32 _
  | Pbytes_load_64 _ | Pbytes_load_vec _ | Pbytes_load_mask _ | Pbytes_set_8 _
  | Pbytes_set_16 _ | Pbytes_set_32 _ | Pbytes_set_f32 _ | Pbytes_set_64 _
  | Pbytes_set_vec _ | Pbigstring_load_i8 _ | Pbytes_set_mask _
  | Pbigstring_load_i16 _ | Pbigstring_load_16 _ | Pbigstring_load_32 _
  | Pbigstring_load_f32 _ | Pbigstring_load_64 _ | Pbigstring_load_vec _
  | Pbigstring_load_mask _ | Pbigstring_set_8 _ | Pbigstring_set_16 _
  | Pbigstring_set_32 _ | Pbigstring_set_f32 _ | Pbigstring_set_64 _
  | Pbigstring_set_vec _ | Pbigstring_set_mask _ | Pfloatarray_load_vec _
  | Pint_array_load_vec _ | Punboxed_float_array_load_vec _
  | Punboxed_float32_array_load_vec _ | Puntagged_int8_array_load_vec _
  | Puntagged_int16_array_load_vec _ | Punboxed_int32_array_load_vec _
  | Punboxed_int64_array_load_vec _ | Punboxed_nativeint_array_load_vec _
  | Pfloatarray_set_vec _ | Pint_array_set_vec _
  | Punboxed_float_array_set_vec _ | Punboxed_float32_array_set_vec _
  | Puntagged_int8_array_set_vec _ | Puntagged_int16_array_set_vec _
  | Punboxed_int32_array_set_vec _ | Punboxed_int64_array_set_vec _
  | Punboxed_nativeint_array_set_vec _ | Pctconst _ | Pint_as_pointer _
  | Patomic_load_field _ | Patomic_load_mixed_field _ | Patomic_set_field _
  | Patomic_set_mixed_field _ | Patomic_exchange_field _
  | Patomic_compare_exchange_field _ | Patomic_compare_set_field _
  | Patomic_fetch_add_field | Patomic_add_field | Patomic_sub_field
  | Patomic_land_field | Patomic_lor_field | Patomic_lxor_field
  | Patomic_fetch_add_idx | Patomic_add_idx | Patomic_sub_idx | Patomic_land_idx
  | Patomic_lor_idx | Patomic_lxor_idx | Patomic_fetch_add_ptr | Patomic_add_ptr
  | Patomic_sub_ptr | Patomic_land_ptr | Patomic_lor_ptr | Patomic_lxor_ptr
  | Pprobe_is_enabled _ | Pobj_dup | Punbox_unit | Punbox_vector _
  | Pbox_vector _ | Punbox_mask | Pbox_mask _ | Pjoin_vec256 | Psplit_vec256
  | Preinterpret_boxed_vector_as_tuple _ | Preinterpret_tuple_as_boxed_vector _
  | Preinterpret_unboxed_int64_as_tagged_int63
  | Preinterpret_tagged_int63_as_unboxed_int64 | Parray_to_iarray
  | Parray_of_iarray | Pget_header _ | Ppeek _ | Ppoke _ | Pdls_get | Ptls_get
  | Pdomain_index | Ppoll | Pcpu_relax ->
    dynamic_prim prim

and eval_template ?name ctx env template captures mode loc =
  let captures = Ident.Map.bindings captures in
  let closure_env, args =
    List.fold_left
      (fun (env, args) (id, (def, _)) ->
        let def = eval_lam ~name:id ctx env def in
        ( Env.add env (Slambdaident.of_ident id) def.slv_comptime,
          def.slv_runtime :: args ))
      (env, []) (List.rev captures)
  in
  let shape =
    Misc.Stdlib.Array.of_list_map
      (fun (_, (_, layout)) ->
        eval_mixed_block_element env (mixed_block_element_of_layout layout))
      captures
  in
  let closure = { clo_template = template; clo_env = closure_env } in
  let id =
    Template_store.add (Ctx.store ctx) ~cu:(Current_unit.get_cu ()) ~name
      closure
  in
  { slv_comptime = Present (Vclosure id);
    slv_runtime = Lprim (Pmakeblock (0, Immutable, Shape shape, mode), args, loc)
  }

and instantiate ctx func args =
  let closure = func |> expect_not_missing |> expect Tclosure in
  Ctx.instantiate ctx closure args ~eval_apply:(eval_apply ctx)

and eval_apply ctx { clo_template; clo_env } args =
  let kind = match clo_template with
  | Kind { ktmpl_body; ktmpl_env_mode; _ } ->
    let kind =
      match ktmpl_body.kind with
      | Tupled ->
        Misc.fatal_error
          "Slambda does not currently support poly tupled functions"
      | Curried { nlocal } ->
        Curried
          { nlocal =
              (match ktmpl_env_mode with
              | Alloc_heap -> nlocal
              | Alloc_local -> List.length ktmpl_body.params + 1)
          }
    in
    if List.length ktmpl_body.params > Lambda.max_arity () - 1
    then
      Misc.fatal_errorf
        "Slambda does not currently support functions with over %i arguments"
        (Lambda.max_arity () - 1);
    kind
  | Static { tmpl_func; _ } -> (
    match tmpl_func.kind, tmpl_func.mode with
    | Curried { nlocal }, Alloc_local -> Curried { nlocal = nlocal + 1 }
    | Curried _, Alloc_heap -> tmpl_func.kind
    | Tupled, _ ->
      Misc.fatal_error
        "Tupled template functions are not supported, functors should always \
         be curried")
    in
  let bind_params params =
    try Misc.Stdlib.Array.fold_left2 Env.add clo_env params args
    with Invalid_argument _ ->
      Misc.fatal_error
        "Slambda eval doesn't support partial or over application of functors."
  in
  match clo_template with
  | Kind { ktmpl_params; ktmpl_body; ktmpl_env; ktmpl_env_mode; ktmpl_loc } ->
    let env = bind_params (Array.of_list ktmpl_params) in
    let body = eval_dynamic ctx env ktmpl_body.body in
    dynamic
      (close_function env ktmpl_env ktmpl_env_mode ktmpl_loc ~kind
         ~mode:alloc_heap ktmpl_body body)
  | Static { tmpl_func; tmpl_env } ->
    let params =
      Misc.Stdlib.Array.of_list_map
        (fun { name; _ } -> Slambdaident.of_ident name)
        tmpl_func.params
    in
    let env = bind_params params in
    let body = eval_lam ctx env tmpl_func.body in
    { slv_comptime = body.slv_comptime;
      slv_runtime =
        close_function env tmpl_env tmpl_func.mode tmpl_func.loc ~kind
          ~mode:tmpl_func.mode tmpl_func body.slv_runtime
    }

(* Check that expansion left no unresolved layouts or templates. *)

exception Found_a_splice

let rec assert_mixed_block_element_contains_no_splices : type a.
    a Lambda.mixed_block_element -> unit = function
  | Splice_variable _ -> raise Found_a_splice
  | Value _ | Float_boxed _ | Float64 | Float32 | Bits8 | Bits16 | Bits32
  | Bits64 | Vec128 | Vec256 | Vec512 | Mask | Word | Untagged_immediate ->
    ()
  | Product elements ->
    Array.iter assert_mixed_block_element_contains_no_splices elements

let assert_mixed_block_shape_contains_no_splices shape =
  Array.iter assert_mixed_block_element_contains_no_splices shape

let rec assert_layout_contains_no_splices : Lambda.layout -> unit = function
  | Psplicevar _ -> raise Found_a_splice
  | Ptop | Pbottom | Punboxed_float _ | Punboxed_or_untagged_integer _
  | Punboxed_vector _ | Punboxed_mask ->
    ()
  | Pvalue value_kind -> assert_value_kind_contains_no_splices value_kind
  | Punboxed_product layouts ->
    List.iter assert_layout_contains_no_splices layouts

and assert_value_kind_contains_no_splices { raw_kind; nullable = _ } =
  assert_raw_value_kind_contains_no_splices raw_kind

and assert_raw_value_kind_contains_no_splices = function
  | Pvariant { consts = _; non_consts } ->
    List.iter
      (fun (_, constructor_shape) ->
        assert_constructor_shape_contains_no_splices constructor_shape)
      non_consts
  | Pgenval | Pintval | Pboxedfloatval _ | Pboxedintval _ | Parrayval _
  | Pboxedvectorval _ | Pboxedmaskval ->
    ()

and assert_constructor_shape_contains_no_splices = function
  | Constructor_uniform value_kinds ->
    List.iter assert_value_kind_contains_no_splices value_kinds
  | Constructor_mixed mixed_block_shape ->
    assert_mixed_block_shape_contains_no_splices mixed_block_shape

let assert_primitive_contains_no_splices (prim : Lambda.primitive) =
  match prim with
  | Popaque layout | Pobj_magic layout ->
    assert_layout_contains_no_splices layout
  | Pget_idx (layout, _)
  | Pset_idx (layout, _)
  | Patomic_load_idx { layout }
  | Patomic_set_idx { layout; _ }
  | Patomic_exchange_idx { layout; _ }
  | Patomic_compare_exchange_idx { layout; _ }
  | Patomic_compare_set_idx { layout; _ }
  | Patomic_load_ptr { layout }
  | Patomic_set_ptr { layout; _ }
  | Patomic_exchange_ptr { layout; _ }
  | Patomic_compare_exchange_ptr { layout; _ }
  | Patomic_compare_set_ptr { layout; _ }
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

let rec assert_constant_contains_no_splices = function
  | Const_block (_, fields) ->
    List.iter assert_constant_contains_no_splices fields
  | Const_mixed_block (_, shape, fields) ->
    assert_mixed_block_shape_contains_no_splices shape;
    List.iter assert_constant_contains_no_splices fields
  | Const_base _ | Const_float_array _ | Const_immstring _ | Const_float_block _
  | Const_null ->
    ()

let rec assert_no_splices (lam : Lambda.lambda) =
  (match lam with
  | Lvar _ | Lmutvar _ -> ()
  | Lconst constant -> assert_constant_contains_no_splices constant
  | Lapply { ap_result_layout; _ } ->
    assert_layout_contains_no_splices ap_result_layout
  | Lfunction func -> assert_function_contains_no_splices func
  | Llet (_, layout, _, _, _, _) -> assert_layout_contains_no_splices layout
  | Lmutlet (layout, _, _, _, _) -> assert_layout_contains_no_splices layout
  | Lletrec (bindings, _) ->
    List.iter
      (fun { def; _ } -> assert_function_contains_no_splices def)
      bindings
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
  | Lsend (_, _, _, _, _, _, _, layout, _) ->
    assert_layout_contains_no_splices layout
  | Levent _ | Lifused _ -> ()
  | Lregion (_, layout) -> assert_layout_contains_no_splices layout
  | Lexclave _ -> ()
  | Lkindtemplate _ | Lkindinstantiate _ | Ltemplate _ | Linstantiate _ ->
    Lambda.fatal_error_invalid_constructor lam);
  Lambda.iter_head_constructor assert_no_splices lam

let eval ~cu_static_data template_lam =
  Profile.record_call "static_eval" (fun () ->
      let ctx = Ctx.create ~cu_static_data in
      let { slv_comptime; slv_runtime } = eval_lam ctx Env.empty template_lam in
      let lambda =
        List.fold_left
          (fun lam (id, def) ->
            Llet (Strict, layout_function, id, debug_uid_none, def, lam))
          slv_runtime (Ctx.instantiations ctx)
      in
      (try assert_no_splices lambda
       with Found_a_splice ->
         Misc.fatal_error "Encountered an unresolved layout after static eval");
      if
        (not Language_extension.(is_at_least Layout_poly Alpha))
        && not (template_lam == lambda)
      then
        Misc.fatal_error
          "Static eval did something non-trivial but layout poly is disabled.";
      { CU_data.templates = Ctx.store ctx; cu = slv_comptime }, lambda)
