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
      clo_runtime_env : (Ident.t * layout) list;
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
      clo_runtime_env : (Ident.t * layout) list;
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

let rec eval_structured_const env old_const =
  match old_const with
  | Const_mixed_block (n, old_shape, old_consts) ->
    let new_shape = eval_mixed_block_shape env old_shape in
    let new_consts =
      Misc.Stdlib.List.map_sharing (eval_structured_const env) old_consts
    in
    if new_shape == old_shape && new_consts == old_consts
    then old_const
    else Const_mixed_block (n, new_shape, new_consts)
  | Const_block (n, old_consts) ->
    let new_consts =
      Misc.Stdlib.List.map_sharing (eval_structured_const env) old_consts
    in
    if new_consts == old_consts then old_const else Const_block (n, new_consts)
  | Const_base _ | Const_float_array _ | Const_immstring _ | Const_float_block _
  | Const_null ->
    old_const

and eval_block_shape env old_block_shape =
  match old_block_shape with
  | All_value -> old_block_shape
  | Shape old_shape ->
    let new_shape = eval_mixed_block_shape env old_shape in
    if new_shape == old_shape then old_block_shape else Shape new_shape

and eval_mixed_block_shape :
    'a. Env.t -> 'a mixed_block_element array -> 'a mixed_block_element array =
 fun env old_shape ->
  Misc.Stdlib.Array.map_sharing (eval_mixed_block_element env) old_shape

and eval_mixed_block_element :
    'a. Env.t -> 'a mixed_block_element -> 'a mixed_block_element =
 fun env old_element ->
  match old_element with
  | Splice_variable id ->
    Env.find env id |> expect_not_missing |> expect Tlayout
    |> mixed_block_element_of_layout
  | Product old_elements ->
    let new_elements =
      Misc.Stdlib.Array.map_sharing (eval_mixed_block_element env) old_elements
    in
    if new_elements == old_elements then old_element else Product new_elements
  | Value _ | Float_boxed _ | Float64 | Float32 | Bits8 | Bits16 | Bits32
  | Bits64 | Vec128 | Vec256 | Vec512 | Mask | Word | Untagged_immediate ->
    old_element

and eval_layout env old_layout =
  match old_layout with
  | Psplicevar id -> Env.find env id |> expect_not_missing |> expect Tlayout
  | Punboxed_product old_layouts ->
    let new_layouts =
      Misc.Stdlib.List.map_sharing (eval_layout env) old_layouts
    in
    if new_layouts == old_layouts
    then old_layout
    else Punboxed_product new_layouts
  | Pvalue old_value_kind ->
    let new_value_kind = eval_value_kind env old_value_kind in
    if new_value_kind == old_value_kind
    then old_layout
    else Pvalue new_value_kind
  | Ptop | Punboxed_float _ | Punboxed_or_untagged_integer _ | Punboxed_vector _
  | Punboxed_mask | Pbottom ->
    old_layout

and eval_value_kind env
    ({ raw_kind = old_raw_kind; nullable } as old_value_kind) =
  let new_raw_kind = eval_raw_value_kind env old_raw_kind in
  if new_raw_kind == old_raw_kind
  then old_value_kind
  else { raw_kind = new_raw_kind; nullable }

and eval_raw_value_kind env old_value_kind =
  match old_value_kind with
  | Pvariant { consts; non_consts = old_non_consts } ->
    let new_non_consts =
      Misc.Stdlib.List.map_sharing
        (fun ((i, old_constructor_shape) as old_non_const) ->
          let new_constructor_shape =
            eval_constructor_shape env old_constructor_shape
          in
          if new_constructor_shape == old_constructor_shape
          then old_non_const
          else i, new_constructor_shape)
        old_non_consts
    in
    if new_non_consts == old_non_consts
    then old_value_kind
    else Pvariant { consts; non_consts = new_non_consts }
  | Pgenval | Pintval | Pboxedfloatval _ | Pboxedintval _ | Parrayval _
  | Pboxedvectorval _ | Pboxedmaskval ->
    old_value_kind

and eval_constructor_shape env old_constructor_shape =
  match old_constructor_shape with
  | Constructor_uniform old_value_kinds ->
    let new_value_kinds =
      Misc.Stdlib.List.map_sharing (eval_value_kind env) old_value_kinds
    in
    if new_value_kinds == old_value_kinds
    then old_constructor_shape
    else Constructor_uniform new_value_kinds
  | Constructor_mixed old_mixed_block_shape ->
    let new_mixed_block_shape =
      eval_mixed_block_shape env old_mixed_block_shape
    in
    if new_mixed_block_shape == old_mixed_block_shape
    then old_constructor_shape
    else Constructor_mixed new_mixed_block_shape

let dynamic slv_runtime = { slv_comptime = Missing; slv_runtime }

(* An all-Missing record carries no static information. Use Missing so its fresh
   identity doesn't cause needless template specializations. *)
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

let eval_lparam env
    ({ name; debug_uid; layout = old_layout; attributes; mode } as old_param) =
  let new_layout = eval_layout env old_layout in
  if new_layout == old_layout
  then old_param
  else { name; debug_uid; layout = new_layout; attributes; mode }

(* Evaluate static half and expand tlambda into lambda. Preserve physical
   equality of unchanged lambda: [eval] relies on this to check that evaluation
   is trivial when layout polymorphism is disabled. *)
let rec eval_lam ?name ctx env old_lambda : halves =
  match old_lambda with
  | Lvar id ->
    { slv_comptime = Env.find env (Slambdaident.of_ident id);
      slv_runtime = old_lambda
    }
  | Lmutvar _ ->
    dynamic old_lambda
  | Lconst old_const ->
    let new_const = eval_structured_const env old_const in
    dynamic (if new_const == old_const then old_lambda else Lconst new_const)
  | Lapply
      ({ ap_func = old_func;
         ap_args = old_args;
         ap_result_layout = old_layout;
         _
       } as old_apply) ->
    let new_func = eval_dynamic ctx env old_func in
    let new_args = eval_dynamic_list ctx env old_args in
    let new_layout = eval_layout env old_layout in
    dynamic
      (if
         new_func == old_func && new_args == old_args
         && new_layout == old_layout
       then old_lambda
       else
         Lapply
           { old_apply with
             ap_func = new_func;
             ap_args = new_args;
             ap_result_layout = new_layout
           })
  | Lfunction old_func ->
    let new_func = eval_lfunction ctx env old_func in
    dynamic (if new_func == old_func then old_lambda else Lfunction new_func)
  | Llet (kind, old_layout, id, uid, old_def, old_body) ->
    (* let id = def in body ~>
         let { c = def_c; r = def_r } = eval_lam env def in
         let { c = body_c; r = body_r } = eval_lam {env with id=def_c} body in
         { c = body_c; r = << let id = def_r in body_r >> } *)
    let new_def = eval_lam ~name:id ctx env old_def in
    let body_env =
      Env.add env (Slambdaident.of_ident id) new_def.slv_comptime
    in
    let new_body = eval_lam ?name ctx body_env old_body in
    let new_layout = eval_layout env old_layout in
    { slv_comptime = new_body.slv_comptime;
      slv_runtime =
        (if
           new_def.slv_runtime == old_def
           && new_body.slv_runtime == old_body
           && new_layout == old_layout
         then old_lambda
         else
           Llet
             ( kind,
               new_layout,
               id,
               uid,
               new_def.slv_runtime,
               new_body.slv_runtime ))
    }
  | Lmutlet (old_layout, id, uid, old_def, old_body) ->
    (* Mutable variables have no static part. *)
    let new_def = eval_dynamic ctx env old_def in
    let new_body = eval_lam ?name ctx env old_body in
    let new_layout = eval_layout env old_layout in
    { slv_comptime = new_body.slv_comptime;
      slv_runtime =
        (if
           new_def == old_def
           && new_body.slv_runtime == old_body
           && new_layout == old_layout
         then old_lambda
         else Lmutlet (new_layout, id, uid, new_def, new_body.slv_runtime))
    }
  | Lletrec (old_bindings, old_body) ->
    (* Functions have no static part, so their identifiers need no binding in
       the static environment. *)
    let new_bindings =
      Misc.Stdlib.List.map_sharing
        (fun ({ def = old_def; _ } as old_binding) ->
          let new_def = eval_lfunction ctx env old_def in
          if old_def == new_def
          then old_binding
          else { old_binding with def = new_def })
        old_bindings
    in
    let new_body = eval_lam ?name ctx env old_body in
    { slv_comptime = new_body.slv_comptime;
      slv_runtime =
        (if new_bindings == old_bindings && new_body.slv_runtime == old_body
         then old_lambda
         else Lletrec (new_bindings, new_body.slv_runtime))
    }
  | Lprim (old_prim, old_args, loc) ->
    eval_prim ?name ctx env old_lambda old_prim old_args loc
  | Lswitch (old_arg, old_switch, loc, old_layout) ->
    (* switch is a runtime operation. *)
    let new_arg = eval_dynamic ctx env old_arg in
    let new_consts = eval_alist ctx env old_switch.sw_consts in
    let new_blocks = eval_alist ctx env old_switch.sw_blocks in
    let new_failaction =
      Misc.Stdlib.Option.map_sharing (eval_dynamic ctx env)
        old_switch.sw_failaction
    in
    let new_switch =
      if
        new_consts == old_switch.sw_consts
        && new_blocks == old_switch.sw_blocks
        && new_failaction == old_switch.sw_failaction
      then old_switch
      else
        { old_switch with
          sw_consts = new_consts;
          sw_blocks = new_blocks;
          sw_failaction = new_failaction
        }
    in
    let new_layout = eval_layout env old_layout in
    dynamic
      (if
         new_arg == old_arg && new_switch == old_switch
         && new_layout == old_layout
       then old_lambda
       else Lswitch (new_arg, new_switch, loc, new_layout))
  | Lstringswitch (old_arg, old_cases, old_default, loc, old_layout) ->
    let new_arg = eval_dynamic ctx env old_arg in
    let new_cases = eval_alist ctx env old_cases in
    let new_default =
      Misc.Stdlib.Option.map_sharing (eval_dynamic ctx env) old_default
    in
    let new_layout = eval_layout env old_layout in
    dynamic
      (if
         new_arg == old_arg && new_cases == old_cases
         && new_default == old_default && new_layout == old_layout
       then old_lambda
       else Lstringswitch (new_arg, new_cases, new_default, loc, new_layout))
  | Lstaticraise (label, old_args) ->
    let new_args = eval_dynamic_list ctx env old_args in
    dynamic
      (if new_args == old_args
       then old_lambda
       else Lstaticraise (label, new_args))
  | Lstaticcatch
      (old_body, (label, old_params), old_handler, pop_region, old_layout) ->
    let new_body = eval_dynamic ctx env old_body in
    let new_handler = eval_dynamic ctx env old_handler in
    let new_params =
      Misc.Stdlib.List.map_sharing
        (fun ((id, uid, old_layout) as old_param) ->
          let new_layout = eval_layout env old_layout in
          if new_layout == old_layout then old_param else id, uid, new_layout)
        old_params
    in
    let new_layout = eval_layout env old_layout in
    dynamic
      (if
         new_body == old_body && new_handler == old_handler
         && new_params == old_params && new_layout == old_layout
       then old_lambda
       else
         Lstaticcatch
           (new_body, (label, new_params), new_handler, pop_region, new_layout))
  | Ltrywith (old_body, id, uid, old_handler, old_layout) ->
    (* Exceptions are runtime-only. *)
    let new_body = eval_dynamic ctx env old_body in
    let new_handler = eval_dynamic ctx env old_handler in
    let new_layout = eval_layout env old_layout in
    dynamic
      (if
         new_body == old_body && new_handler == old_handler
         && new_layout == old_layout
       then old_lambda
       else Ltrywith (new_body, id, uid, new_handler, new_layout))
  | Lifthenelse (old_cond, old_ifso, old_ifnot, old_layout) ->
    let new_cond = eval_dynamic ctx env old_cond in
    let new_ifso = eval_dynamic ctx env old_ifso in
    let new_ifnot = eval_dynamic ctx env old_ifnot in
    let new_layout = eval_layout env old_layout in
    dynamic
      (if
         new_cond == old_cond && new_ifso == old_ifso && new_ifnot == old_ifnot
         && new_layout == old_layout
       then old_lambda
       else Lifthenelse (new_cond, new_ifso, new_ifnot, new_layout))
  | Lsequence (old_left, old_right) ->
    (* left; right ~>
         let { c = _; r = left_r } = eval_lam env left in
         let { c = right_c; r = right_r } = eval_lam env right in
         { c = right_c; r = << left_r; right_r >> } *)
    let new_left = eval_dynamic ctx env old_left in
    let new_right = eval_lam ?name ctx env old_right in
    { slv_comptime = new_right.slv_comptime;
      slv_runtime =
        (if new_left == old_left && new_right.slv_runtime == old_right
         then old_lambda
         else Lsequence (new_left, new_right.slv_runtime))
    }
  | Lwhile { wh_cond = old_cond; wh_body = old_body } ->
    (* Expansion visits the condition and body once, not once per iteration. *)
    let new_cond = eval_dynamic ctx env old_cond in
    let new_body = eval_dynamic ctx env old_body in
    dynamic
      (if new_cond == old_cond && new_body == old_body
       then old_lambda
       else Lwhile { wh_cond = new_cond; wh_body = new_body })
  | Lfor
      ({ for_from = old_from; for_to = old_to; for_body = old_body; _ } as
       old_loop) ->
    (* Expand the body once; the loop variable is runtime-only. *)
    let new_from = eval_dynamic ctx env old_from in
    let new_to = eval_dynamic ctx env old_to in
    let new_body = eval_dynamic ctx env old_body in
    dynamic
      (if new_from == old_from && new_to == old_to && new_body == old_body
       then old_lambda
       else
         Lfor
           { old_loop with
             for_from = new_from;
             for_to = new_to;
             for_body = new_body
           })
  | Lassign (id, old_value) ->
    let new_value = eval_dynamic ctx env old_value in
    dynamic
      (if new_value == old_value then old_lambda else Lassign (id, new_value))
  | Lsend
      ( kind,
        old_met,
        old_obj,
        old_args,
        region_close,
        mode,
        loc,
        old_layout,
        yielding ) ->
    let new_met = eval_dynamic ctx env old_met in
    let new_obj = eval_dynamic ctx env old_obj in
    let new_args = eval_dynamic_list ctx env old_args in
    let new_layout = eval_layout env old_layout in
    dynamic
      (if
         new_met == old_met && new_obj == old_obj && new_args == old_args
         && new_layout == old_layout
       then old_lambda
       else
         Lsend
           ( kind,
             new_met,
             new_obj,
             new_args,
             region_close,
             mode,
             loc,
             new_layout,
             yielding ))
  | Levent (old_body, event) ->
    let new_body = eval_lam ?name ctx env old_body in
    { new_body with
      slv_runtime =
        (if new_body.slv_runtime == old_body
         then old_lambda
         else Levent (new_body.slv_runtime, event))
    }
  | Lifused (id, old_body) ->
    let new_body = eval_lam ?name ctx env old_body in
    { new_body with
      slv_runtime =
        (if new_body.slv_runtime == old_body
         then old_lambda
         else Lifused (id, new_body.slv_runtime))
    }
  | Lregion (old_body, old_layout) ->
    let new_body = eval_lam ?name ctx env old_body in
    let new_layout = eval_layout env old_layout in
    { new_body with
      slv_runtime =
        (if new_body.slv_runtime == old_body && new_layout == old_layout
         then old_lambda
         else Lregion (new_body.slv_runtime, new_layout))
    }
  | Lexclave old_body ->
    let new_body = eval_lam ?name ctx env old_body in
    { new_body with
      slv_runtime =
        (if new_body.slv_runtime == old_body
         then old_lambda
         else Lexclave new_body.slv_runtime)
    }
  | Lkindtemplate template ->
    (* kindtemplate k1 ... kn -> func, capturing id1=e1, ..., idm=em ~>
         let { c = c1; r = r1 } = eval_lam env e1 in
         ...
         let { c = cm; r = rm } = eval_lam env em in
         { c = closure (Kind template, {env with id1=c1; ...; idm=cm});
           r = << makeblock r1 ... rm >> } *)
    eval_template ?name ctx env (Kind template) template.ktmpl_env
      template.ktmpl_env_mode template.ktmpl_loc
  | Ltemplate template ->
    (* template arg1 ... argn -> body, capturing id1=e1, ..., idm=em ~>
         let { c = c1; r = r1 } = eval_lam env e1 in
         ...
         let { c = cm; r = rm } = eval_lam env em in
         { c = closure (Static template, {env with id1=c1; ...; idm=cm});
           r = << makeblock r1 ... rm >> } *)
    eval_template ?name ctx env (Static template) template.tmpl_env
      template.tmpl_func.mode template.tmpl_func.loc
  | Lkindinstantiate
      { kinst_func = old_func;
        kinst_args = old_args;
        kinst_result_layout = old_layout;
        kinst_mode;
        kinst_loc
      } ->
    (* kindinstantiate func [k1; ...; kn] ~>
         let { c = func_c; r = func_r } = eval_lam env func in
         let { c = inst_c; r = inst_r } = func_c [k1; ...; kn] in
         { c = inst_c; r = << inst_r func_r >> } *)
    let new_func = eval_lam ctx env old_func in
    let new_args =
      Misc.Stdlib.Array.of_list_map
        (fun old_layout_arg ->
          Or_missing.Present (Vlayout (eval_layout env old_layout_arg)))
        old_args
    in
    let instantiated = instantiate ctx new_func.slv_comptime new_args in
    { slv_comptime = instantiated.slv_comptime;
      slv_runtime =
        Lapply
          { ap_func = instantiated.slv_runtime;
            ap_args = [new_func.slv_runtime];
            ap_result_layout = eval_layout env old_layout;
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
  | Linstantiate
      ({ ap_func = old_func;
         ap_args = old_args;
         ap_result_layout = old_layout;
         _
       } as old_apply) ->
    (* instantiate func arg1 ... argn ~>
         let { c = func_c; r = func_r } = eval_lam env func in
         let { c = argn_c; r = argn_r } = eval_lam env argn in
         ...
         let { c = arg1_c; r = arg1_r } = eval_lam env arg1 in
         let { c = inst_c; r = inst_r } = func_c [arg1_c; ...; argn_c] in
         { c = inst_c; r = << inst_r func_r arg1_r ... argn_r >> } *)
    let new_func = eval_lam ctx env old_func in
    let new_args_c, new_args_r = eval_args_reverse ctx env old_args in
    let instantiated =
      instantiate ctx new_func.slv_comptime (Array.of_list new_args_c)
    in
    { slv_comptime = instantiated.slv_comptime;
      slv_runtime =
        Lapply
          { old_apply with
            ap_func = instantiated.slv_runtime;
            ap_args = new_func.slv_runtime :: new_args_r;
            ap_result_layout = eval_layout env old_layout
          }
    }

and eval_lfunction ctx env
    ({ kind;
       params = old_params;
       return = old_return;
       body = old_body;
       attr;
       loc;
       mode;
       ret_mode;
       yielding
     } as old_func) =
  let new_body = eval_dynamic ctx env old_body in
  let new_params = Misc.Stdlib.List.map_sharing (eval_lparam env) old_params in
  let new_return = eval_layout env old_return in
  if
    new_body == old_body && new_params == old_params && new_return == old_return
  then old_func
  else
    lfunction' ~kind ~params:new_params ~return:new_return ~body:new_body ~attr
      ~loc ~mode ~ret_mode
    |> lfunction_with_yielding yielding

(* Still evaluates static content and records template instantiations; only the
   returned static value is discarded. *)
and eval_dynamic ctx env old_lambda = (eval_lam ctx env old_lambda).slv_runtime

and eval_dynamic_list ctx env old_args =
  Misc.Stdlib.List.map_sharing (eval_dynamic ctx env) old_args

and eval_alist : 'a. Ctx.t -> Env.t -> ('a * lambda) list -> ('a * lambda) list
    =
 fun ctx env old_entries ->
  Misc.Stdlib.List.map_sharing
    (fun ((tag, old_body) as old_entry) ->
      let new_body = eval_dynamic ctx env old_body in
      if new_body == old_body then old_entry else tag, new_body)
    old_entries

(* Primitive fields and static arguments are expanded right-to-left, while
   preserving their original order in the residual code. *)
and eval_args_reverse ctx env old_args =
  let rec loop unchanged new_args_c new_args_r = function
    | [] -> new_args_c, if unchanged then old_args else new_args_r
    | old_arg :: rest ->
      let new_arg = eval_lam ctx env old_arg in
      loop
        (unchanged && new_arg.slv_runtime == old_arg)
        (new_arg.slv_comptime :: new_args_c)
        (new_arg.slv_runtime :: new_args_r)
        rest
  in
  loop true [] [] (List.rev old_args)

and eval_prim ?name ctx env old_lambda old_prim old_args loc =
  let wrong_arity expected =
    Misc.fatal_errorf "Slambda: %a takes exactly %d arguments, got %d"
      Printlambda.primitive old_prim expected (List.length old_args)
  in
  let new_args_c, new_args_r = eval_args_reverse ctx env old_args in
  let one_arg () =
    match new_args_c with [arg_c] -> arg_c | _ -> wrong_arity 1
  in
  let result new_prim slv_comptime =
    { slv_comptime;
      slv_runtime =
        (if new_prim == old_prim && new_args_r == old_args
         then old_lambda
         else Lprim (new_prim, new_args_r, loc))
    }
  in
  let dynamic_prim new_prim = result new_prim Missing in
  match old_prim with
  | Pgetglobal (cu, Static) ->
    (* (Pgetglobal c) [] ~>
       { c = cu_static_data ctx cu; r = << (Pgetglobal c) [] >>} *)
    (match old_args with [] -> () | _ -> wrong_arity 0);
    result old_prim (Ctx.cu_static_data ctx cu)
  | Pmakeblock (n, mut, old_shape, mode) ->
    (* (Pmakeblock) [arg1, .., argn] ~>
       let { c = argn_c; r = argn_r } = eval_lam env argn in
       ...
       let { c = arg1_c; r = arg1_r } = eval_lam env arg1 in
       { c = [arg1_c; ...; argn_c]; r = [(Pmakeblock) [arg1_r, .., argn_r]] }
       Mutable blocks get a missing compile-time part so we don't read the wrong
       value out after they've been mutated. *)
    let new_shape = eval_block_shape env old_shape in
    let new_prim =
      if new_shape == old_shape
      then old_prim
      else Pmakeblock (n, mut, new_shape, mode)
    in
    begin match mut with
    | Immutable | Immutable_unique ->
      result new_prim (make_record ?name ctx new_args_c)
    | Mutable -> dynamic_prim new_prim
    end
  | Pfield (pos, _, Reads_agree) ->
    (* e.(pos) ~>
       let { c = e_c; r = e_r } = eval_lam env e in
       { c = e_c.(pos); r = << e_r.(pos) >> } *)
    let arg_c = one_arg () in
    result old_prim (project_field arg_c pos)
  | Pmixedfield (path, old_shape, sem) ->
    (* e.(pos1).(pos2) ~>
       let { c = e_c; r = e_r } = eval_lam env e in
       { c = e_c.(pos1).(pos2); r = << e_r.(pos1).(pos2) >> }
       If it's a read of a mutable field the compile-time part gets set to
       missing so we don't accidentally read the wrong value. *)
    let new_shape = eval_mixed_block_shape env old_shape in
    let new_prim =
      if new_shape == old_shape
      then old_prim
      else Pmixedfield (path, new_shape, sem)
    in
    begin match sem with
    | Reads_agree ->
      let arg_c = one_arg () in
      result new_prim (List.fold_left project_field arg_c path)
    | Reads_vary -> dynamic_prim new_prim
    end
  | Psetmixedfield (is, old_shape, init_or_assign) ->
    let new_shape = eval_mixed_block_shape env old_shape in
    dynamic_prim
      (if new_shape == old_shape
       then old_prim
       else Psetmixedfield (is, new_shape, init_or_assign))
  | Pmake_unboxed_product old_layouts ->
    let new_layouts =
      Misc.Stdlib.List.map_sharing (eval_layout env) old_layouts
    in
    dynamic_prim
      (if new_layouts == old_layouts
       then old_prim
       else Pmake_unboxed_product new_layouts)
  | Punboxed_product_field (i, old_layouts) ->
    let new_layouts =
      Misc.Stdlib.List.map_sharing (eval_layout env) old_layouts
    in
    dynamic_prim
      (if new_layouts == old_layouts
       then old_prim
       else Punboxed_product_field (i, new_layouts))
  | Pmake_idx_mixed_field (old_shape, i, path) ->
    let new_shape = eval_mixed_block_shape env old_shape in
    dynamic_prim
      (if new_shape == old_shape
       then old_prim
       else Pmake_idx_mixed_field (new_shape, i, path))
  | Pmake_idx_array (kind, index_kind, old_element, path) ->
    let new_element = eval_mixed_block_element env old_element in
    dynamic_prim
      (if new_element == old_element
       then old_prim
       else Pmake_idx_array (kind, index_kind, new_element, path))
  | Pidx_deepen (old_element, path) ->
    let new_element = eval_mixed_block_element env old_element in
    dynamic_prim
      (if new_element == old_element
       then old_prim
       else Pidx_deepen (new_element, path))
  | Popaque old_layout ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then old_prim else Popaque new_layout)
  | Pobj_magic old_layout ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then old_prim else Pobj_magic new_layout)
  | Pget_idx (old_layout, mut) ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then old_prim else Pget_idx (new_layout, mut))
  | Pset_idx (old_layout, mode) ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then old_prim else Pset_idx (new_layout, mode))
  | Pget_ptr (old_layout, mut) ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then old_prim else Pget_ptr (new_layout, mut))
  | Pset_ptr (old_layout, mode) ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout then old_prim else Pset_ptr (new_layout, mode))
  | Pget_ext_ptr (old_layout, mut) ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then old_prim
       else Pget_ext_ptr (new_layout, mut))
  | Pset_ext_ptr (old_layout, mode) ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then old_prim
       else Pset_ext_ptr (new_layout, mode))
  | Patomic_load_idx { layout = old_layout } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then old_prim
       else Patomic_load_idx { layout = new_layout })
  | Patomic_set_idx { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then old_prim
       else Patomic_set_idx { layout = new_layout; mode })
  | Patomic_exchange_idx { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then old_prim
       else Patomic_exchange_idx { layout = new_layout; mode })
  | Patomic_compare_exchange_idx { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then old_prim
       else Patomic_compare_exchange_idx { layout = new_layout; mode })
  | Patomic_compare_set_idx { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then old_prim
       else Patomic_compare_set_idx { layout = new_layout; mode })
  | Patomic_load_ptr { layout = old_layout } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then old_prim
       else Patomic_load_ptr { layout = new_layout })
  | Patomic_set_ptr { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then old_prim
       else Patomic_set_ptr { layout = new_layout; mode })
  | Patomic_exchange_ptr { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then old_prim
       else Patomic_exchange_ptr { layout = new_layout; mode })
  | Patomic_compare_exchange_ptr { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then old_prim
       else Patomic_compare_exchange_ptr { layout = new_layout; mode })
  | Patomic_compare_set_ptr { layout = old_layout; mode } ->
    let new_layout = eval_layout env old_layout in
    dynamic_prim
      (if new_layout == old_layout
       then old_prim
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
    dynamic_prim old_prim

(** template p1 ... pn -> body, capturing id1=e1, ..., idm=em ~>
    let { c = c1; r = r1 } = eval_lam env e1 in
    ...
    let { c = cm; r = rm } = eval_lam env em in
    { c = closure (template, {env with id1=c1; ...; idm=cm});
      r = << makeblock r1 ... rm >> } *)
and eval_template ?name ctx env template old_captures mode loc =
  let new_captures =
    old_captures
    |> Ident.Map.mapi (fun id (old_def, old_layout) ->
        let new_def = eval_lam ~name:id ctx env old_def in
        let new_layout = eval_layout env old_layout in
        new_def, new_layout)
    |> Ident.Map.bindings
  in
  let clo_env, args =
    List.fold_right
      (fun (id, (new_def, _)) (env, args) ->
        ( Env.add env (Slambdaident.of_ident id) new_def.slv_comptime,
          new_def.slv_runtime :: args ))
      new_captures (env, [])
  in
  let shape =
    Misc.Stdlib.Array.of_list_map
      (fun (_, (_, new_layout)) -> mixed_block_element_of_layout new_layout)
      new_captures
  in
  let clo_runtime_env =
    List.map (fun (id, (_, new_layout)) -> id, new_layout) new_captures
  in
  let closure = { clo_template = template; clo_runtime_env; clo_env } in
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

(** Evaluate the compile-time application of a closure to its arguments.

  Note that the arguments must have already been evaluated.
  Thsi function currently does not support partial- or over-application.

  (kindtemplate {fv0, ..., fvn} k1 ... kn -> func p1 ... pm -> body)
      arg1 ... argn
  ~>
    let { c = _; r = body_r} =
      eval_lam { env with k1=arg1; ...; kn=argn } body
    in
    { c = Missing
      r = << fun env p1 ... pm ->
               let fv0 = env.(0) in
               ...
               let fvk = env.(k) in
               body_r >> }

  (template {fv0, ..., fvn} p1 ... pn -> body) arg1 ... argn) ~>
    let { c = body_c; r = body_r } =
      eval_lam { env with p1=arg1; ..; pn=argn } body
    in
    { c = body_c;
      r = << fun env p1 ... pn ->
               let fv0 = env.(0) in
               ...
               let fvk = env.(k) in
               body_r >>} *)
and eval_apply ctx { clo_template; clo_runtime_env; clo_env } args =
  let bind_params params =
    try Misc.Stdlib.Array.fold_left2 Env.add clo_env params args
    with Invalid_argument _ ->
      Misc.fatal_error
        "Slambda eval doesn't support partial or over application of functors."
  in
  let shape =
    Misc.Stdlib.Array.of_list_map
      (fun (_, layout) -> mixed_block_element_of_layout layout)
      clo_runtime_env
  in
  let close_body ~loc ~env_mode old_body =
    let closure_id = Ident.create_local "closure" in
    let closure_param =
      { name = closure_id;
        debug_uid = debug_uid_none;
        layout = layout_template_env;
        attributes = default_param_attribute;
        mode = env_mode
      }
    in
    let _, new_body =
      List.fold_left
        (fun (i, body) (id, layout) ->
          ( i + 1,
            Llet
              ( Alias,
                layout,
                id,
                debug_uid_none,
                Lprim
                  (Pmixedfield ([i], shape, Reads_agree), [Lvar closure_id], loc),
                body ) ))
        (0, old_body) clo_runtime_env
    in
    closure_param, new_body
  in
  let close_function env (closure_param : lparam)
      { kind = old_kind;
        params = old_params;
        return = old_return;
        body = _;
        attr;
        loc;
        mode = _;
        ret_mode;
        yielding
      } new_body =
    let new_kind =
      match old_kind, closure_param.mode with
      | Curried { nlocal }, Alloc_local -> Curried { nlocal = nlocal + 1 }
      | Curried _, Alloc_heap -> old_kind
      | Tupled, _ ->
        Misc.fatal_error "Tupled static functions are not currently supported"
    in
    let new_params =
      Misc.Stdlib.List.map_sharing (eval_lparam env) old_params
    in
    if List.length new_params > Lambda.max_arity () - 1
    then
      Misc.fatal_errorf
        "Slambda does not currently support functions with over %i arguments"
        (Lambda.max_arity () - 1);
    let new_return = eval_layout env old_return in
    lfunction' ~kind:new_kind
      ~params:(closure_param :: new_params)
      ~return:new_return ~body:new_body ~attr ~loc ~mode:alloc_heap ~ret_mode
    |> lfunction_with_yielding yielding
    |> fun new_func -> Lfunction new_func
  in
  match clo_template with
  | Kind
      { ktmpl_params;
        ktmpl_body = old_func;
        ktmpl_env = _;
        ktmpl_env_mode;
        ktmpl_loc
      } ->
    let env = bind_params (Array.of_list ktmpl_params) in
    let new_body = eval_dynamic ctx env old_func.body in
    let closure_param, closed_body =
      close_body ~loc:ktmpl_loc ~env_mode:ktmpl_env_mode new_body
    in
    dynamic (close_function env closure_param old_func closed_body)
  | Static { tmpl_func = old_func; tmpl_env = _ } ->
    let static_params =
      Misc.Stdlib.Array.of_list_map
        (fun { name; _ } -> Slambdaident.of_ident name)
        old_func.params
    in
    let env = bind_params static_params in
    let new_body = eval_lam ctx env old_func.body in
    let closure_param, closed_body =
      close_body ~loc:old_func.loc ~env_mode:old_func.mode new_body.slv_runtime
    in
    { slv_comptime = new_body.slv_comptime;
      slv_runtime = close_function env closure_param old_func closed_body
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
