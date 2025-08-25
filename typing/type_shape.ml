module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

type base_layout = Jkind_types.Sort.base

type path_lookup = Path.t -> args:Shape.t list -> Shape.t option

module Type_shape = struct
  module Predef = struct
    open Shape.Predef

    let simd_base_type_of_path = function
      | p when Path.same p Predef.path_int8x16 -> Some Int8x16
      | p when Path.same p Predef.path_int16x8 -> Some Int16x8
      | p when Path.same p Predef.path_int32x4 -> Some Int32x4
      | p when Path.same p Predef.path_int64x2 -> Some Int64x2
      | p when Path.same p Predef.path_float32x4 -> Some Float32x4
      | p when Path.same p Predef.path_float64x2 -> Some Float64x2
      | p when Path.same p Predef.path_int8x32 -> Some Int8x32
      | p when Path.same p Predef.path_int16x16 -> Some Int16x16
      | p when Path.same p Predef.path_int32x8 -> Some Int32x8
      | p when Path.same p Predef.path_int64x4 -> Some Int64x4
      | p when Path.same p Predef.path_float32x8 -> Some Float32x8
      | p when Path.same p Predef.path_float64x4 -> Some Float64x4
      | p when Path.same p Predef.path_int8x64 -> Some Int8x64
      | p when Path.same p Predef.path_int16x32 -> Some Int16x32
      | p when Path.same p Predef.path_int32x16 -> Some Int32x16
      | p when Path.same p Predef.path_int64x8 -> Some Int64x8
      | p when Path.same p Predef.path_float32x16 -> Some Float32x16
      | p when Path.same p Predef.path_float64x8 -> Some Float64x8
      | _ -> None

    let simd_vec_split_of_path = function
      | p when Path.same p Predef.path_unboxed_int8x16 -> Some Int8x16
      | p when Path.same p Predef.path_unboxed_int16x8 -> Some Int16x8
      | p when Path.same p Predef.path_unboxed_int32x4 -> Some Int32x4
      | p when Path.same p Predef.path_unboxed_int64x2 -> Some Int64x2
      | p when Path.same p Predef.path_unboxed_float32x4 -> Some Float32x4
      | p when Path.same p Predef.path_unboxed_float64x2 -> Some Float64x2
      | p when Path.same p Predef.path_unboxed_int8x32 -> Some Int8x32
      | p when Path.same p Predef.path_unboxed_int16x16 -> Some Int16x16
      | p when Path.same p Predef.path_unboxed_int32x8 -> Some Int32x8
      | p when Path.same p Predef.path_unboxed_int64x4 -> Some Int64x4
      | p when Path.same p Predef.path_unboxed_float32x8 -> Some Float32x8
      | p when Path.same p Predef.path_unboxed_float64x4 -> Some Float64x4
      | p when Path.same p Predef.path_unboxed_int8x64 -> Some Int8x64
      | p when Path.same p Predef.path_unboxed_int16x32 -> Some Int16x32
      | p when Path.same p Predef.path_unboxed_int32x16 -> Some Int32x16
      | p when Path.same p Predef.path_unboxed_int64x8 -> Some Int64x8
      | p when Path.same p Predef.path_unboxed_float32x16 -> Some Float32x16
      | p when Path.same p Predef.path_unboxed_float64x8 -> Some Float64x8
      | _ -> None

    let unboxed_of_path = function
      | p when Path.same p Predef.path_unboxed_float -> Some Unboxed_float
      | p when Path.same p Predef.path_unboxed_float32 -> Some Unboxed_float32
      | p when Path.same p Predef.path_unboxed_nativeint ->
        Some Unboxed_nativeint
      | p when Path.same p Predef.path_unboxed_int64 -> Some Unboxed_int64
      | p when Path.same p Predef.path_unboxed_int32 -> Some Unboxed_int32
      | p -> Option.map (fun s -> Unboxed_simd s) (simd_vec_split_of_path p)

    let of_path : Path.t -> t option = function
      | p when Path.same p Predef.path_array -> Some Array
      | p when Path.same p Predef.path_bytes -> Some Bytes
      | p when Path.same p Predef.path_char -> Some Char
      | p when Path.same p Predef.path_extension_constructor ->
        Some Extension_constructor
      | p when Path.same p Predef.path_float -> Some Float
      | p when Path.same p Predef.path_float32 -> Some Float32
      | p when Path.same p Predef.path_floatarray -> Some Floatarray
      | p when Path.same p Predef.path_int -> Some Int
      | p when Path.same p Predef.path_int32 -> Some Int32
      | p when Path.same p Predef.path_int64 -> Some Int64
      | p when Path.same p Predef.path_lazy_t -> Some Lazy_t
      | p when Path.same p Predef.path_nativeint -> Some Nativeint
      | p when Path.same p Predef.path_string -> Some String
      | p when Path.same p Predef.path_exn -> Some Exception
      | p -> (
        match simd_base_type_of_path p with
        | Some b -> Some (Simd b)
        | None -> (
          match unboxed_of_path p with
          | Some u -> Some (Unboxed u)
          | None -> None))

    let shape_for_constr_with_predefs f path ~args =
      match of_path path with
      | Some predef -> Some (Shape.predef predef args)
      | None -> f path ~args
  end

  (* Similarly to [value_kind], we track a set of visited types to avoid cycles
     in the lookup and we, additionally, carry a maximal depth for the recursion.
     We allow a deeper bound than [value_kind]. *)
  (* CR sspies: Consider additionally adding a max size for the set of visited
     types.  Also consider reverting to the original value kind depth limit
     (although 2 seems low). *)
  let rec of_type_expr_go ~visited ~depth (expr : Types.type_expr)
      (subst : (Types.type_expr * Shape.t) list) shape_for_constr : Shape.t =
    let open Shape in
    let unknown_shape = Shape.leaf' None in
    (* Leaves indicate we do not know. *)
    let[@inline] cannot_proceed () =
      Numbers.Int.Map.mem (Types.get_id expr) visited || depth > 10
      (* CR sspies: Make the depth a command line flag. *)
    in
    if cannot_proceed ()
    then
      match Numbers.Int.Map.find_opt (Types.get_id expr) visited with
      | Some () ->
        unknown_shape (* CR sspies: We can use this for recursive cycles. *)
      | None -> unknown_shape
    else
      match List.find_opt (fun (p, _) -> Types.eq_type p expr) subst with
      | Some (_, s) -> s
      | None ->
        let visited = Numbers.Int.Map.add (Types.get_id expr) () visited in
        let depth = depth + 1 in
        let desc = Types.get_desc expr in
        let of_expr_list (exprs : Types.type_expr list) =
          List.map
            (fun expr ->
              of_type_expr_go ~depth ~visited expr subst shape_for_constr)
            exprs
        in
        let type_shape =
          match desc with
          | Tconstr (path, constrs, _) ->
            let args = of_expr_list constrs in
            let shape = shape_for_constr path ~args in
            Option.value shape ~default:unknown_shape
          | Ttuple exprs -> Shape.tuple (of_expr_list (List.map snd exprs))
          | Tvar { name = Some x; _ } -> Shape.var' None (Ident.create_local x)
          | Tvar { name = None; _ } -> unknown_shape
          (* CR sspies: This is not a great way of handling type variables. This
             case should only be triggered for free variables. We should compute
             a layout from the jkind and produce a shape with this layout.
             Revisit this when revisiting the layout generation. *)
          | Tpoly (type_expr, _type_vars) ->
            (* CR sspies: At the moment, we simply ignore the polymorphic
               variables.
               This code used to only work for [_type_vars = []]. Consider
               alternatively introducing abstractions here? *)
            of_type_expr_go ~depth ~visited type_expr subst shape_for_constr
          | Tunboxed_tuple exprs ->
            Shape.unboxed_tuple (of_expr_list (List.map snd exprs))
          | Tobject _ | Tnil | Tfield _ ->
            unknown_shape
            (* Objects are currently not supported in the debugger. *)
          | Tlink _ | Tsubst _ ->
            Misc.fatal_error
              "linking and substitution should not reach this stage."
          | Tvariant rd ->
            let row_fields = Types.row_fields rd in
            let row_fields =
              List.concat_map
                (fun (name, desc) ->
                  match Types.row_field_repr desc with
                  | Types.Rpresent (Some ty) ->
                    [ { pv_constr_name = name;
                        pv_constr_args =
                          [ of_type_expr_go ~depth ~visited ty subst
                              shape_for_constr ]
                      } ]
                  | Types.Rpresent None ->
                    [{ pv_constr_name = name; pv_constr_args = [] }]
                  | Types.Rabsent -> [] (* we filter out absent constructors *)
                  | Types.Reither (_, args, _) ->
                    [ { pv_constr_name = name;
                        pv_constr_args = of_expr_list args
                      } ])
                row_fields
            in
            Shape.poly_variant row_fields
          | Tarrow (_, arg, ret, _) ->
            Shape.arrow
              (of_type_expr_go ~depth ~visited arg subst shape_for_constr)
              (of_type_expr_go ~depth ~visited ret subst shape_for_constr)
          | Tunivar _ -> unknown_shape
          | Tof_kind _ -> unknown_shape
          | Tpackage _ -> unknown_shape
          (* CR sspies: Support first-class modules. *)
        in
        (* CR sspies: For recursive types, we can stick on a recursive binder
           here. *)
        type_shape

  let of_type_expr (expr : Types.type_expr) shape_for_constr =
    of_type_expr_go ~visited:Numbers.Int.Map.empty ~depth:0 expr []
      (Predef.shape_for_constr_with_predefs shape_for_constr)

  let of_type_expr_with_type_subst (expr : Types.type_expr) shape_for_constr
      subst =
    of_type_expr_go ~visited:Numbers.Int.Map.empty ~depth:0 expr subst
      (Predef.shape_for_constr_with_predefs shape_for_constr)
end

module Type_decl_shape = struct
  let rec mixed_block_shape_to_layout = function
    | Types.Value -> Layout.Base Value
    | Types.Float_boxed ->
      Layout.Base Float64
      (* [Float_boxed] records are unboxed in the variant at runtime,
         contrary to the name.*)
    | Types.Float64 -> Layout.Base Float64
    | Types.Float32 -> Layout.Base Float32
    | Types.Bits8 -> Layout.Base Bits8
    | Types.Bits16 -> Layout.Base Bits16
    | Types.Bits32 -> Layout.Base Bits32
    | Types.Untagged_immediate -> Layout.Base Untagged_immediate
    | Types.Bits64 -> Layout.Base Bits64
    | Types.Vec128 -> Layout.Base Vec128
    | Types.Vec256 -> Layout.Base Vec256
    | Types.Vec512 -> Layout.Base Vec512
    | Types.Word -> Layout.Base Word
    | Types.Void -> Layout.Base Void
    | Types.Product args ->
      Layout.Product
        (Array.to_list (Array.map mixed_block_shape_to_layout args))

  let of_complex_constructor type_subst name
      (cstr_args : Types.constructor_declaration)
      ((constructor_repr, _) : Types.constructor_representation * _)
      shape_for_constr =
    let args =
      match cstr_args.cd_args with
      | Cstr_tuple list ->
        List.map
          (fun ({ ca_type = type_expr; ca_sort = type_layout; _ } :
                 Types.constructor_argument) ->
            { Shape.field_name = None;
              field_value =
                ( Type_shape.of_type_expr_with_type_subst type_expr
                    shape_for_constr type_subst,
                  type_layout )
            })
          list
      | Cstr_record list ->
        List.map
          (fun (lbl : Types.label_declaration) ->
            { Shape.field_name = Some (Ident.name lbl.ld_id);
              field_value =
                ( Type_shape.of_type_expr_with_type_subst lbl.ld_type
                    shape_for_constr type_subst,
                  lbl.ld_sort )
            })
          list
    in
    let constructor_repr =
      match constructor_repr with
      | Constructor_mixed shapes ->
        List.iter2
          (fun mix_shape { Shape.field_name = _; field_value = _, ly } ->
            let ly2 = mixed_block_shape_to_layout mix_shape in
            if not (Layout.equal ly ly2)
            then
              Misc.fatal_errorf
                "Type_shape: variant constructor with mismatched layout, has \
                 %a but expected %a"
                Layout.format ly Layout.format ly2)
          (Array.to_list shapes) args;
        Array.map mixed_block_shape_to_layout shapes
      | Constructor_uniform_value ->
        let lys =
          List.map
            (fun { Shape.field_name = _; field_value = _, ly } ->
              if not
                   (Layout.equal ly (Layout.Base Value)
                   || Layout.equal ly (Layout.Base Void))
              then
                Misc.fatal_errorf
                  "Type_shape: variant constructor with mismatched layout, has \
                   %a but expected value or void."
                  Layout.format ly
              else ly)
            args
        in
        Array.of_list lys
    in
    { Shape.name; kind = constructor_repr; args }

  let is_empty_constructor_list (cstr_args : Types.constructor_declaration) =
    match cstr_args.cd_args with
    | Cstr_tuple [] -> true
    | Cstr_tuple (_ :: _)
    | Cstr_record _
    (* Records are not allowed to have an empty list of fields.*) ->
      false

  let record_of_labels ~shape_for_constr ~type_subst kind labels =
    Shape.record kind
      (List.map
         (fun (lbl : Types.label_declaration) ->
           ( Ident.name lbl.ld_id,
             Type_shape.of_type_expr_with_type_subst lbl.ld_type
               shape_for_constr type_subst,
             lbl.ld_sort ))
         labels)

  let type_var_count = ref 0

  let of_type_declaration_go (type_declaration : Types.type_declaration)
      type_param_shapes shape_for_constr =
    let module Types_predef = Predef in
    let open Shape in
    let unknown_shape = Shape.leaf' None in
    let type_params = type_declaration.type_params in
    let type_subst = List.combine type_params type_param_shapes in
    (* Duplicates are fine, the constraint system makes sure they are
       instantiated with the same type expression. *)
    let definition =
      match type_declaration.type_manifest with
      | Some type_expr ->
        Type_shape.of_type_expr_with_type_subst type_expr shape_for_constr
          type_subst
      | None -> (
        match type_declaration.type_kind with
        | Type_variant (cstr_list, Variant_boxed layouts, _unsafe_mode_crossing)
          ->
          let cstrs_with_layouts =
            List.combine cstr_list (Array.to_list layouts)
          in
          let simple_constructors, complex_constructors =
            List.partition_map
              (fun ((cstr, arg_layouts) : Types.constructor_declaration * _) ->
                let name = Ident.name cstr.cd_id in
                match is_empty_constructor_list cstr with
                | true -> Left name
                | false ->
                  Right
                    (of_complex_constructor type_subst name cstr arg_layouts
                       shape_for_constr))
              cstrs_with_layouts
          in
          Shape.variant simple_constructors complex_constructors
        | Type_variant ([cstr], Variant_unboxed, _unsafe_mode_crossing)
          when not (is_empty_constructor_list cstr) ->
          let name = Ident.name cstr.cd_id in
          let field_name, type_expr, layout =
            match cstr.cd_args with
            | Cstr_tuple [ca] -> None, ca.ca_type, ca.ca_sort
            | Cstr_record [ld] ->
              Some (Ident.name ld.ld_id), ld.ld_type, ld.ld_sort
            | Cstr_tuple _ | Cstr_record _ ->
              Misc.fatal_error "Unboxed variant must have exactly one argument."
          in
          Shape.variant_unboxed name field_name
            (Type_shape.of_type_expr_with_type_subst type_expr shape_for_constr
               type_subst)
            layout
        | Type_variant ([_], Variant_unboxed, _unsafe_mode_crossing) ->
          Misc.fatal_error "Unboxed variant must have constructor arguments."
        | Type_variant (([] | _ :: _ :: _), Variant_unboxed, _) ->
          Misc.fatal_error "Unboxed variant must have exactly one constructor."
        | Type_variant
            (_, (Variant_extensible | Variant_with_null), _unsafe_mode_crossing)
          ->
          unknown_shape (* CR sspies: These variants are not yet supported. *)
        | Type_record (lbl_list, record_repr, _unsafe_mode_crossing) -> (
          match record_repr with
          | Record_boxed _ ->
            record_of_labels ~shape_for_constr ~type_subst Record_boxed lbl_list
          | Record_mixed fields ->
            record_of_labels ~shape_for_constr ~type_subst
              (Record_mixed (Array.map mixed_block_shape_to_layout fields))
              lbl_list
          | Record_unboxed ->
            record_of_labels ~shape_for_constr ~type_subst Record_unboxed
              lbl_list
          | Record_float | Record_ufloat ->
            let lbl_list =
              List.map
                (fun (lbl : Types.label_declaration) ->
                  { lbl with
                    ld_sort = Base Float64;
                    ld_type = Types_predef.type_unboxed_float
                  })
                  (* CR sspies: We are changing the type and the layout here.
                     Consider adding a name for the types of the fields instead
                     of replacing it with [float#]. *)
                lbl_list
            in
            record_of_labels ~shape_for_constr ~type_subst Record_floats
              lbl_list
          | Record_inlined _ ->
            Misc.fatal_error "inlined records not allowed here"
            (* Inline records of this form should not occur as part of type
               declarations.  They do not exist for top-level declarations,
               but they do exist temporarily such as inside of a match (e.g.,
               [t] is an inline record in [match e with Foo t -> ...]). *))
        | Type_abstract _ -> unknown_shape
        | Type_open -> unknown_shape
        | Type_record_unboxed_product (lbl_list, _, _) ->
          record_of_labels ~shape_for_constr ~type_subst Record_unboxed_product
            lbl_list)
    in
    definition

  let shape_for_constr_with_declarations
      (decl_lookup_map : Types.type_declaration Ident.Map.t) shape_for_constr
      ~id:_ ~decl_args:_ path ~args:inner_args =
    match shape_for_constr path ~args:inner_args with
    | Some s -> Some s
    | None -> (
      match path with
      | Path.Pident id' -> (
        match Ident.Map.find_opt id' decl_lookup_map with
        | None -> None
        | Some _ ->
          Some (Shape.constr id' inner_args)
          (* CR sspies: We can use this in a future to deal with recursive
             declarations.  For now, we simply leave the identifier there,
             which will be emitted as an unknown value. *))
      | _ -> None)

  let of_type_declaration_with_variables (id : Ident.t)
      (type_declaration : Types.type_declaration) shape_for_constr =
    let type_param_idents =
      List.map
        (fun _ ->
          let name = Format.asprintf "a/%d" !type_var_count in
          type_var_count := !type_var_count + 1;
          Ident.create_local name)
        type_declaration.type_params
    in
    let type_param_shapes =
      List.map (fun id -> Shape.var' None id) type_param_idents
    in
    let shape_for_constr = shape_for_constr ~id ~decl_args:type_param_shapes in
    let definition =
      of_type_declaration_go type_declaration type_param_shapes shape_for_constr
    in
    let decl_shape = Shape.abs_list definition type_param_idents in
    Shape.set_uid_if_none decl_shape type_declaration.type_uid

  let of_type_declarations
      (type_declarations : (Ident.t * Types.type_declaration) list)
      shape_for_constr =
    let decl_lookup_map = Ident.Map.of_list type_declarations in
    (* We unbind all declarations, to avoid accidental recursive cycles. *)
    let shape_for_constr' path ~args =
      match path with
      | Path.Pident id when Ident.Map.mem id decl_lookup_map -> None
      | _ -> shape_for_constr path ~args
    in
    let shape_for_constr' =
      Type_shape.Predef.shape_for_constr_with_predefs shape_for_constr'
    in
    let shape_for_constr' =
      shape_for_constr_with_declarations decl_lookup_map shape_for_constr'
    in
    let individual_declarations =
      Ident.Map.mapi
        (fun id decl ->
          of_type_declaration_with_variables id decl shape_for_constr')
        decl_lookup_map
    in
    List.map
      (fun (id, _) -> Ident.Map.find id individual_declarations)
      type_declarations

  let of_type_declaration id decl shape_for_constr =
    let decls = of_type_declarations [id, decl] shape_for_constr in
    match decls with [decl] -> decl | _ -> assert false
end

type shape_with_layout =
  { type_shape : Shape.t;
    type_layout : Layout.t;
    type_name : string
  }

let (all_type_decls : Shape.t Uid.Tbl.t) = Uid.Tbl.create 16

let (all_type_shapes : shape_with_layout Uid.Tbl.t) = Uid.Tbl.create 16

let add_to_type_decls (decls : (Ident.t * Types.type_declaration) list)
    shape_for_constr =
  let type_decl_shapes =
    Type_decl_shape.of_type_declarations decls shape_for_constr
  in
  List.iter
    (fun ((_, decl), sh) -> Uid.Tbl.add all_type_decls decl.Types.type_uid sh)
    (List.combine decls type_decl_shapes)

let add_to_type_shapes var_uid type_expr type_layout ~name:type_name uid_of_path
    =
  let type_shape = Type_shape.of_type_expr type_expr uid_of_path in
  Uid.Tbl.add all_type_shapes var_uid { type_shape; type_name; type_layout }

let rec estimate_layout_from_type_shape (t : Shape.t) : Layout.t option =
  match t.desc with
  | Predef (t, _) -> Some (Shape.Predef.to_layout t)
  | Constr (_, _) ->
    None (* recursive occurrence, conservatively not handled for now *)
  | Unboxed_tuple fields ->
    let field_layouts = List.map estimate_layout_from_type_shape fields in
    if List.for_all Option.is_some field_layouts
    then Some (Layout.Product (List.map Option.get field_layouts))
    else None
  | Var _ -> None (* CR sspies: Find out what happens to type variables. *)
  | Variant_unboxed { arg_layout; _ } ->
    Some arg_layout
    (* CR sspies: [arg_layout] could become unreliable in the future. Consider
       recursively descending in that case. *)
  | Tuple _ | Arrow _ | Variant _ | Poly_variant _ | Record _ ->
    Some (Layout.Base Value)
  | Alias t ->
    estimate_layout_from_type_shape t
    (* Simple treatment of recursion, we simply look inside. *)
  | Leaf | Abs _ | Error _ | Comp_unit _ | App _ | Proj _ | Struct _ -> None

let print_table_all_type_decls ppf =
  let entries = Uid.Tbl.to_list all_type_decls in
  let entries = List.sort (fun (a, _) (b, _) -> Uid.compare a b) entries in
  let entries =
    List.map
      (fun (k, v) ->
        Format.asprintf "%a" Uid.print k, Format.asprintf "%a" Shape.print v)
      entries
  in
  let uids, decls = List.split entries in
  Misc.pp_table ppf ["UID", uids; "Type Declaration", decls]

let print_table_all_type_shapes ppf =
  let entries = Uid.Tbl.to_list all_type_shapes in
  let entries = List.sort (fun (a, _) (b, _) -> Uid.compare a b) entries in
  let entries =
    List.map
      (fun (k, { type_shape; type_name; type_layout }) ->
        ( Format.asprintf "%a" Uid.print k,
          ( type_name,
            ( Format.asprintf "%a" Shape.print type_shape,
              Format.asprintf "%a" Layout.format type_layout ) ) ))
      entries
  in
  let uids, rest = List.split entries in
  let names, rest = List.split rest in
  let types, sorts = List.split rest in
  Misc.pp_table ppf ["UID", uids; "Type", names; "Shape", types; "Sort", sorts]

(* Print debug uid tables when the command line flag [-ddebug-uids] is set. *)
let print_debug_uid_tables ppf =
  Format.fprintf ppf "\n";
  print_table_all_type_decls ppf;
  Format.fprintf ppf "\n";
  print_table_all_type_shapes ppf
