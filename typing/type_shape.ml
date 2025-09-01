module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

type base_layout = Jkind_types.Sort.base

type path_lookup = Path.t -> args:Shape.t list -> Shape.t option

module Recursive_binder : sig
  type t

  val mk_recursive_binder : unit -> t

  val use_recursive_binder : t -> Shape.t

  val bind_recursive_binder : ?preserve_uid:bool -> t -> Shape.t -> Shape.t
end = struct
  (* CR sspies: To improve performance, consider replacing this pass with
     a single pass over the resulting definition that simultaneously turns
     all binders into DeBruijn indices. *)
  let rec shape_subst_uid_with_rec_var ~preserve_uid uid rv outer =
    let open Shape in
    match outer.desc with
    | Leaf when Option.equal Uid.equal outer.uid (Some uid) ->
      let uid = if preserve_uid then Some uid else None in
      Shape.rec_var ?uid rv
    | Leaf | Error _ | Rec_var _ | Comp_unit _ | Var _ -> outer (* base cases *)
    | Alias sh ->
      Shape.alias ?uid:outer.uid
        (shape_subst_uid_with_rec_var ~preserve_uid uid rv sh)
    | App (sh, arg) ->
      Shape.app ?uid:outer.uid
        (shape_subst_uid_with_rec_var ~preserve_uid uid rv sh)
        ~arg:(shape_subst_uid_with_rec_var ~preserve_uid uid rv arg)
    | Proj (sh, item) ->
      Shape.proj ?uid:outer.uid
        (shape_subst_uid_with_rec_var ~preserve_uid uid rv sh)
        item
    | Struct map ->
      Shape.str ?uid:outer.uid
        (Item.Map.map (shape_subst_uid_with_rec_var ~preserve_uid uid rv) map)
    | Abs (var, sh) ->
      Shape.abs ?uid:outer.uid var
        (shape_subst_uid_with_rec_var ~preserve_uid uid rv sh)
    | Mu sh ->
      Shape.mu ?uid:outer.uid
        (shape_subst_uid_with_rec_var ~preserve_uid uid (rv + 1) sh)
    | Mutrec map ->
      Shape.mutrec ?uid:outer.uid
        (Ident.Map.map
           (fun sh -> shape_subst_uid_with_rec_var ~preserve_uid uid rv sh)
           map)
    | Proj_decl (sh, id) ->
      Shape.proj_decl ?uid:outer.uid
        (shape_subst_uid_with_rec_var ~preserve_uid uid rv sh)
        id
    | Constr (id, args) ->
      Shape.constr ?uid:outer.uid id
        (List.map (shape_subst_uid_with_rec_var ~preserve_uid uid rv) args)
    | Tuple shapes ->
      Shape.tuple ?uid:outer.uid
        (List.map (shape_subst_uid_with_rec_var ~preserve_uid uid rv) shapes)
    | Unboxed_tuple shapes ->
      Shape.unboxed_tuple ?uid:outer.uid
        (List.map (shape_subst_uid_with_rec_var ~preserve_uid uid rv) shapes)
    | Predef (predef, args) ->
      Shape.predef predef ?uid:outer.uid
        (List.map
           (fun sh -> shape_subst_uid_with_rec_var ~preserve_uid uid rv sh)
           args)
    | Arrow -> Shape.arrow ?uid:outer.uid ()
    | Poly_variant fields ->
      Shape.poly_variant ?uid:outer.uid
        (poly_variant_constructors_map
           (shape_subst_uid_with_rec_var ~preserve_uid uid rv)
           fields)
    | Record { fields; kind } ->
      Shape.record ?uid:outer.uid kind
        (List.map
           (fun (name, uid_opt, sh, layout) ->
             ( name,
               uid_opt,
               shape_subst_uid_with_rec_var ~preserve_uid uid rv sh,
               layout ))
           fields)
    | Variant constructors ->
      Shape.variant ?uid:outer.uid
        (Shape.complex_constructors_map
           (fun (sh, layout) ->
             shape_subst_uid_with_rec_var ~preserve_uid uid rv sh, layout)
           constructors)
    | Variant_unboxed
        { name; variant_uid; arg_name; arg_uid; arg_shape; arg_layout } ->
      Shape.variant_unboxed ?uid:outer.uid ~variant_uid ~arg_uid name arg_name
        (shape_subst_uid_with_rec_var ~preserve_uid uid rv arg_shape)
        arg_layout

  type t =
    { uid : Uid.t;
      mutable used : bool
    }

  let mk_recursive_binder () = { uid = Uid.mk ~current_unit:None; used = false }

  let use_recursive_binder db =
    db.used <- true;
    Shape.leaf db.uid

  let bind_recursive_binder ?(preserve_uid = true) db sh =
    if not db.used
    then sh
    else
      let sh = shape_subst_uid_with_rec_var ~preserve_uid db.uid 0 sh in
      Shape.mu ?uid:(if preserve_uid then Some db.uid else None) sh
end

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
      Numbers.Int.Map.mem (Types.get_id expr) visited
      || depth > !Clflags.gdwarf_config_max_type_to_shape_depth
    in
    if cannot_proceed ()
    then
      match Numbers.Int.Map.find_opt (Types.get_id expr) visited with
      | Some db -> Recursive_binder.use_recursive_binder db
      | None -> unknown_shape
    else
      match List.find_opt (fun (p, _) -> Types.eq_type p expr) subst with
      | Some (_, replace_by) -> replace_by
      | None ->
        let rec_binder = Recursive_binder.mk_recursive_binder () in
        let visited =
          Numbers.Int.Map.add (Types.get_id expr) rec_binder visited
        in
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
            if !Clflags.dwarf_pedantic
            then
              Misc.fatal_error
                "linking and substitution should not reach this stage."
            else unknown_shape
          | Tvariant rd ->
            let row_fields = Types.row_fields rd in
            let row_fields =
              List.concat_map
                (fun (name, desc) ->
                  match Types.row_field_repr desc with
                  | Rpresent (Some ty) ->
                    [ { pv_constr_name = name;
                        pv_constr_args =
                          [ of_type_expr_go ~depth ~visited ty subst
                              shape_for_constr ]
                      } ]
                  | Rpresent None ->
                    [{ pv_constr_name = name; pv_constr_args = [] }]
                  | Rabsent -> [] (* we filter out absent constructors *)
                  | Reither (_, args, _) ->
                    [ { pv_constr_name = name;
                        pv_constr_args = of_expr_list args
                      } ])
                row_fields
            in
            Shape.poly_variant row_fields
          | Tarrow (_, _, _, _) -> Shape.arrow ()
          | Tunivar _ -> unknown_shape
          | Tof_kind _ -> unknown_shape
          | Tpackage _ -> unknown_shape
          (* CR sspies: Support first-class modules. *)
        in
        Recursive_binder.bind_recursive_binder ~preserve_uid:false rec_binder
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
              field_uid = None;
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
              field_uid = Some lbl.ld_uid;
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
              if !Clflags.dwarf_pedantic
              then
                Misc.fatal_errorf
                  "Type_shape: variant constructor with mismatched layout, has \
                   %a but expected %a"
                  Layout.format ly Layout.format ly2
              else ())
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
                if !Clflags.dwarf_pedantic
                then
                  Misc.fatal_errorf
                    "Type_shape: variant constructor with mismatched layout, \
                     has %a but expected value or void."
                    Layout.format ly
                else Layout.Base Value
              else ly)
            args
        in
        Array.of_list lys
    in
    { Shape.name;
      constr_uid = Some cstr_args.cd_uid;
      kind = constructor_repr;
      args
    }

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
             Some lbl.ld_uid,
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
          let constructors =
            List.map
              (fun ((cstr, arg_layouts) : Types.constructor_declaration * _) ->
                let name = Ident.name cstr.cd_id in
                of_complex_constructor type_subst name cstr arg_layouts
                  shape_for_constr)
              cstrs_with_layouts
          in
          Shape.variant constructors
        | Type_variant ([cstr], Variant_unboxed, _unsafe_mode_crossing)
          when not (is_empty_constructor_list cstr) ->
          let name = Ident.name cstr.cd_id in
          let cstr_uid = cstr.cd_uid in
          let field_name, field_uid, type_expr, layout =
            match cstr.cd_args with
            | Cstr_tuple [ca] -> None, None, ca.ca_type, ca.ca_sort
            | Cstr_record [ld] ->
              Some (Ident.name ld.ld_id), Some ld.ld_uid, ld.ld_type, ld.ld_sort
            | Cstr_tuple _ | Cstr_record _ ->
              Misc.fatal_error "Unboxed variant must have exactly one argument."
          in
          Shape.variant_unboxed ~variant_uid:(Some cstr_uid) ~arg_uid:field_uid
            name field_name
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
            if !Clflags.dwarf_pedantic
            then Misc.fatal_error "inlined records not allowed here"
            else
              unknown_shape
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

  (* Heuristic: In (a block of mutually) recursive defintions, it is possibly to
     create recursive cycles that do not have a closed form. For example,

        type 'a foo = A of 'a | B of (int * 'a) foo

     does not have a closed form that we could compute, because in each recursive
     iteration, the type argument grows by one tuple component. Thus, we employ
     the following heuristic:
       1. We support recursive occurrences (including of mutually recursive
          declarations) if they are applied to exactly the same arguments as
          the current declaration. This ensures that when we fully unfold the
          type, including mutual recursion, the only thing that can happen is
          that we encounter a type cycle (which DWARF can handle)---we cannot
          end up in an infinite chain of new, unencountered types.
       2. We support recursive occurrences (including of mutually recursive
          declarations) if all of their arguments are closed. In these cases,
          the expansion can also only lead to cycles, but not to finite chains.
          We approximate closedness with the function [is_closed_shape] below.

      For all other cases, we replace the type arguments with a leaf, which will
      concepturally be handled as [Top], meaning the values of this type could be
      any valid OCaml values (or any valid values of the corresponding layout).
  *)

  let rec is_closed_type_shape shape =
    let open Shape in
    match shape.desc with
    | Leaf -> true
    | Predef (_, args) | Constr (_, args) ->
      List.for_all is_closed_type_shape args
    | Alias sh -> is_closed_type_shape sh
    | Tuple shapes | Unboxed_tuple shapes ->
      List.for_all is_closed_type_shape shapes
    | Arrow -> true
    | Poly_variant constrs ->
      List.for_all
        (fun { pv_constr_name = _; pv_constr_args = shs } ->
          List.for_all is_closed_type_shape shs)
        constrs
    | Variant constructors ->
      List.for_all
        (fun { name = _; constr_uid = _; kind = _; args } ->
          List.for_all
            (fun { field_name = _; field_uid = _; field_value = sh, _ } ->
              is_closed_type_shape sh)
            args)
        constructors
    | Variant_unboxed
        { name = _;
          variant_uid = _;
          arg_name = _;
          arg_uid = _;
          arg_shape = sh;
          arg_layout = _
        } ->
      is_closed_type_shape sh
    | Record { fields; kind = _ } ->
      List.for_all (fun (_, _, sh, _) -> is_closed_type_shape sh) fields
    | _ -> false

  let shape_for_constr_with_declarations
      (decl_lookup_map : Types.type_declaration Ident.Map.t) shape_for_constr
      ~recursive ~id:_ ~decl_args (path : Path.t) ~args:inner_args =
    match shape_for_constr path ~args:inner_args with
    | Some s -> Some s
    | None -> (
      match path with
      | Pident id' -> (
        match Ident.Map.find_opt id' decl_lookup_map with
        | None -> None
        | Some _ when List.equal Shape.equal decl_args inner_args ->
          recursive := true;
          Some (Shape.constr id' inner_args)
        | Some _ when List.for_all is_closed_type_shape inner_args ->
          recursive := true;
          Some (Shape.constr id' inner_args)
        | Some _ ->
          recursive := true;
          (* We are applying the declaration to different arguments
             that are not closed. In this case, we create a version of the type
             that can have any OCaml values for its arguments. *)
          Some
            (Shape.constr id' (List.map (fun _ -> Shape.leaf' None) inner_args))
        )
      | Pdot _ | Papply _ | Pextra_ty _ -> None)

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
    let shape_for_constr' (path : Path.t) ~args =
      match path with
      | Pident id when Ident.Map.mem id decl_lookup_map -> None
      | Pident _ | Pdot _ | Papply _ | Pextra_ty _ ->
        shape_for_constr path ~args
    in
    let shape_for_constr' =
      Type_shape.Predef.shape_for_constr_with_predefs shape_for_constr'
    in
    let recursive = ref false in
    (* We add a small optimization: For the block of declarations, we track via
       this reference whether there are any recursive occurrenes. If not, we do
       not have to add a mutually recursive binder for the declarations. *)
    let shape_for_constr' =
      shape_for_constr_with_declarations ~recursive decl_lookup_map
        shape_for_constr'
    in
    let individual_declarations =
      Ident.Map.mapi
        (fun id decl ->
          of_type_declaration_with_variables id decl shape_for_constr')
        decl_lookup_map
    in
    if !recursive
    then
      let mutrec = Shape.mutrec individual_declarations in
      List.map (fun (id, _) -> Shape.proj_decl mutrec id) type_declarations
    else
      List.map
        (fun (id, _) -> Ident.Map.find id individual_declarations)
        type_declarations

  let of_type_declaration id decl shape_for_constr =
    let decls = of_type_declarations [id, decl] shape_for_constr in
    match decls with [decl] -> decl | _ -> assert false

  let of_extension_constructor_merlin_only (ext : Types.extension_constructor) =
    match ext.ext_args with
    | Types.Cstr_record lbls ->
      let record =
        record_of_labels
          ~shape_for_constr:(fun _ ~args:_ -> None)
          ~type_subst:[] Record_boxed lbls
        (* CR sspies: Instead of [Record_boxed], it would be nicer to mark
           these as virtual, because they only exist for Merlin. This saves
           us from trouble when shapes that are intended for Merlin end up
           in the DWARF emission, because they will at least be labeled. *)
      in
      Shape.set_uid_if_none record ext.ext_uid
    | _ -> Shape.leaf ext.ext_uid
end

let rec decompose_application (t : Shape.t) =
  match t.Shape.desc with
  | Shape.App (f, arg) ->
    let head, tail = decompose_application f in
    head, tail @ [arg]
  | _ -> t, []

let find_constr_id_with_args (subst_constr, _) id args =
  match Ident.Map.find_opt id subst_constr with
  | Some t ->
    List.find_opt (fun (args', _) -> List.equal Shape.equal args args') t
    |> Option.map snd
  | None -> None

let find_mut_rec_shape (_, subst_constr_mut) id =
  Ident.Map.find_opt id subst_constr_mut

let update_subst_with_id_arg_binder (subst_constr, subst_constr_mut) id args
    rec_binder =
  let new_list =
    match Ident.Map.find_opt id subst_constr with
    | Some t -> (args, rec_binder) :: t
    | None -> [args, rec_binder]
  in
  Ident.Map.add id new_list subst_constr, subst_constr_mut

let update_subst_with_mutrec_decl (subst_constr, subst_constr_mut) t map =
  ( subst_constr,
    Ident.Map.fold
      (fun id _ map -> Ident.Map.add id (Shape.proj_decl t id) map)
      map subst_constr_mut )

module Evaluation_diagnostics = struct
  type evaluate_diagnostics = { mutable reduction_steps : int }

  type t = evaluate_diagnostics option

  let no_diagnostics = None

  let create_diagnostics () = Some { reduction_steps = 0 }

  let count_evaluation_step diagnostics =
    match diagnostics with
    | Some d -> d.reduction_steps <- d.reduction_steps + 1
    | None -> ()

  let get_reduction_steps diagnostics =
    match diagnostics with None -> 0 | Some d -> d.reduction_steps
end

module D = Evaluation_diagnostics

(* To unroll the mutually recursive declarations, we perform a simple call by
   value evaluation and catch cycles for ident binders. *)
let rec unfold_and_evaluate ~diagnostics ~depth ~steps_remaining subst_type
    subst_constr (t : Shape.t) =
  D.count_evaluation_step diagnostics;
  if Misc.Maybe_bounded.is_depleted steps_remaining
  then Shape.leaf' None
  else if depth >= !Clflags.gdwarf_config_shape_eval_depth
          (* CR sspies: This depth limit can currently produce very large shapes, and
             some additional caching would be appropriate. *)
  then Shape.leaf' None
  else (
    Misc.Maybe_bounded.decr steps_remaining;
    (* we special case the case where the head is a projection, because of
       recursive unfolding *)
    let head, args = decompose_application t in
    let maybe_evaluated_shape =
      match head.Shape.desc with
      | Proj_decl (str, i) -> (
        let args =
          List.map
            (unfold_and_evaluate ~diagnostics ~depth ~steps_remaining subst_type
               subst_constr)
            args
        in
        let str =
          unfold_and_evaluate ~diagnostics ~depth ~steps_remaining subst_type
            subst_constr str
        in
        match str.Shape.desc with
        | Mutrec ts ->
          let depth = depth + 1 in
          let rec_binder = Recursive_binder.mk_recursive_binder () in
          let subst_constr =
            update_subst_with_mutrec_decl subst_constr str ts
          in
          let subst_constr =
            update_subst_with_id_arg_binder subst_constr i args rec_binder
          in
          let ts = Ident.Map.find i ts in
          unfold_and_evaluate ~diagnostics ~depth ~steps_remaining subst_type
            subst_constr (Shape.app_list ts args)
          |> Recursive_binder.bind_recursive_binder ~preserve_uid:false
               rec_binder
          |> Option.some
        | Leaf -> None
        | _ -> assert false
        (* projections are always directly applied to the mutrec *))
      | _ -> None
    in
    match maybe_evaluated_shape with
    | Some t -> t
    | None -> (
      match t.desc with
      | Var id -> (
        match Ident.Map.find_opt id subst_type with
        | Some t -> t
        | None -> t (* we encountered a free variable *))
      | Constr (id, constr_args) -> (
        let constr_args =
          List.map
            (unfold_and_evaluate ~diagnostics ~depth ~steps_remaining subst_type
               subst_constr)
            constr_args
        in
        match find_constr_id_with_args subst_constr id constr_args with
        | Some t -> Recursive_binder.use_recursive_binder t
        | None -> (
          match find_mut_rec_shape subst_constr id with
          | Some t ->
            unfold_and_evaluate ~diagnostics ~depth ~steps_remaining subst_type
              subst_constr
              (Shape.app_list t constr_args)
          | None -> Shape.leaf' None))
      | App (f, arg) -> (
        let f =
          unfold_and_evaluate ~diagnostics ~depth ~steps_remaining subst_type
            subst_constr f
        in
        let arg =
          unfold_and_evaluate ~diagnostics ~depth ~steps_remaining subst_type
            subst_constr arg
        in
        match f.Shape.desc with
        | Abs (x, s') ->
          unfold_and_evaluate ~diagnostics ~depth ~steps_remaining
            (Ident.Map.add x arg subst_type)
            subst_constr s'
        | _ -> Shape.app f ~arg)
      | Proj_decl _ ->
        Shape.leaf' None
        (* only possible for the [Leaf] case, see [maybe_evaluated_shape] above *)
      | Variant constructors ->
        let constructors =
          Shape.complex_constructors_map
            (fun ((sh, ly) : Shape.t * _) ->
              ( unfold_and_evaluate ~diagnostics ~depth ~steps_remaining
                  subst_type subst_constr sh,
                ly ))
            constructors
        in
        Shape.variant constructors
      | Record { fields; kind } ->
        Shape.record kind
          (List.map
             (fun ((name, uid, sh, ly) : _ * _ * Shape.t * _) ->
               ( name,
                 uid,
                 unfold_and_evaluate ~diagnostics ~depth ~steps_remaining
                   subst_type subst_constr sh,
                 ly ))
             fields)
      | Poly_variant constrs ->
        Shape.poly_variant
          (Shape.poly_variant_constructors_map
             (fun (sh : Shape.t) ->
               unfold_and_evaluate ~diagnostics ~depth ~steps_remaining
                 subst_type subst_constr sh)
             constrs)
      | Arrow -> Shape.arrow ()
      | Variant_unboxed
          { name; variant_uid; arg_name; arg_uid; arg_shape; arg_layout } ->
        Shape.variant_unboxed ~variant_uid ~arg_uid name arg_name
          (unfold_and_evaluate ~diagnostics ~depth ~steps_remaining subst_type
             subst_constr arg_shape)
          arg_layout
      | Proj (t, i) ->
        Shape.proj
          (unfold_and_evaluate ~diagnostics ~depth ~steps_remaining subst_type
             subst_constr t)
          i
      | Tuple args ->
        Shape.tuple
          (List.map
             (fun (sh : Shape.t) ->
               unfold_and_evaluate ~diagnostics ~depth ~steps_remaining
                 subst_type subst_constr sh)
             args)
      | Unboxed_tuple args ->
        Shape.unboxed_tuple
          (List.map
             (fun (sh : Shape.t) ->
               unfold_and_evaluate ~diagnostics ~depth ~steps_remaining
                 subst_type subst_constr sh)
             args)
      | Predef (p, args) ->
        Shape.predef p
          (List.map
             (fun (sh : Shape.t) ->
               unfold_and_evaluate ~diagnostics ~depth ~steps_remaining
                 subst_type subst_constr sh)
             args)
      | Mu body ->
        Shape.mu
          (unfold_and_evaluate ~diagnostics ~depth ~steps_remaining subst_type
             subst_constr body)
      | Alias t ->
        Shape.alias
          (unfold_and_evaluate ~diagnostics ~depth ~steps_remaining subst_type
             subst_constr t)
      | Struct items ->
        Shape.str
          (Shape.Item.Map.map
             (fun (sh : Shape.t) ->
               unfold_and_evaluate ~diagnostics ~depth ~steps_remaining
                 subst_type subst_constr sh)
             items)
      (* normal forms for CBV evaluation *)
      | Mutrec _ | Abs _ | Error _ | Comp_unit _ | Rec_var _ | Leaf ->
        t (* normal form in this CBV evaluation *)))

(* CR sspies: The performance of this evaluation is quite poor, requiring us to
   limit the depth to about 5 at the moment. Improve it using caching to make it
   possible to have deeper shapes. *)
let unfold_and_evaluate ?(diagnostics = Evaluation_diagnostics.no_diagnostics) t
    =
  let steps_remaining =
    Misc.Maybe_bounded.of_option
      !Clflags.gdwarf_config_max_evaluation_steps_per_variable
  in
  unfold_and_evaluate ~diagnostics ~depth:0 ~steps_remaining Ident.Map.empty
    (Ident.Map.empty, Ident.Map.empty)
    t

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
  | Tuple _ | Arrow | Variant _ | Poly_variant _ | Record _ ->
    Some (Layout.Base Value)
  | Alias t -> estimate_layout_from_type_shape t
  | Mu t ->
    estimate_layout_from_type_shape t
    (* Simple treatment of recursion, we simply look inside. *)
  | Leaf | Abs _ | Mutrec _ | Error _ | Comp_unit _ | Rec_var _ | App _ | Proj _
  | Struct _ | Proj_decl _ ->
    None

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
