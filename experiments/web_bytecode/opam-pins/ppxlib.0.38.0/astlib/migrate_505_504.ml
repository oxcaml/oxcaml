open Stdlib0
module From = Ast_505
module To = Ast_504

module External_type = struct
  type 'a may_contain_external_type = {
    contains_external_type : bool;
    value : 'a;
  }

  let wrap ?(contains_external_type = false) value =
    { contains_external_type; value }

  let contains_external_type f = f.contains_external_type
  let value f = f.value

  let map ~f { contains_external_type; value } =
    { contains_external_type; value = f value }

  let list_map ~f l =
    let acc = wrap ~contains_external_type:false [] in
    let v =
      List.fold_left
        (fun acc x ->
          let x' = f x in
          let vs = x'.value :: acc.value in
          wrap
            ~contains_external_type:
              (acc.contains_external_type || x'.contains_external_type)
            vs)
        acc l
    in
    { v with value = List.rev v.value }

  let err ~loc =
    Location.raise_errorf ~loc
      "Ppxlib migration error: external type cannot be migrated from OCaml 5.5 \
       to 5.4"
end

let copy_location x = x

let rec copy_longident : Ast_505.Longident.t -> Ast_504.Longident.t = function
  | Ast_505.Longident.Lident x0 -> Ast_504.Longident.Lident x0
  | Ast_505.Longident.Ldot (x0, x1) ->
      Ast_504.Longident.Ldot
        (copy_loc copy_longident x0, copy_loc (fun x -> x) x1)
  | Ast_505.Longident.Lapply (x0, x1) ->
      Ast_504.Longident.Lapply
        (copy_loc copy_longident x0, copy_loc copy_longident x1)

and copy_asttypes_constant :
    Ast_505.Asttypes.constant -> Ast_504.Asttypes.constant = function
  | Ast_505.Asttypes.Const_int x0 -> Ast_504.Asttypes.Const_int x0
  | Ast_505.Asttypes.Const_char x0 -> Ast_504.Asttypes.Const_char x0
  | Ast_505.Asttypes.Const_string (x0, x1, x2) ->
      Ast_504.Asttypes.Const_string
        (x0, copy_location x1, Option.map (fun x -> x) x2)
  | Ast_505.Asttypes.Const_float x0 -> Ast_504.Asttypes.Const_float x0
  | Ast_505.Asttypes.Const_int32 x0 -> Ast_504.Asttypes.Const_int32 x0
  | Ast_505.Asttypes.Const_int64 x0 -> Ast_504.Asttypes.Const_int64 x0
  | Ast_505.Asttypes.Const_nativeint x0 -> Ast_504.Asttypes.Const_nativeint x0

and copy_rec_flag : Ast_505.Asttypes.rec_flag -> Ast_504.Asttypes.rec_flag =
  function
  | Ast_505.Asttypes.Nonrecursive -> Ast_504.Asttypes.Nonrecursive
  | Ast_505.Asttypes.Recursive -> Ast_504.Asttypes.Recursive

and copy_direction_flag :
    Ast_505.Asttypes.direction_flag -> Ast_504.Asttypes.direction_flag =
  function
  | Ast_505.Asttypes.Upto -> Ast_504.Asttypes.Upto
  | Ast_505.Asttypes.Downto -> Ast_504.Asttypes.Downto

and copy_private_flag :
    Ast_505.Asttypes.private_flag -> Ast_504.Asttypes.private_flag = function
  | Ast_505.Asttypes.Private -> Ast_504.Asttypes.Private
  | Ast_505.Asttypes.Public -> Ast_504.Asttypes.Public

and copy_mutable_flag :
    Ast_505.Asttypes.mutable_flag -> Ast_504.Asttypes.mutable_flag = function
  | Ast_505.Asttypes.Immutable -> Ast_504.Asttypes.Immutable
  | Ast_505.Asttypes.Mutable -> Ast_504.Asttypes.Mutable

and copy_virtual_flag :
    Ast_505.Asttypes.virtual_flag -> Ast_504.Asttypes.virtual_flag = function
  | Ast_505.Asttypes.Virtual -> Ast_504.Asttypes.Virtual
  | Ast_505.Asttypes.Concrete -> Ast_504.Asttypes.Concrete

and copy_override_flag :
    Ast_505.Asttypes.override_flag -> Ast_504.Asttypes.override_flag = function
  | Ast_505.Asttypes.Override -> Ast_504.Asttypes.Override
  | Ast_505.Asttypes.Fresh -> Ast_504.Asttypes.Fresh

and copy_closed_flag :
    Ast_505.Asttypes.closed_flag -> Ast_504.Asttypes.closed_flag = function
  | Ast_505.Asttypes.Closed -> Ast_504.Asttypes.Closed
  | Ast_505.Asttypes.Open -> Ast_504.Asttypes.Open

and copy_label : Ast_505.Asttypes.label -> Ast_504.Asttypes.label = fun x -> x

and copy_arg_label : Ast_505.Asttypes.arg_label -> Ast_504.Asttypes.arg_label =
  function
  | Ast_505.Asttypes.Nolabel -> Ast_504.Asttypes.Nolabel
  | Ast_505.Asttypes.Labelled x0 -> Ast_504.Asttypes.Labelled x0
  | Ast_505.Asttypes.Optional x0 -> Ast_504.Asttypes.Optional x0

and copy_loc :
    'f0 'g0.
    ('f0 -> 'g0) -> 'f0 Ast_505.Asttypes.loc -> 'g0 Ast_504.Asttypes.loc =
 fun f0 { Ast_505.Asttypes.txt; Ast_505.Asttypes.loc } ->
  { Ast_504.Asttypes.txt = f0 txt; Ast_504.Asttypes.loc = copy_location loc }

and copy_variance : Ast_505.Asttypes.variance -> Ast_504.Asttypes.variance =
  function
  | Ast_505.Asttypes.Covariant -> Ast_504.Asttypes.Covariant
  | Ast_505.Asttypes.Contravariant -> Ast_504.Asttypes.Contravariant
  | Ast_505.Asttypes.NoVariance -> Ast_504.Asttypes.NoVariance
  | Ast_505.Asttypes.Bivariant -> Ast_504.Asttypes.Bivariant

and copy_injectivity :
    Ast_505.Asttypes.injectivity -> Ast_504.Asttypes.injectivity = function
  | Ast_505.Asttypes.Injective -> Ast_504.Asttypes.Injective
  | Ast_505.Asttypes.NoInjectivity -> Ast_504.Asttypes.NoInjectivity

and copy_constant : Ast_505.Parsetree.constant -> Ast_504.Parsetree.constant =
 fun { Ast_505.Parsetree.pconst_desc; Ast_505.Parsetree.pconst_loc } ->
  {
    Ast_504.Parsetree.pconst_desc = copy_constant_desc pconst_desc;
    Ast_504.Parsetree.pconst_loc = copy_location pconst_loc;
  }

and copy_constant_desc :
    Ast_505.Parsetree.constant_desc -> Ast_504.Parsetree.constant_desc =
  function
  | Ast_505.Parsetree.Pconst_integer (x0, x1) ->
      Ast_504.Parsetree.Pconst_integer (x0, Option.map (fun x -> x) x1)
  | Ast_505.Parsetree.Pconst_char x0 -> Ast_504.Parsetree.Pconst_char x0
  | Ast_505.Parsetree.Pconst_string (x0, x1, x2) ->
      Ast_504.Parsetree.Pconst_string
        (x0, copy_location x1, Option.map (fun x -> x) x2)
  | Ast_505.Parsetree.Pconst_float (x0, x1) ->
      Ast_504.Parsetree.Pconst_float (x0, Option.map (fun x -> x) x1)

and copy_location_stack :
    Ast_505.Parsetree.location_stack -> Ast_504.Parsetree.location_stack =
 fun x -> List.map copy_location x

and copy_attribute : Ast_505.Parsetree.attribute -> Ast_504.Parsetree.attribute
    =
 fun {
       Ast_505.Parsetree.attr_name;
       Ast_505.Parsetree.attr_payload;
       Ast_505.Parsetree.attr_loc;
     } ->
  {
    Ast_504.Parsetree.attr_name = copy_loc (fun x -> x) attr_name;
    Ast_504.Parsetree.attr_payload = copy_payload attr_payload;
    Ast_504.Parsetree.attr_loc = copy_location attr_loc;
  }

and copy_extension : Ast_505.Parsetree.extension -> Ast_504.Parsetree.extension
    =
 fun x ->
  let x0, x1 = x in
  (copy_loc (fun x -> x) x0, copy_payload x1)

and copy_attributes :
    Ast_505.Parsetree.attributes -> Ast_504.Parsetree.attributes =
 fun x -> List.map copy_attribute x

and copy_payload : Ast_505.Parsetree.payload -> Ast_504.Parsetree.payload =
  function
  | Ast_505.Parsetree.PStr x0 -> Ast_504.Parsetree.PStr (copy_structure x0)
  | Ast_505.Parsetree.PSig x0 -> Ast_504.Parsetree.PSig (copy_signature x0)
  | Ast_505.Parsetree.PTyp x0 -> Ast_504.Parsetree.PTyp (copy_core_type x0)
  | Ast_505.Parsetree.PPat (x0, x1) ->
      Ast_504.Parsetree.PPat (copy_pattern x0, Option.map copy_expression x1)

and copy_core_type : Ast_505.Parsetree.core_type -> Ast_504.Parsetree.core_type
    =
 fun {
       Ast_505.Parsetree.ptyp_desc;
       Ast_505.Parsetree.ptyp_loc;
       Ast_505.Parsetree.ptyp_loc_stack;
       Ast_505.Parsetree.ptyp_attributes;
     } ->
  let loc = copy_location ptyp_loc in
  {
    Ast_504.Parsetree.ptyp_desc = copy_core_type_desc ~loc ptyp_desc;
    Ast_504.Parsetree.ptyp_loc = loc;
    Ast_504.Parsetree.ptyp_loc_stack = copy_location_stack ptyp_loc_stack;
    Ast_504.Parsetree.ptyp_attributes = copy_attributes ptyp_attributes;
  }

and copy_core_type_desc :
    loc:Location.t ->
    Ast_505.Parsetree.core_type_desc ->
    Ast_504.Parsetree.core_type_desc =
 fun ~loc -> function
  | Ast_505.Parsetree.Ptyp_any -> Ast_504.Parsetree.Ptyp_any
  | Ast_505.Parsetree.Ptyp_var x0 -> Ast_504.Parsetree.Ptyp_var x0
  | Ast_505.Parsetree.Ptyp_arrow (x0, x1, x2) ->
      Ast_504.Parsetree.Ptyp_arrow
        (copy_arg_label x0, copy_core_type x1, copy_core_type x2)
  | Ast_505.Parsetree.Ptyp_tuple x0 ->
      Ast_504.Parsetree.Ptyp_tuple
        (List.map
           (fun x ->
             let x0, x1 = x in
             (Option.map (fun x -> x) x0, copy_core_type x1))
           x0)
  | Ast_505.Parsetree.Ptyp_constr (x0, x1) ->
      Ast_504.Parsetree.Ptyp_constr
        (copy_loc copy_longident x0, List.map copy_core_type x1)
  | Ast_505.Parsetree.Ptyp_object (x0, x1) ->
      Ast_504.Parsetree.Ptyp_object
        (List.map copy_object_field x0, copy_closed_flag x1)
  | Ast_505.Parsetree.Ptyp_class (x0, x1) ->
      Ast_504.Parsetree.Ptyp_class
        (copy_loc copy_longident x0, List.map copy_core_type x1)
  | Ast_505.Parsetree.Ptyp_alias (x0, x1) ->
      Ast_504.Parsetree.Ptyp_alias (copy_core_type x0, copy_loc (fun x -> x) x1)
  | Ast_505.Parsetree.Ptyp_variant (x0, x1, x2) ->
      Ast_504.Parsetree.Ptyp_variant
        ( List.map copy_row_field x0,
          copy_closed_flag x1,
          Option.map (List.map copy_label) x2 )
  | Ast_505.Parsetree.Ptyp_poly (x0, x1) ->
      Ast_504.Parsetree.Ptyp_poly
        (List.map (copy_loc (fun x -> x)) x0, copy_core_type x1)
  | Ast_505.Parsetree.Ptyp_package x0 ->
      Ast_504.Parsetree.Ptyp_package (copy_package_type x0)
  | Ast_505.Parsetree.Ptyp_open (x0, x1) ->
      Ast_504.Parsetree.Ptyp_open (copy_loc copy_longident x0, copy_core_type x1)
  | Ast_505.Parsetree.Ptyp_extension x0 ->
      Ast_504.Parsetree.Ptyp_extension (copy_extension x0)
  | Ast_505.Parsetree.Ptyp_functor (lbl, name, pkg, typ) ->
      let lbl = copy_arg_label lbl in
      let pkg = copy_package_type pkg in
      let typ = copy_core_type typ in
      Encoding_505.To_504.encode_ptyp_functor ~loc (lbl, name, pkg, typ)

and copy_package_type :
    Ast_505.Parsetree.package_type -> Ast_504.Parsetree.package_type =
 fun {
       Ast_505.Parsetree.ppt_path;
       Ast_505.Parsetree.ppt_constraints;
       Ast_505.Parsetree.ppt_loc;
       Ast_505.Parsetree.ppt_attrs;
     } ->
  {
    Ast_504.Parsetree.ppt_path = copy_loc copy_longident ppt_path;
    Ast_504.Parsetree.ppt_cstrs =
      List.map
        (fun x ->
          let x0, x1 = x in
          (copy_loc copy_longident x0, copy_core_type x1))
        ppt_constraints;
    Ast_504.Parsetree.ppt_loc = copy_location ppt_loc;
    Ast_504.Parsetree.ppt_attrs = copy_attributes ppt_attrs;
  }

and copy_row_field : Ast_505.Parsetree.row_field -> Ast_504.Parsetree.row_field
    =
 fun {
       Ast_505.Parsetree.prf_desc;
       Ast_505.Parsetree.prf_loc;
       Ast_505.Parsetree.prf_attributes;
     } ->
  {
    Ast_504.Parsetree.prf_desc = copy_row_field_desc prf_desc;
    Ast_504.Parsetree.prf_loc = copy_location prf_loc;
    Ast_504.Parsetree.prf_attributes = copy_attributes prf_attributes;
  }

and copy_row_field_desc :
    Ast_505.Parsetree.row_field_desc -> Ast_504.Parsetree.row_field_desc =
  function
  | Ast_505.Parsetree.Rtag (x0, x1, x2) ->
      Ast_504.Parsetree.Rtag
        (copy_loc copy_label x0, x1, List.map copy_core_type x2)
  | Ast_505.Parsetree.Rinherit x0 ->
      Ast_504.Parsetree.Rinherit (copy_core_type x0)

and copy_object_field :
    Ast_505.Parsetree.object_field -> Ast_504.Parsetree.object_field =
 fun {
       Ast_505.Parsetree.pof_desc;
       Ast_505.Parsetree.pof_loc;
       Ast_505.Parsetree.pof_attributes;
     } ->
  {
    Ast_504.Parsetree.pof_desc = copy_object_field_desc pof_desc;
    Ast_504.Parsetree.pof_loc = copy_location pof_loc;
    Ast_504.Parsetree.pof_attributes = copy_attributes pof_attributes;
  }

and copy_object_field_desc :
    Ast_505.Parsetree.object_field_desc -> Ast_504.Parsetree.object_field_desc =
  function
  | Ast_505.Parsetree.Otag (x0, x1) ->
      Ast_504.Parsetree.Otag (copy_loc copy_label x0, copy_core_type x1)
  | Ast_505.Parsetree.Oinherit x0 ->
      Ast_504.Parsetree.Oinherit (copy_core_type x0)

and copy_pattern : Ast_505.Parsetree.pattern -> Ast_504.Parsetree.pattern =
 fun {
       Ast_505.Parsetree.ppat_desc;
       Ast_505.Parsetree.ppat_loc;
       Ast_505.Parsetree.ppat_loc_stack;
       Ast_505.Parsetree.ppat_attributes;
     } ->
  {
    Ast_504.Parsetree.ppat_desc = copy_pattern_desc ~loc:ppat_loc ppat_desc;
    Ast_504.Parsetree.ppat_loc = copy_location ppat_loc;
    Ast_504.Parsetree.ppat_loc_stack = copy_location_stack ppat_loc_stack;
    Ast_504.Parsetree.ppat_attributes = copy_attributes ppat_attributes;
  }

and copy_pattern_desc ~loc :
    Ast_505.Parsetree.pattern_desc -> Ast_504.Parsetree.pattern_desc = function
  | Ast_505.Parsetree.Ppat_any -> Ast_504.Parsetree.Ppat_any
  | Ast_505.Parsetree.Ppat_var x0 ->
      Ast_504.Parsetree.Ppat_var (copy_loc (fun x -> x) x0)
  | Ast_505.Parsetree.Ppat_alias (x0, x1) ->
      Ast_504.Parsetree.Ppat_alias (copy_pattern x0, copy_loc (fun x -> x) x1)
  | Ast_505.Parsetree.Ppat_constant x0 ->
      Ast_504.Parsetree.Ppat_constant (copy_constant x0)
  | Ast_505.Parsetree.Ppat_interval (x0, x1) ->
      Ast_504.Parsetree.Ppat_interval (copy_constant x0, copy_constant x1)
  | Ast_505.Parsetree.Ppat_tuple (x0, x1) ->
      Ast_504.Parsetree.Ppat_tuple
        ( List.map
            (fun x ->
              let x0, x1 = x in
              (Option.map (fun x -> x) x0, copy_pattern x1))
            x0,
          copy_closed_flag x1 )
  | Ast_505.Parsetree.Ppat_construct (x0, x1) ->
      Ast_504.Parsetree.Ppat_construct
        ( copy_loc copy_longident x0,
          Option.map
            (fun x ->
              let x0, x1 = x in
              (List.map (copy_loc (fun x -> x)) x0, copy_pattern x1))
            x1 )
  | Ast_505.Parsetree.Ppat_variant (x0, x1) ->
      Ast_504.Parsetree.Ppat_variant (copy_label x0, Option.map copy_pattern x1)
  | Ast_505.Parsetree.Ppat_record (x0, x1) ->
      Ast_504.Parsetree.Ppat_record
        ( List.map
            (fun x ->
              let x0, x1 = x in
              (copy_loc copy_longident x0, copy_pattern x1))
            x0,
          copy_closed_flag x1 )
  | Ast_505.Parsetree.Ppat_array x0 ->
      Ast_504.Parsetree.Ppat_array (List.map copy_pattern x0)
  | Ast_505.Parsetree.Ppat_or (x0, x1) ->
      Ast_504.Parsetree.Ppat_or (copy_pattern x0, copy_pattern x1)
  | Ast_505.Parsetree.Ppat_constraint
      ( ({ ppat_desc = Ppat_unpack (_, None); _ } as x0),
        ({ ptyp_desc = Ptyp_package _; _ } as x1) ) ->
      let x0' = copy_pattern x0 in
      let x1' = copy_core_type x1 in
      Encoding_505.To_504.preserve_ppat_constraint x0' x1'
  | Ast_505.Parsetree.Ppat_constraint (x0, x1) ->
      Ast_504.Parsetree.Ppat_constraint (copy_pattern x0, copy_core_type x1)
  | Ast_505.Parsetree.Ppat_type x0 ->
      Ast_504.Parsetree.Ppat_type (copy_loc copy_longident x0)
  | Ast_505.Parsetree.Ppat_lazy x0 ->
      Ast_504.Parsetree.Ppat_lazy (copy_pattern x0)
  | Ast_505.Parsetree.Ppat_unpack (x0, x1) -> (
      let unpack =
        Ast_504.Parsetree.Ppat_unpack (copy_loc (Option.map (fun x -> x)) x0)
      in
      match x1 with
      | None -> unpack
      | Some c ->
          let ghost_loc = { loc with Location.loc_ghost = true } in
          let unpack_pattern : Ast_504.Parsetree.pattern =
            {
              ppat_desc = unpack;
              ppat_loc = ghost_loc;
              ppat_attributes = [];
              ppat_loc_stack = [];
            }
          in
          let package_type : Ast_504.Parsetree.core_type =
            {
              ptyp_desc = Ptyp_package (copy_package_type c);
              ptyp_loc = ghost_loc;
              ptyp_attributes = [];
              ptyp_loc_stack = [];
            }
          in
          Ast_504.Parsetree.Ppat_constraint (unpack_pattern, package_type))
  | Ast_505.Parsetree.Ppat_exception x0 ->
      Ast_504.Parsetree.Ppat_exception (copy_pattern x0)
  | Ast_505.Parsetree.Ppat_effect (x0, x1) ->
      Ast_504.Parsetree.Ppat_effect (copy_pattern x0, copy_pattern x1)
  | Ast_505.Parsetree.Ppat_extension x0 ->
      Ast_504.Parsetree.Ppat_extension (copy_extension x0)
  | Ast_505.Parsetree.Ppat_open (x0, x1) ->
      Ast_504.Parsetree.Ppat_open (copy_loc copy_longident x0, copy_pattern x1)

and copy_expression :
    Ast_505.Parsetree.expression -> Ast_504.Parsetree.expression =
 fun {
       Ast_505.Parsetree.pexp_desc;
       Ast_505.Parsetree.pexp_loc;
       Ast_505.Parsetree.pexp_loc_stack;
       Ast_505.Parsetree.pexp_attributes;
     } ->
  {
    Ast_504.Parsetree.pexp_desc = copy_expression_desc pexp_desc;
    Ast_504.Parsetree.pexp_loc = copy_location pexp_loc;
    Ast_504.Parsetree.pexp_loc_stack = copy_location_stack pexp_loc_stack;
    Ast_504.Parsetree.pexp_attributes = copy_attributes pexp_attributes;
  }

and copy_expression_desc :
    Ast_505.Parsetree.expression_desc -> Ast_504.Parsetree.expression_desc =
  function
  | Ast_505.Parsetree.Pexp_ident x0 ->
      Ast_504.Parsetree.Pexp_ident (copy_loc copy_longident x0)
  | Ast_505.Parsetree.Pexp_constant x0 ->
      Ast_504.Parsetree.Pexp_constant (copy_constant x0)
  | Ast_505.Parsetree.Pexp_let (x0, x1, x2) ->
      Ast_504.Parsetree.Pexp_let
        (copy_rec_flag x0, List.map copy_value_binding x1, copy_expression x2)
  | Ast_505.Parsetree.Pexp_function (x0, x1, x2) ->
      Ast_504.Parsetree.Pexp_function
        ( List.map copy_function_param x0,
          Option.map copy_type_constraint x1,
          copy_function_body x2 )
  | Ast_505.Parsetree.Pexp_apply (x0, x1) ->
      Ast_504.Parsetree.Pexp_apply
        ( copy_expression x0,
          List.map
            (fun x ->
              let x0, x1 = x in
              (copy_arg_label x0, copy_expression x1))
            x1 )
  | Ast_505.Parsetree.Pexp_match (x0, x1) ->
      Ast_504.Parsetree.Pexp_match (copy_expression x0, List.map copy_case x1)
  | Ast_505.Parsetree.Pexp_try (x0, x1) ->
      Ast_504.Parsetree.Pexp_try (copy_expression x0, List.map copy_case x1)
  | Ast_505.Parsetree.Pexp_tuple x0 ->
      Ast_504.Parsetree.Pexp_tuple
        (List.map
           (fun x ->
             let x0, x1 = x in
             (Option.map (fun x -> x) x0, copy_expression x1))
           x0)
  | Ast_505.Parsetree.Pexp_construct (x0, x1) ->
      Ast_504.Parsetree.Pexp_construct
        (copy_loc copy_longident x0, Option.map copy_expression x1)
  | Ast_505.Parsetree.Pexp_variant (x0, x1) ->
      Ast_504.Parsetree.Pexp_variant
        (copy_label x0, Option.map copy_expression x1)
  | Ast_505.Parsetree.Pexp_record (x0, x1) ->
      Ast_504.Parsetree.Pexp_record
        ( List.map
            (fun x ->
              let x0, x1 = x in
              (copy_loc copy_longident x0, copy_expression x1))
            x0,
          Option.map copy_expression x1 )
  | Ast_505.Parsetree.Pexp_field (x0, x1) ->
      Ast_504.Parsetree.Pexp_field
        (copy_expression x0, copy_loc copy_longident x1)
  | Ast_505.Parsetree.Pexp_setfield (x0, x1, x2) ->
      Ast_504.Parsetree.Pexp_setfield
        (copy_expression x0, copy_loc copy_longident x1, copy_expression x2)
  | Ast_505.Parsetree.Pexp_array x0 ->
      Ast_504.Parsetree.Pexp_array (List.map copy_expression x0)
  | Ast_505.Parsetree.Pexp_ifthenelse (x0, x1, x2) ->
      Ast_504.Parsetree.Pexp_ifthenelse
        (copy_expression x0, copy_expression x1, Option.map copy_expression x2)
  | Ast_505.Parsetree.Pexp_sequence (x0, x1) ->
      Ast_504.Parsetree.Pexp_sequence (copy_expression x0, copy_expression x1)
  | Ast_505.Parsetree.Pexp_while (x0, x1) ->
      Ast_504.Parsetree.Pexp_while (copy_expression x0, copy_expression x1)
  | Ast_505.Parsetree.Pexp_for (x0, x1, x2, x3, x4) ->
      Ast_504.Parsetree.Pexp_for
        ( copy_pattern x0,
          copy_expression x1,
          copy_expression x2,
          copy_direction_flag x3,
          copy_expression x4 )
  | Ast_505.Parsetree.Pexp_constraint (x0, x1) ->
      Ast_504.Parsetree.Pexp_constraint (copy_expression x0, copy_core_type x1)
  | Ast_505.Parsetree.Pexp_coerce (x0, x1, x2) ->
      Ast_504.Parsetree.Pexp_coerce
        (copy_expression x0, Option.map copy_core_type x1, copy_core_type x2)
  | Ast_505.Parsetree.Pexp_send (x0, x1) ->
      Ast_504.Parsetree.Pexp_send (copy_expression x0, copy_loc copy_label x1)
  | Ast_505.Parsetree.Pexp_new x0 ->
      Ast_504.Parsetree.Pexp_new (copy_loc copy_longident x0)
  | Ast_505.Parsetree.Pexp_setinstvar (x0, x1) ->
      Ast_504.Parsetree.Pexp_setinstvar
        (copy_loc copy_label x0, copy_expression x1)
  | Ast_505.Parsetree.Pexp_override x0 ->
      Ast_504.Parsetree.Pexp_override
        (List.map
           (fun x ->
             let x0, x1 = x in
             (copy_loc copy_label x0, copy_expression x1))
           x0)
  | Ast_505.Parsetree.Pexp_struct_item (x0, x1) -> (
      match x0.pstr_desc with
      | Pstr_open odecl ->
          Ast_504.Parsetree.Pexp_open
            (copy_open_declaration odecl, copy_expression x1)
      | Pstr_exception tyexn ->
          let ex_cstr = tyexn.ptyexn_constructor in
          Ast_504.Parsetree.Pexp_letexception
            (copy_extension_constructor ex_cstr, copy_expression x1)
      | Pstr_module mb ->
          let mname = mb.pmb_name in
          let mexpr = mb.pmb_expr in
          Ast_504.Parsetree.Pexp_letmodule
            ( copy_loc (Option.map Fun.id) mname,
              copy_module_expr mexpr,
              copy_expression x1 )
      | _ ->
          let item = copy_structure_item x0 in
          Encoding_505.To_504.encode_pexp_struct_item ~loc:x0.pstr_loc
            (item, copy_expression x1))
  | Ast_505.Parsetree.Pexp_assert x0 ->
      Ast_504.Parsetree.Pexp_assert (copy_expression x0)
  | Ast_505.Parsetree.Pexp_lazy x0 ->
      Ast_504.Parsetree.Pexp_lazy (copy_expression x0)
  | Ast_505.Parsetree.Pexp_poly (x0, x1) ->
      Ast_504.Parsetree.Pexp_poly
        (copy_expression x0, Option.map copy_core_type x1)
  | Ast_505.Parsetree.Pexp_object x0 ->
      Ast_504.Parsetree.Pexp_object (copy_class_structure x0)
  | Ast_505.Parsetree.Pexp_newtype (x0, x1) ->
      Ast_504.Parsetree.Pexp_newtype
        (copy_loc (fun x -> x) x0, copy_expression x1)
  | Ast_505.Parsetree.Pexp_pack (x0, x1) ->
      Ast_504.Parsetree.Pexp_pack
        (copy_module_expr x0, Option.map copy_package_type x1)
  | Ast_505.Parsetree.Pexp_letop x0 ->
      Ast_504.Parsetree.Pexp_letop (copy_letop x0)
  | Ast_505.Parsetree.Pexp_extension x0 ->
      Ast_504.Parsetree.Pexp_extension (copy_extension x0)
  | Ast_505.Parsetree.Pexp_unreachable -> Ast_504.Parsetree.Pexp_unreachable

and copy_case : Ast_505.Parsetree.case -> Ast_504.Parsetree.case =
 fun {
       Ast_505.Parsetree.pc_lhs;
       Ast_505.Parsetree.pc_guard;
       Ast_505.Parsetree.pc_rhs;
     } ->
  {
    Ast_504.Parsetree.pc_lhs = copy_pattern pc_lhs;
    Ast_504.Parsetree.pc_guard = Option.map copy_expression pc_guard;
    Ast_504.Parsetree.pc_rhs = copy_expression pc_rhs;
  }

and copy_letop : Ast_505.Parsetree.letop -> Ast_504.Parsetree.letop =
 fun { Ast_505.Parsetree.let_; Ast_505.Parsetree.ands; Ast_505.Parsetree.body } ->
  {
    Ast_504.Parsetree.let_ = copy_binding_op let_;
    Ast_504.Parsetree.ands = List.map copy_binding_op ands;
    Ast_504.Parsetree.body = copy_expression body;
  }

and copy_binding_op :
    Ast_505.Parsetree.binding_op -> Ast_504.Parsetree.binding_op =
 fun {
       Ast_505.Parsetree.pbop_op;
       Ast_505.Parsetree.pbop_pat;
       Ast_505.Parsetree.pbop_exp;
       Ast_505.Parsetree.pbop_loc;
     } ->
  {
    Ast_504.Parsetree.pbop_op = copy_loc (fun x -> x) pbop_op;
    Ast_504.Parsetree.pbop_pat = copy_pattern pbop_pat;
    Ast_504.Parsetree.pbop_exp = copy_expression pbop_exp;
    Ast_504.Parsetree.pbop_loc = copy_location pbop_loc;
  }

and copy_function_param_desc :
    Ast_505.Parsetree.function_param_desc ->
    Ast_504.Parsetree.function_param_desc = function
  | Ast_505.Parsetree.Pparam_val (x0, x1, x2) ->
      Ast_504.Parsetree.Pparam_val
        (copy_arg_label x0, Option.map copy_expression x1, copy_pattern x2)
  | Ast_505.Parsetree.Pparam_newtype x0 ->
      Ast_504.Parsetree.Pparam_newtype (copy_loc (fun x -> x) x0)

and copy_function_param :
    Ast_505.Parsetree.function_param -> Ast_504.Parsetree.function_param =
 fun { Ast_505.Parsetree.pparam_loc; Ast_505.Parsetree.pparam_desc } ->
  {
    Ast_504.Parsetree.pparam_loc = copy_location pparam_loc;
    Ast_504.Parsetree.pparam_desc = copy_function_param_desc pparam_desc;
  }

and copy_function_body :
    Ast_505.Parsetree.function_body -> Ast_504.Parsetree.function_body =
  function
  | Ast_505.Parsetree.Pfunction_body x0 ->
      Ast_504.Parsetree.Pfunction_body (copy_expression x0)
  | Ast_505.Parsetree.Pfunction_cases (x0, x1, x2) ->
      Ast_504.Parsetree.Pfunction_cases
        (List.map copy_case x0, copy_location x1, copy_attributes x2)

and copy_type_constraint :
    Ast_505.Parsetree.type_constraint -> Ast_504.Parsetree.type_constraint =
  function
  | Ast_505.Parsetree.Pconstraint x0 ->
      Ast_504.Parsetree.Pconstraint (copy_core_type x0)
  | Ast_505.Parsetree.Pcoerce (x0, x1) ->
      Ast_504.Parsetree.Pcoerce (Option.map copy_core_type x0, copy_core_type x1)

and copy_value_description :
    Ast_505.Parsetree.value_description -> Ast_504.Parsetree.value_description =
 fun {
       Ast_505.Parsetree.pval_name;
       Ast_505.Parsetree.pval_type;
       Ast_505.Parsetree.pval_prim;
       Ast_505.Parsetree.pval_attributes;
       Ast_505.Parsetree.pval_loc;
     } ->
  {
    Ast_504.Parsetree.pval_name = copy_loc (fun x -> x) pval_name;
    Ast_504.Parsetree.pval_type = copy_core_type pval_type;
    Ast_504.Parsetree.pval_prim = List.map (fun x -> x) pval_prim;
    Ast_504.Parsetree.pval_attributes = copy_attributes pval_attributes;
    Ast_504.Parsetree.pval_loc = copy_location pval_loc;
  }

and copy_type_declaration_ :
    Ast_505.Parsetree.type_declaration ->
    Ast_504.Parsetree.type_declaration External_type.may_contain_external_type =
 fun {
       Ast_505.Parsetree.ptype_name;
       Ast_505.Parsetree.ptype_params;
       Ast_505.Parsetree.ptype_constraints;
       Ast_505.Parsetree.ptype_kind;
       Ast_505.Parsetree.ptype_private;
       Ast_505.Parsetree.ptype_manifest;
       Ast_505.Parsetree.ptype_attributes;
       Ast_505.Parsetree.ptype_loc;
     } ->
  let loc = copy_location ptype_loc in
  let contains_external_type, (ptype_kind, ptype_attributes) =
    let attributes = copy_attributes ptype_attributes in
    match ptype_kind with
    | Ptype_external name ->
        ( true,
          Encoding_505.To_504.encode_ptype_kind_external ~loc name attributes )
    | ptype_kind -> (false, (copy_type_kind ptype_kind, attributes))
  in
  let td =
    {
      Ast_504.Parsetree.ptype_name = copy_loc (fun x -> x) ptype_name;
      Ast_504.Parsetree.ptype_params =
        List.map
          (fun x ->
            let x0, x1 = x in
            ( copy_core_type x0,
              let x0, x1 = x1 in
              (copy_variance x0, copy_injectivity x1) ))
          ptype_params;
      Ast_504.Parsetree.ptype_cstrs =
        List.map
          (fun x ->
            let x0, x1, x2 = x in
            (copy_core_type x0, copy_core_type x1, copy_location x2))
          ptype_constraints;
      Ast_504.Parsetree.ptype_kind;
      Ast_504.Parsetree.ptype_private = copy_private_flag ptype_private;
      Ast_504.Parsetree.ptype_manifest =
        Option.map copy_core_type ptype_manifest;
      Ast_504.Parsetree.ptype_attributes;
      Ast_504.Parsetree.ptype_loc = loc;
    }
  in
  External_type.wrap ~contains_external_type td

and copy_type_declaration_list l =
  External_type.list_map ~f:copy_type_declaration_ l

and copy_type_kind : Ast_505.Parsetree.type_kind -> Ast_504.Parsetree.type_kind
    = function
  | Ast_505.Parsetree.Ptype_abstract -> Ast_504.Parsetree.Ptype_abstract
  | Ast_505.Parsetree.Ptype_variant x0 ->
      Ast_504.Parsetree.Ptype_variant (List.map copy_constructor_declaration x0)
  | Ast_505.Parsetree.Ptype_record x0 ->
      Ast_504.Parsetree.Ptype_record (List.map copy_label_declaration x0)
  | Ast_505.Parsetree.Ptype_open -> Ast_504.Parsetree.Ptype_open
  | Ast_505.Parsetree.Ptype_external _ -> External_type.err ~loc:Location.none

and copy_label_declaration :
    Ast_505.Parsetree.label_declaration -> Ast_504.Parsetree.label_declaration =
 fun {
       Ast_505.Parsetree.pld_name;
       Ast_505.Parsetree.pld_mutable;
       Ast_505.Parsetree.pld_type;
       Ast_505.Parsetree.pld_loc;
       Ast_505.Parsetree.pld_attributes;
     } ->
  {
    Ast_504.Parsetree.pld_name = copy_loc (fun x -> x) pld_name;
    Ast_504.Parsetree.pld_mutable = copy_mutable_flag pld_mutable;
    Ast_504.Parsetree.pld_type = copy_core_type pld_type;
    Ast_504.Parsetree.pld_loc = copy_location pld_loc;
    Ast_504.Parsetree.pld_attributes = copy_attributes pld_attributes;
  }

and copy_constructor_declaration :
    Ast_505.Parsetree.constructor_declaration ->
    Ast_504.Parsetree.constructor_declaration =
 fun {
       Ast_505.Parsetree.pcd_name;
       Ast_505.Parsetree.pcd_vars;
       Ast_505.Parsetree.pcd_args;
       Ast_505.Parsetree.pcd_res;
       Ast_505.Parsetree.pcd_loc;
       Ast_505.Parsetree.pcd_attributes;
     } ->
  {
    Ast_504.Parsetree.pcd_name = copy_loc (fun x -> x) pcd_name;
    Ast_504.Parsetree.pcd_vars = List.map (copy_loc (fun x -> x)) pcd_vars;
    Ast_504.Parsetree.pcd_args = copy_constructor_arguments pcd_args;
    Ast_504.Parsetree.pcd_res = Option.map copy_core_type pcd_res;
    Ast_504.Parsetree.pcd_loc = copy_location pcd_loc;
    Ast_504.Parsetree.pcd_attributes = copy_attributes pcd_attributes;
  }

and copy_constructor_arguments :
    Ast_505.Parsetree.constructor_arguments ->
    Ast_504.Parsetree.constructor_arguments = function
  | Ast_505.Parsetree.Pcstr_tuple x0 ->
      Ast_504.Parsetree.Pcstr_tuple (List.map copy_core_type x0)
  | Ast_505.Parsetree.Pcstr_record x0 ->
      Ast_504.Parsetree.Pcstr_record (List.map copy_label_declaration x0)

and copy_type_extension :
    Ast_505.Parsetree.type_extension -> Ast_504.Parsetree.type_extension =
 fun {
       Ast_505.Parsetree.ptyext_path;
       Ast_505.Parsetree.ptyext_params;
       Ast_505.Parsetree.ptyext_constructors;
       Ast_505.Parsetree.ptyext_private;
       Ast_505.Parsetree.ptyext_loc;
       Ast_505.Parsetree.ptyext_attributes;
     } ->
  {
    Ast_504.Parsetree.ptyext_path = copy_loc copy_longident ptyext_path;
    Ast_504.Parsetree.ptyext_params =
      List.map
        (fun x ->
          let x0, x1 = x in
          ( copy_core_type x0,
            let x0, x1 = x1 in
            (copy_variance x0, copy_injectivity x1) ))
        ptyext_params;
    Ast_504.Parsetree.ptyext_constructors =
      List.map copy_extension_constructor ptyext_constructors;
    Ast_504.Parsetree.ptyext_private = copy_private_flag ptyext_private;
    Ast_504.Parsetree.ptyext_loc = copy_location ptyext_loc;
    Ast_504.Parsetree.ptyext_attributes = copy_attributes ptyext_attributes;
  }

and copy_extension_constructor :
    Ast_505.Parsetree.extension_constructor ->
    Ast_504.Parsetree.extension_constructor =
 fun {
       Ast_505.Parsetree.pext_name;
       Ast_505.Parsetree.pext_kind;
       Ast_505.Parsetree.pext_loc;
       Ast_505.Parsetree.pext_attributes;
     } ->
  {
    Ast_504.Parsetree.pext_name = copy_loc (fun x -> x) pext_name;
    Ast_504.Parsetree.pext_kind = copy_extension_constructor_kind pext_kind;
    Ast_504.Parsetree.pext_loc = copy_location pext_loc;
    Ast_504.Parsetree.pext_attributes = copy_attributes pext_attributes;
  }

and copy_type_exception :
    Ast_505.Parsetree.type_exception -> Ast_504.Parsetree.type_exception =
 fun {
       Ast_505.Parsetree.ptyexn_constructor;
       Ast_505.Parsetree.ptyexn_loc;
       Ast_505.Parsetree.ptyexn_attributes;
     } ->
  {
    Ast_504.Parsetree.ptyexn_constructor =
      copy_extension_constructor ptyexn_constructor;
    Ast_504.Parsetree.ptyexn_loc = copy_location ptyexn_loc;
    Ast_504.Parsetree.ptyexn_attributes = copy_attributes ptyexn_attributes;
  }

and copy_extension_constructor_kind :
    Ast_505.Parsetree.extension_constructor_kind ->
    Ast_504.Parsetree.extension_constructor_kind = function
  | Ast_505.Parsetree.Pext_decl (x0, x1, x2) ->
      Ast_504.Parsetree.Pext_decl
        ( List.map (copy_loc (fun x -> x)) x0,
          copy_constructor_arguments x1,
          Option.map copy_core_type x2 )
  | Ast_505.Parsetree.Pext_rebind x0 ->
      Ast_504.Parsetree.Pext_rebind (copy_loc copy_longident x0)

and copy_class_type :
    Ast_505.Parsetree.class_type -> Ast_504.Parsetree.class_type =
 fun {
       Ast_505.Parsetree.pcty_desc;
       Ast_505.Parsetree.pcty_loc;
       Ast_505.Parsetree.pcty_attributes;
     } ->
  {
    Ast_504.Parsetree.pcty_desc = copy_class_type_desc pcty_desc;
    Ast_504.Parsetree.pcty_loc = copy_location pcty_loc;
    Ast_504.Parsetree.pcty_attributes = copy_attributes pcty_attributes;
  }

and copy_class_type_desc :
    Ast_505.Parsetree.class_type_desc -> Ast_504.Parsetree.class_type_desc =
  function
  | Ast_505.Parsetree.Pcty_constr (x0, x1) ->
      Ast_504.Parsetree.Pcty_constr
        (copy_loc copy_longident x0, List.map copy_core_type x1)
  | Ast_505.Parsetree.Pcty_signature x0 ->
      Ast_504.Parsetree.Pcty_signature (copy_class_signature x0)
  | Ast_505.Parsetree.Pcty_arrow (x0, x1, x2) ->
      Ast_504.Parsetree.Pcty_arrow
        (copy_arg_label x0, copy_core_type x1, copy_class_type x2)
  | Ast_505.Parsetree.Pcty_extension x0 ->
      Ast_504.Parsetree.Pcty_extension (copy_extension x0)
  | Ast_505.Parsetree.Pcty_open (x0, x1) ->
      Ast_504.Parsetree.Pcty_open (copy_open_description x0, copy_class_type x1)

and copy_class_signature :
    Ast_505.Parsetree.class_signature -> Ast_504.Parsetree.class_signature =
 fun { Ast_505.Parsetree.pcsig_self; Ast_505.Parsetree.pcsig_fields } ->
  {
    Ast_504.Parsetree.pcsig_self = copy_core_type pcsig_self;
    Ast_504.Parsetree.pcsig_fields = List.map copy_class_type_field pcsig_fields;
  }

and copy_class_type_field :
    Ast_505.Parsetree.class_type_field -> Ast_504.Parsetree.class_type_field =
 fun {
       Ast_505.Parsetree.pctf_desc;
       Ast_505.Parsetree.pctf_loc;
       Ast_505.Parsetree.pctf_attributes;
     } ->
  {
    Ast_504.Parsetree.pctf_desc = copy_class_type_field_desc pctf_desc;
    Ast_504.Parsetree.pctf_loc = copy_location pctf_loc;
    Ast_504.Parsetree.pctf_attributes = copy_attributes pctf_attributes;
  }

and copy_class_type_field_desc :
    Ast_505.Parsetree.class_type_field_desc ->
    Ast_504.Parsetree.class_type_field_desc = function
  | Ast_505.Parsetree.Pctf_inherit x0 ->
      Ast_504.Parsetree.Pctf_inherit (copy_class_type x0)
  | Ast_505.Parsetree.Pctf_val x0 ->
      Ast_504.Parsetree.Pctf_val
        (let x0, x1, x2, x3 = x0 in
         ( copy_loc copy_label x0,
           copy_mutable_flag x1,
           copy_virtual_flag x2,
           copy_core_type x3 ))
  | Ast_505.Parsetree.Pctf_method x0 ->
      Ast_504.Parsetree.Pctf_method
        (let x0, x1, x2, x3 = x0 in
         ( copy_loc copy_label x0,
           copy_private_flag x1,
           copy_virtual_flag x2,
           copy_core_type x3 ))
  | Ast_505.Parsetree.Pctf_constraint x0 ->
      Ast_504.Parsetree.Pctf_constraint
        (let x0, x1 = x0 in
         (copy_core_type x0, copy_core_type x1))
  | Ast_505.Parsetree.Pctf_attribute x0 ->
      Ast_504.Parsetree.Pctf_attribute (copy_attribute x0)
  | Ast_505.Parsetree.Pctf_extension x0 ->
      Ast_504.Parsetree.Pctf_extension (copy_extension x0)

and copy_class_infos :
    'f0 'g0.
    ('f0 -> 'g0) ->
    'f0 Ast_505.Parsetree.class_infos ->
    'g0 Ast_504.Parsetree.class_infos =
 fun f0
     {
       Ast_505.Parsetree.pci_virt;
       Ast_505.Parsetree.pci_params;
       Ast_505.Parsetree.pci_name;
       Ast_505.Parsetree.pci_expr;
       Ast_505.Parsetree.pci_loc;
       Ast_505.Parsetree.pci_attributes;
     } ->
  {
    Ast_504.Parsetree.pci_virt = copy_virtual_flag pci_virt;
    Ast_504.Parsetree.pci_params =
      List.map
        (fun x ->
          let x0, x1 = x in
          ( copy_core_type x0,
            let x0, x1 = x1 in
            (copy_variance x0, copy_injectivity x1) ))
        pci_params;
    Ast_504.Parsetree.pci_name = copy_loc (fun x -> x) pci_name;
    Ast_504.Parsetree.pci_expr = f0 pci_expr;
    Ast_504.Parsetree.pci_loc = copy_location pci_loc;
    Ast_504.Parsetree.pci_attributes = copy_attributes pci_attributes;
  }

and copy_class_description :
    Ast_505.Parsetree.class_description -> Ast_504.Parsetree.class_description =
 fun x -> copy_class_infos copy_class_type x

and copy_class_type_declaration :
    Ast_505.Parsetree.class_type_declaration ->
    Ast_504.Parsetree.class_type_declaration =
 fun x -> copy_class_infos copy_class_type x

and copy_class_expr :
    Ast_505.Parsetree.class_expr -> Ast_504.Parsetree.class_expr =
 fun {
       Ast_505.Parsetree.pcl_desc;
       Ast_505.Parsetree.pcl_loc;
       Ast_505.Parsetree.pcl_attributes;
     } ->
  {
    Ast_504.Parsetree.pcl_desc = copy_class_expr_desc pcl_desc;
    Ast_504.Parsetree.pcl_loc = copy_location pcl_loc;
    Ast_504.Parsetree.pcl_attributes = copy_attributes pcl_attributes;
  }

and copy_class_expr_desc :
    Ast_505.Parsetree.class_expr_desc -> Ast_504.Parsetree.class_expr_desc =
  function
  | Ast_505.Parsetree.Pcl_constr (x0, x1) ->
      Ast_504.Parsetree.Pcl_constr
        (copy_loc copy_longident x0, List.map copy_core_type x1)
  | Ast_505.Parsetree.Pcl_structure x0 ->
      Ast_504.Parsetree.Pcl_structure (copy_class_structure x0)
  | Ast_505.Parsetree.Pcl_fun (x0, x1, x2, x3) ->
      Ast_504.Parsetree.Pcl_fun
        ( copy_arg_label x0,
          Option.map copy_expression x1,
          copy_pattern x2,
          copy_class_expr x3 )
  | Ast_505.Parsetree.Pcl_apply (x0, x1) ->
      Ast_504.Parsetree.Pcl_apply
        ( copy_class_expr x0,
          List.map
            (fun x ->
              let x0, x1 = x in
              (copy_arg_label x0, copy_expression x1))
            x1 )
  | Ast_505.Parsetree.Pcl_let (x0, x1, x2) ->
      Ast_504.Parsetree.Pcl_let
        (copy_rec_flag x0, List.map copy_value_binding x1, copy_class_expr x2)
  | Ast_505.Parsetree.Pcl_constraint (x0, x1) ->
      Ast_504.Parsetree.Pcl_constraint (copy_class_expr x0, copy_class_type x1)
  | Ast_505.Parsetree.Pcl_extension x0 ->
      Ast_504.Parsetree.Pcl_extension (copy_extension x0)
  | Ast_505.Parsetree.Pcl_open (x0, x1) ->
      Ast_504.Parsetree.Pcl_open (copy_open_description x0, copy_class_expr x1)

and copy_class_structure :
    Ast_505.Parsetree.class_structure -> Ast_504.Parsetree.class_structure =
 fun { Ast_505.Parsetree.pcstr_self; Ast_505.Parsetree.pcstr_fields } ->
  {
    Ast_504.Parsetree.pcstr_self = copy_pattern pcstr_self;
    Ast_504.Parsetree.pcstr_fields = List.map copy_class_field pcstr_fields;
  }

and copy_class_field :
    Ast_505.Parsetree.class_field -> Ast_504.Parsetree.class_field =
 fun {
       Ast_505.Parsetree.pcf_desc;
       Ast_505.Parsetree.pcf_loc;
       Ast_505.Parsetree.pcf_attributes;
     } ->
  {
    Ast_504.Parsetree.pcf_desc = copy_class_field_desc pcf_desc;
    Ast_504.Parsetree.pcf_loc = copy_location pcf_loc;
    Ast_504.Parsetree.pcf_attributes = copy_attributes pcf_attributes;
  }

and copy_class_field_desc :
    Ast_505.Parsetree.class_field_desc -> Ast_504.Parsetree.class_field_desc =
  function
  | Ast_505.Parsetree.Pcf_inherit (x0, x1, x2) ->
      Ast_504.Parsetree.Pcf_inherit
        ( copy_override_flag x0,
          copy_class_expr x1,
          Option.map (copy_loc (fun x -> x)) x2 )
  | Ast_505.Parsetree.Pcf_val x0 ->
      Ast_504.Parsetree.Pcf_val
        (let x0, x1, x2 = x0 in
         (copy_loc copy_label x0, copy_mutable_flag x1, copy_class_field_kind x2))
  | Ast_505.Parsetree.Pcf_method x0 ->
      Ast_504.Parsetree.Pcf_method
        (let x0, x1, x2 = x0 in
         (copy_loc copy_label x0, copy_private_flag x1, copy_class_field_kind x2))
  | Ast_505.Parsetree.Pcf_constraint x0 ->
      Ast_504.Parsetree.Pcf_constraint
        (let x0, x1 = x0 in
         (copy_core_type x0, copy_core_type x1))
  | Ast_505.Parsetree.Pcf_initializer x0 ->
      Ast_504.Parsetree.Pcf_initializer (copy_expression x0)
  | Ast_505.Parsetree.Pcf_attribute x0 ->
      Ast_504.Parsetree.Pcf_attribute (copy_attribute x0)
  | Ast_505.Parsetree.Pcf_extension x0 ->
      Ast_504.Parsetree.Pcf_extension (copy_extension x0)

and copy_class_field_kind :
    Ast_505.Parsetree.class_field_kind -> Ast_504.Parsetree.class_field_kind =
  function
  | Ast_505.Parsetree.Cfk_virtual x0 ->
      Ast_504.Parsetree.Cfk_virtual (copy_core_type x0)
  | Ast_505.Parsetree.Cfk_concrete (x0, x1) ->
      Ast_504.Parsetree.Cfk_concrete (copy_override_flag x0, copy_expression x1)

and copy_class_declaration :
    Ast_505.Parsetree.class_declaration -> Ast_504.Parsetree.class_declaration =
 fun x -> copy_class_infos copy_class_expr x

and copy_module_type :
    Ast_505.Parsetree.module_type -> Ast_504.Parsetree.module_type =
 fun {
       Ast_505.Parsetree.pmty_desc;
       Ast_505.Parsetree.pmty_loc;
       Ast_505.Parsetree.pmty_attributes;
     } ->
  let loc = copy_location pmty_loc in
  {
    Ast_504.Parsetree.pmty_desc = copy_module_type_desc_with_loc ~loc pmty_desc;
    Ast_504.Parsetree.pmty_loc = loc;
    Ast_504.Parsetree.pmty_attributes = copy_attributes pmty_attributes;
  }

and copy_module_type_desc_with_loc ~loc :
    Ast_505.Parsetree.module_type_desc -> Ast_504.Parsetree.module_type_desc =
  function
  | Ast_505.Parsetree.Pmty_ident x0 ->
      Ast_504.Parsetree.Pmty_ident (copy_loc copy_longident x0)
  | Ast_505.Parsetree.Pmty_signature x0 ->
      Ast_504.Parsetree.Pmty_signature (copy_signature x0)
  | Ast_505.Parsetree.Pmty_functor (x0, x1) ->
      Ast_504.Parsetree.Pmty_functor
        (copy_functor_parameter x0, copy_module_type x1)
  | Ast_505.Parsetree.Pmty_with (x0, x1) ->
      let mty = copy_module_type x0 in
      let constraints = External_type.list_map ~f:copy_with_constraint_ x1 in
      if External_type.contains_external_type constraints then
        Encoding_505.To_504.encode_external_pmty_with ~loc mty constraints.value
      else Ast_504.Parsetree.Pmty_with (mty, constraints.value)
  | Ast_505.Parsetree.Pmty_typeof x0 ->
      Ast_504.Parsetree.Pmty_typeof (copy_module_expr x0)
  | Ast_505.Parsetree.Pmty_extension x0 ->
      Ast_504.Parsetree.Pmty_extension (copy_extension x0)
  | Ast_505.Parsetree.Pmty_alias x0 ->
      Ast_504.Parsetree.Pmty_alias (copy_loc copy_longident x0)

and copy_module_type_desc mty_desc =
  copy_module_type_desc_with_loc ~loc:Location.none mty_desc

and copy_functor_parameter :
    Ast_505.Parsetree.functor_parameter -> Ast_504.Parsetree.functor_parameter =
  function
  | Ast_505.Parsetree.Unit -> Ast_504.Parsetree.Unit
  | Ast_505.Parsetree.Named (x0, x1) ->
      Ast_504.Parsetree.Named
        (copy_loc (Option.map (fun x -> x)) x0, copy_module_type x1)

and copy_signature : Ast_505.Parsetree.signature -> Ast_504.Parsetree.signature
    =
 fun x -> List.map copy_signature_item x

and copy_signature_item :
    Ast_505.Parsetree.signature_item -> Ast_504.Parsetree.signature_item =
 fun { Ast_505.Parsetree.psig_desc; Ast_505.Parsetree.psig_loc } ->
  let loc = copy_location psig_loc in
  {
    Ast_504.Parsetree.psig_desc =
      copy_signature_item_desc_with_loc ~loc psig_desc;
    Ast_504.Parsetree.psig_loc = loc;
  }

and copy_signature_item_desc_with_loc ~loc :
    Ast_505.Parsetree.signature_item_desc ->
    Ast_504.Parsetree.signature_item_desc = function
  | Ast_505.Parsetree.Psig_value x0 ->
      Ast_504.Parsetree.Psig_value (copy_value_description x0)
  | Ast_505.Parsetree.Psig_type (x0, x1) ->
      let rec_flag = copy_rec_flag x0 in
      let tds = copy_type_declaration_list x1 in
      if External_type.contains_external_type tds then
        Encoding_505.To_504.encode_external_psig_type ~loc rec_flag tds.value
      else Ast_504.Parsetree.Psig_type (rec_flag, tds.value)
  | Ast_505.Parsetree.Psig_typesubst x0 ->
      let tds = copy_type_declaration_list x0 in
      if External_type.contains_external_type tds then
        Encoding_505.To_504.encode_external_psig_typesubst ~loc tds.value
      else Ast_504.Parsetree.Psig_typesubst tds.value
  | Ast_505.Parsetree.Psig_typext x0 ->
      Ast_504.Parsetree.Psig_typext (copy_type_extension x0)
  | Ast_505.Parsetree.Psig_exception x0 ->
      Ast_504.Parsetree.Psig_exception (copy_type_exception x0)
  | Ast_505.Parsetree.Psig_module x0 ->
      Ast_504.Parsetree.Psig_module (copy_module_declaration x0)
  | Ast_505.Parsetree.Psig_modsubst x0 ->
      Ast_504.Parsetree.Psig_modsubst (copy_module_substitution x0)
  | Ast_505.Parsetree.Psig_recmodule x0 ->
      Ast_504.Parsetree.Psig_recmodule (List.map copy_module_declaration x0)
  | Ast_505.Parsetree.Psig_modtype x0 ->
      Ast_504.Parsetree.Psig_modtype (copy_module_type_declaration x0)
  | Ast_505.Parsetree.Psig_modtypesubst x0 ->
      Ast_504.Parsetree.Psig_modtypesubst (copy_module_type_declaration x0)
  | Ast_505.Parsetree.Psig_open x0 ->
      Ast_504.Parsetree.Psig_open (copy_open_description x0)
  | Ast_505.Parsetree.Psig_include x0 ->
      Ast_504.Parsetree.Psig_include (copy_include_description x0)
  | Ast_505.Parsetree.Psig_class x0 ->
      Ast_504.Parsetree.Psig_class (List.map copy_class_description x0)
  | Ast_505.Parsetree.Psig_class_type x0 ->
      Ast_504.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | Ast_505.Parsetree.Psig_attribute x0 ->
      Ast_504.Parsetree.Psig_attribute (copy_attribute x0)
  | Ast_505.Parsetree.Psig_extension (x0, x1) ->
      Ast_504.Parsetree.Psig_extension (copy_extension x0, copy_attributes x1)

and copy_signature_item_desc sigi_desc =
  copy_signature_item_desc_with_loc ~loc:Location.none sigi_desc

and copy_module_declaration :
    Ast_505.Parsetree.module_declaration -> Ast_504.Parsetree.module_declaration
    =
 fun {
       Ast_505.Parsetree.pmd_name;
       Ast_505.Parsetree.pmd_type;
       Ast_505.Parsetree.pmd_attributes;
       Ast_505.Parsetree.pmd_loc;
     } ->
  {
    Ast_504.Parsetree.pmd_name = copy_loc (Option.map (fun x -> x)) pmd_name;
    Ast_504.Parsetree.pmd_type = copy_module_type pmd_type;
    Ast_504.Parsetree.pmd_attributes = copy_attributes pmd_attributes;
    Ast_504.Parsetree.pmd_loc = copy_location pmd_loc;
  }

and copy_module_substitution :
    Ast_505.Parsetree.module_substitution ->
    Ast_504.Parsetree.module_substitution =
 fun {
       Ast_505.Parsetree.pms_name;
       Ast_505.Parsetree.pms_manifest;
       Ast_505.Parsetree.pms_attributes;
       Ast_505.Parsetree.pms_loc;
     } ->
  {
    Ast_504.Parsetree.pms_name = copy_loc (fun x -> x) pms_name;
    Ast_504.Parsetree.pms_manifest = copy_loc copy_longident pms_manifest;
    Ast_504.Parsetree.pms_attributes = copy_attributes pms_attributes;
    Ast_504.Parsetree.pms_loc = copy_location pms_loc;
  }

and copy_module_type_declaration :
    Ast_505.Parsetree.module_type_declaration ->
    Ast_504.Parsetree.module_type_declaration =
 fun {
       Ast_505.Parsetree.pmtd_name;
       Ast_505.Parsetree.pmtd_type;
       Ast_505.Parsetree.pmtd_attributes;
       Ast_505.Parsetree.pmtd_loc;
     } ->
  {
    Ast_504.Parsetree.pmtd_name = copy_loc (fun x -> x) pmtd_name;
    Ast_504.Parsetree.pmtd_type = Option.map copy_module_type pmtd_type;
    Ast_504.Parsetree.pmtd_attributes = copy_attributes pmtd_attributes;
    Ast_504.Parsetree.pmtd_loc = copy_location pmtd_loc;
  }

and copy_open_infos :
    'f0 'g0.
    ('f0 -> 'g0) ->
    'f0 Ast_505.Parsetree.open_infos ->
    'g0 Ast_504.Parsetree.open_infos =
 fun f0
     {
       Ast_505.Parsetree.popen_expr;
       Ast_505.Parsetree.popen_override;
       Ast_505.Parsetree.popen_loc;
       Ast_505.Parsetree.popen_attributes;
     } ->
  {
    Ast_504.Parsetree.popen_expr = f0 popen_expr;
    Ast_504.Parsetree.popen_override = copy_override_flag popen_override;
    Ast_504.Parsetree.popen_loc = copy_location popen_loc;
    Ast_504.Parsetree.popen_attributes = copy_attributes popen_attributes;
  }

and copy_open_description :
    Ast_505.Parsetree.open_description -> Ast_504.Parsetree.open_description =
 fun x -> copy_open_infos (copy_loc copy_longident) x

and copy_open_declaration :
    Ast_505.Parsetree.open_declaration -> Ast_504.Parsetree.open_declaration =
 fun x -> copy_open_infos copy_module_expr x

and copy_include_infos :
    'f0 'g0.
    ('f0 -> 'g0) ->
    'f0 Ast_505.Parsetree.include_infos ->
    'g0 Ast_504.Parsetree.include_infos =
 fun f0
     {
       Ast_505.Parsetree.pincl_mod;
       Ast_505.Parsetree.pincl_loc;
       Ast_505.Parsetree.pincl_attributes;
     } ->
  {
    Ast_504.Parsetree.pincl_mod = f0 pincl_mod;
    Ast_504.Parsetree.pincl_loc = copy_location pincl_loc;
    Ast_504.Parsetree.pincl_attributes = copy_attributes pincl_attributes;
  }

and copy_include_description :
    Ast_505.Parsetree.include_description ->
    Ast_504.Parsetree.include_description =
 fun x -> copy_include_infos copy_module_type x

and copy_include_declaration :
    Ast_505.Parsetree.include_declaration ->
    Ast_504.Parsetree.include_declaration =
 fun x -> copy_include_infos copy_module_expr x

and copy_with_constraint_ :
    Ast_505.Parsetree.with_constraint ->
    Ast_504.Parsetree.with_constraint External_type.may_contain_external_type =
  function
  | Ast_505.Parsetree.Pwith_type (x0, x1) ->
      let lident_loc = copy_loc copy_longident x0 in
      let td = copy_type_declaration_ x1 in
      External_type.map
        ~f:(fun td -> Ast_504.Parsetree.Pwith_type (lident_loc, td))
        td
  | Ast_505.Parsetree.Pwith_module (x0, x1) ->
      External_type.wrap ~contains_external_type:false
        (Ast_504.Parsetree.Pwith_module
           (copy_loc copy_longident x0, copy_loc copy_longident x1))
  | Ast_505.Parsetree.Pwith_modtype (x0, x1) ->
      External_type.wrap ~contains_external_type:false
        (Ast_504.Parsetree.Pwith_modtype
           (copy_loc copy_longident x0, copy_module_type x1))
  | Ast_505.Parsetree.Pwith_modtypesubst (x0, x1) ->
      External_type.wrap ~contains_external_type:false
        (Ast_504.Parsetree.Pwith_modtypesubst
           (copy_loc copy_longident x0, copy_module_type x1))
  | Ast_505.Parsetree.Pwith_typesubst (x0, x1) ->
      let lident_loc = copy_loc copy_longident x0 in
      let td = copy_type_declaration_ x1 in
      External_type.map
        ~f:(fun td -> Ast_504.Parsetree.Pwith_typesubst (lident_loc, td))
        td
  | Ast_505.Parsetree.Pwith_modsubst (x0, x1) ->
      External_type.wrap ~contains_external_type:false
        (Ast_504.Parsetree.Pwith_modsubst
           (copy_loc copy_longident x0, copy_loc copy_longident x1))

and copy_module_expr :
    Ast_505.Parsetree.module_expr -> Ast_504.Parsetree.module_expr =
 fun {
       Ast_505.Parsetree.pmod_desc;
       Ast_505.Parsetree.pmod_loc;
       Ast_505.Parsetree.pmod_attributes;
     } ->
  {
    Ast_504.Parsetree.pmod_desc = copy_module_expr_desc pmod_desc;
    Ast_504.Parsetree.pmod_loc = copy_location pmod_loc;
    Ast_504.Parsetree.pmod_attributes = copy_attributes pmod_attributes;
  }

and copy_module_expr_desc :
    Ast_505.Parsetree.module_expr_desc -> Ast_504.Parsetree.module_expr_desc =
  function
  | Ast_505.Parsetree.Pmod_ident x0 ->
      Ast_504.Parsetree.Pmod_ident (copy_loc copy_longident x0)
  | Ast_505.Parsetree.Pmod_structure x0 ->
      Ast_504.Parsetree.Pmod_structure (copy_structure x0)
  | Ast_505.Parsetree.Pmod_functor (x0, x1) ->
      Ast_504.Parsetree.Pmod_functor
        (copy_functor_parameter x0, copy_module_expr x1)
  | Ast_505.Parsetree.Pmod_apply (x0, x1) ->
      Ast_504.Parsetree.Pmod_apply (copy_module_expr x0, copy_module_expr x1)
  | Ast_505.Parsetree.Pmod_apply_unit x0 ->
      Ast_504.Parsetree.Pmod_apply_unit (copy_module_expr x0)
  | Ast_505.Parsetree.Pmod_constraint (x0, x1) ->
      Ast_504.Parsetree.Pmod_constraint
        (copy_module_expr x0, copy_module_type x1)
  | Ast_505.Parsetree.Pmod_unpack x0 ->
      Ast_504.Parsetree.Pmod_unpack (copy_expression x0)
  | Ast_505.Parsetree.Pmod_extension x0 ->
      Ast_504.Parsetree.Pmod_extension (copy_extension x0)

and copy_structure : Ast_505.Parsetree.structure -> Ast_504.Parsetree.structure
    =
 fun x -> List.map copy_structure_item x

and copy_structure_item :
    Ast_505.Parsetree.structure_item -> Ast_504.Parsetree.structure_item =
 fun { Ast_505.Parsetree.pstr_desc; Ast_505.Parsetree.pstr_loc } ->
  let loc = copy_location pstr_loc in
  {
    Ast_504.Parsetree.pstr_desc =
      copy_structure_item_desc_with_loc ~loc pstr_desc;
    Ast_504.Parsetree.pstr_loc = loc;
  }

and copy_structure_item_desc_with_loc ~loc :
    Ast_505.Parsetree.structure_item_desc ->
    Ast_504.Parsetree.structure_item_desc = function
  | Ast_505.Parsetree.Pstr_eval (x0, x1) ->
      Ast_504.Parsetree.Pstr_eval (copy_expression x0, copy_attributes x1)
  | Ast_505.Parsetree.Pstr_value (x0, x1) ->
      Ast_504.Parsetree.Pstr_value
        (copy_rec_flag x0, List.map copy_value_binding x1)
  | Ast_505.Parsetree.Pstr_primitive x0 ->
      Ast_504.Parsetree.Pstr_primitive (copy_value_description x0)
  | Ast_505.Parsetree.Pstr_type (x0, x1) ->
      let rec_flag = copy_rec_flag x0 in
      let tds = copy_type_declaration_list x1 in
      if External_type.contains_external_type tds then
        Encoding_505.To_504.encode_external_pstr_type ~loc rec_flag
          (External_type.value tds)
      else Ast_504.Parsetree.Pstr_type (rec_flag, External_type.value tds)
  | Ast_505.Parsetree.Pstr_typext x0 ->
      Ast_504.Parsetree.Pstr_typext (copy_type_extension x0)
  | Ast_505.Parsetree.Pstr_exception x0 ->
      Ast_504.Parsetree.Pstr_exception (copy_type_exception x0)
  | Ast_505.Parsetree.Pstr_module x0 ->
      Ast_504.Parsetree.Pstr_module (copy_module_binding x0)
  | Ast_505.Parsetree.Pstr_recmodule x0 ->
      Ast_504.Parsetree.Pstr_recmodule (List.map copy_module_binding x0)
  | Ast_505.Parsetree.Pstr_modtype x0 ->
      Ast_504.Parsetree.Pstr_modtype (copy_module_type_declaration x0)
  | Ast_505.Parsetree.Pstr_open x0 ->
      Ast_504.Parsetree.Pstr_open (copy_open_declaration x0)
  | Ast_505.Parsetree.Pstr_class x0 ->
      Ast_504.Parsetree.Pstr_class (List.map copy_class_declaration x0)
  | Ast_505.Parsetree.Pstr_class_type x0 ->
      Ast_504.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | Ast_505.Parsetree.Pstr_include x0 ->
      Ast_504.Parsetree.Pstr_include (copy_include_declaration x0)
  | Ast_505.Parsetree.Pstr_attribute x0 ->
      Ast_504.Parsetree.Pstr_attribute (copy_attribute x0)
  | Ast_505.Parsetree.Pstr_extension (x0, x1) ->
      Ast_504.Parsetree.Pstr_extension (copy_extension x0, copy_attributes x1)

and copy_structure_item_desc stri_desc =
  copy_structure_item_desc_with_loc ~loc:Location.none stri_desc

and copy_value_constraint :
    Ast_505.Parsetree.value_constraint -> Ast_504.Parsetree.value_constraint =
  function
  | Ast_505.Parsetree.Pvc_constraint { locally_abstract_univars; typ } ->
      Ast_504.Parsetree.Pvc_constraint
        {
          locally_abstract_univars =
            List.map (copy_loc (fun x -> x)) locally_abstract_univars;
          typ = copy_core_type typ;
        }
  | Ast_505.Parsetree.Pvc_coercion { ground; coercion } ->
      Ast_504.Parsetree.Pvc_coercion
        {
          ground = Option.map copy_core_type ground;
          coercion = copy_core_type coercion;
        }

and copy_value_binding :
    Ast_505.Parsetree.value_binding -> Ast_504.Parsetree.value_binding =
 fun {
       Ast_505.Parsetree.pvb_pat;
       Ast_505.Parsetree.pvb_expr;
       Ast_505.Parsetree.pvb_constraint;
       Ast_505.Parsetree.pvb_attributes;
       Ast_505.Parsetree.pvb_loc;
     } ->
  {
    Ast_504.Parsetree.pvb_pat = copy_pattern pvb_pat;
    Ast_504.Parsetree.pvb_expr = copy_expression pvb_expr;
    Ast_504.Parsetree.pvb_constraint =
      Option.map copy_value_constraint pvb_constraint;
    Ast_504.Parsetree.pvb_attributes = copy_attributes pvb_attributes;
    Ast_504.Parsetree.pvb_loc = copy_location pvb_loc;
  }

and copy_module_binding :
    Ast_505.Parsetree.module_binding -> Ast_504.Parsetree.module_binding =
 fun {
       Ast_505.Parsetree.pmb_name;
       Ast_505.Parsetree.pmb_expr;
       Ast_505.Parsetree.pmb_attributes;
       Ast_505.Parsetree.pmb_loc;
     } ->
  {
    Ast_504.Parsetree.pmb_name = copy_loc (Option.map (fun x -> x)) pmb_name;
    Ast_504.Parsetree.pmb_expr = copy_module_expr pmb_expr;
    Ast_504.Parsetree.pmb_attributes = copy_attributes pmb_attributes;
    Ast_504.Parsetree.pmb_loc = copy_location pmb_loc;
  }

and copy_toplevel_phrase :
    Ast_505.Parsetree.toplevel_phrase -> Ast_504.Parsetree.toplevel_phrase =
  function
  | Ast_505.Parsetree.Ptop_def x0 ->
      Ast_504.Parsetree.Ptop_def (copy_structure x0)
  | Ast_505.Parsetree.Ptop_dir x0 ->
      Ast_504.Parsetree.Ptop_dir (copy_toplevel_directive x0)

and copy_toplevel_directive :
    Ast_505.Parsetree.toplevel_directive -> Ast_504.Parsetree.toplevel_directive
    =
 fun {
       Ast_505.Parsetree.pdir_name;
       Ast_505.Parsetree.pdir_arg;
       Ast_505.Parsetree.pdir_loc;
     } ->
  {
    Ast_504.Parsetree.pdir_name = copy_loc (fun x -> x) pdir_name;
    Ast_504.Parsetree.pdir_arg = Option.map copy_directive_argument pdir_arg;
    Ast_504.Parsetree.pdir_loc = copy_location pdir_loc;
  }

and copy_directive_argument :
    Ast_505.Parsetree.directive_argument -> Ast_504.Parsetree.directive_argument
    =
 fun { Ast_505.Parsetree.pdira_desc; Ast_505.Parsetree.pdira_loc } ->
  {
    Ast_504.Parsetree.pdira_desc = copy_directive_argument_desc pdira_desc;
    Ast_504.Parsetree.pdira_loc = copy_location pdira_loc;
  }

and copy_directive_argument_desc :
    Ast_505.Parsetree.directive_argument_desc ->
    Ast_504.Parsetree.directive_argument_desc = function
  | Ast_505.Parsetree.Pdir_string x0 -> Ast_504.Parsetree.Pdir_string x0
  | Ast_505.Parsetree.Pdir_int (x0, x1) ->
      Ast_504.Parsetree.Pdir_int (x0, Option.map (fun x -> x) x1)
  | Ast_505.Parsetree.Pdir_ident x0 ->
      Ast_504.Parsetree.Pdir_ident (copy_longident x0)
  | Ast_505.Parsetree.Pdir_bool x0 -> Ast_504.Parsetree.Pdir_bool x0

(** The functions below are provided to keep a coherent and stable API with
    other migrate_X_Y modules while allowing us to locally define variations of
    those functions used to encode 5.5 features into the 5.4 AST. *)

let copy_type_declaration td =
  let td' = copy_type_declaration_ td in
  if External_type.contains_external_type td' then
    External_type.err ~loc:td.ptype_loc
  else External_type.value td'
