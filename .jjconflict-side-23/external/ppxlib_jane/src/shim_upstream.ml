open Astlib
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree

module Longident = struct
  type t = Ast_504.Longident.t =
    | Lident of string
    | Ldot of t loc * string loc
    | Lapply of t loc * t loc

  let loc txt = { txt; loc = Location.none }

  let rec to_parsetree : t -> Astlib.Longident.t = function
    | Lident s -> Lident s
    | Ldot (lid, s) -> Ldot (to_parsetree lid.txt, s.txt)
    | Lapply (lid, lid2) -> Lapply (to_parsetree lid.txt, to_parsetree lid2.txt)
  ;;

  let rec of_parsetree : Astlib.Longident.t -> t = function
    | Lident s -> Lident s
    | Ldot (lid, s) -> Ldot (loc (of_parsetree lid), loc s)
    | Lapply (lid, lid2) -> Lapply (loc (of_parsetree lid), loc (of_parsetree lid2))
  ;;

  let flatten t = Astlib.Longident.flatten (to_parsetree t)
  let parse s = Astlib.Longident.parse s |> of_parsetree
end

module Modality = struct
  type nonrec t = Modality of string [@@unboxed]
end

module Modalities = struct
  type t = Modality.t loc list

  let none = []
  let portable ~loc = [ { txt = Modality.Modality "portable"; loc } ]
end

module Mode = struct
  type t = Mode of string [@@unboxed]
end

module Modes = struct
  type t = Mode.t loc list

  let none = []
  let local ~loc = [ { txt = Mode.Mode "local"; loc } ]
end

module Include_kind = struct
  type t =
    | Structure
    | Functor
end

type arrow_argument =
  { arg_label : arg_label
  ; arg_modes : Modes.t
  ; arg_type : core_type
  }

type arrow_result =
  { result_modes : Modes.t
  ; result_type : core_type
  }

module Pcstr_tuple_arg = struct
  type t = core_type

  let extract_modalities t = [], t
  let to_core_type t = t
  let of_core_type core_type = core_type
  let map_core_type t ~f = f t
  let map_core_type_extra t ~f = f t
  let create ~loc:_ ~modalities:_ ~type_ = type_
end

module Label_declaration = struct
  let extract_modalities ld = [], ld

  let create ~loc ~name ~mutable_ ~modalities:_ ~type_ =
    { pld_loc = loc
    ; pld_name = name
    ; pld_type = type_
    ; pld_mutable = mutable_
    ; pld_attributes = []
    }
  ;;
end

module Value_description = struct
  let extract_modalities vd = [], vd

  let create ~loc ~name ~type_ ~modalities:_ ~prim =
    { pval_loc = loc
    ; pval_name = name
    ; pval_type = type_
    ; pval_prim = prim
    ; pval_attributes = []
    }
  ;;
end

module Module_declaration = struct
  type t =
    { pmd_name : string option loc
    ; pmd_type : module_type
    ; pmd_modalities : Modalities.t
    ; pmd_attributes : attributes (** [... [\@\@id1] [\@\@id2]] *)
    ; pmd_loc : Location.t
    }

  let to_parsetree
    ({ pmd_name; pmd_type; pmd_attributes; pmd_loc; pmd_modalities = _ } : t)
    : module_declaration
    =
    { pmd_name; pmd_type; pmd_attributes; pmd_loc }
  ;;

  let of_parsetree ({ pmd_name; pmd_type; pmd_attributes; pmd_loc } : module_declaration)
    : t
    =
    let pmd_modalities = [] in
    { pmd_name; pmd_type; pmd_attributes; pmd_loc; pmd_modalities }
  ;;
end

module Value_binding = struct
  let extract_modes vb = [], vb

  let create ~loc ~pat ~expr ~modes:_ =
    { pvb_pat = pat
    ; pvb_expr = expr
    ; pvb_constraint = None
    ; pvb_attributes = []
    ; pvb_loc = loc
    }
  ;;
end

module T = struct
  type jkind_annotation_desc =
    | Pjk_default
    | Pjk_abbreviation of Astlib.Longident.t loc
    | Pjk_operator of jkind_annotation * string loc list
    | Pjk_mod of jkind_annotation * Modes.t
    | Pjk_with of jkind_annotation * core_type * Modalities.t
    | Pjk_kind_of of core_type
    | Pjk_product of jkind_annotation list

  and jkind_annotation =
    { pjka_loc : Location.t
    ; pjka_desc : jkind_annotation_desc
    }

  and jkind_declaration =
    { pjkind_name : string loc
    ; pjkind_manifest : jkind_annotation option
    ; pjkind_attributes : attributes
    ; pjkind_loc : Location.t
    }
end

include T

module Type_declaration = struct
  type t =
    { ptype_name : string loc
    ; ptype_params : (core_type * (variance * injectivity)) list
    ; ptype_cstrs : (core_type * core_type * Location.t) list
    ; ptype_kind : type_kind
    ; ptype_private : private_flag
    ; ptype_manifest : core_type option
    ; ptype_attributes : attributes
    ; ptype_jkind_annotation : jkind_annotation option
    ; ptype_loc : Location.t
    }

  let to_parsetree : t -> type_declaration =
    fun { ptype_name
        ; ptype_params
        ; ptype_cstrs
        ; ptype_kind
        ; ptype_private
        ; ptype_manifest
        ; ptype_attributes
        ; ptype_jkind_annotation = _
        ; ptype_loc
        } ->
    { ptype_name
    ; ptype_params
    ; ptype_cstrs
    ; ptype_kind
    ; ptype_private
    ; ptype_manifest
    ; ptype_attributes
    ; ptype_loc
    }
  ;;

  let of_parsetree : type_declaration -> t =
    fun { ptype_name
        ; ptype_params
        ; ptype_cstrs
        ; ptype_kind
        ; ptype_private
        ; ptype_manifest
        ; ptype_attributes
        ; ptype_loc
        } ->
    { ptype_name
    ; ptype_params
    ; ptype_cstrs
    ; ptype_kind
    ; ptype_private
    ; ptype_manifest
    ; ptype_attributes
    ; ptype_jkind_annotation = None
    ; ptype_loc
    }
  ;;

  let extract_jkind_annotation _ = None
end

module Constant = struct
  type t =
    | Pconst_integer of string * char option
    | Pconst_unboxed_integer of string * char
    | Pconst_char of char
    | Pconst_untagged_char of char
    | Pconst_string of string * Location.t * string option
    | Pconst_float of string * char option
    | Pconst_unboxed_float of string * char option

  let of_parsetree : constant -> t = function
    | Pconst_integer (a, b) -> Pconst_integer (a, b)
    | Pconst_char a -> Pconst_char a
    | Pconst_string (a, b, c) -> Pconst_string (a, b, c)
    | Pconst_float (a, b) -> Pconst_float (a, b)
  ;;

  let to_parsetree : t -> constant = function
    | Pconst_integer (a, b) -> Pconst_integer (a, b)
    | Pconst_char a -> Pconst_char a
    | Pconst_string (a, b, c) -> Pconst_string (a, b, c)
    | Pconst_float (a, b) -> Pconst_float (a, b)
    (* Unboxed literal constants erase to boxed literals. *)
    | Pconst_unboxed_integer (a, b) -> Pconst_integer (a, Some b)
    | Pconst_unboxed_float (a, b) -> Pconst_float (a, b)
    | Pconst_untagged_char a -> Pconst_char a
  ;;
end

module Pexp_function = struct
  type function_param_desc =
    | Pparam_val of arg_label * expression option * pattern
    | Pparam_newtype of string loc * jkind_annotation option

  type function_param =
    { pparam_loc : Location.t
    ; pparam_desc : function_param_desc
    }

  type nonrec type_constraint = type_constraint =
    | Pconstraint of core_type
    | Pcoerce of core_type option * core_type

  module Function_constraint = struct
    type t =
      { mode_annotations : Modes.t
      ; ret_mode_annotations : Modes.t
      ; ret_type_constraint : type_constraint option
      }

    let none =
      { mode_annotations = []; ret_mode_annotations = []; ret_type_constraint = None }
    ;;

    let is_none { ret_mode_annotations; ret_type_constraint; _ } =
      match ret_mode_annotations, ret_type_constraint with
      | [], None -> true
      | _, _ -> false
    ;;
  end

  type nonrec function_body = function_body =
    | Pfunction_body of expression
    | Pfunction_cases of case list * Location.t * attributes

  let function_param_to_parsetree ({ pparam_loc; pparam_desc } : function_param)
    : Ppxlib_ast.Parsetree.function_param
    =
    { pparam_loc
    ; pparam_desc =
        (match pparam_desc with
         | Pparam_val (lbl, eo, pat) -> Pparam_val (lbl, eo, pat)
         | Pparam_newtype (newtype, _jkind) -> Pparam_newtype newtype)
    }
  ;;

  let function_param_of_parsetree
    ({ pparam_loc; pparam_desc } : Ppxlib_ast.Parsetree.function_param)
    : function_param
    =
    { pparam_loc
    ; pparam_desc =
        (match pparam_desc with
         | Pparam_val (lbl, eo, pat) -> Pparam_val (lbl, eo, pat)
         | Pparam_newtype newtype -> Pparam_newtype (newtype, None))
    }
  ;;

  let to_parsetree
    ~params
    ~constraint_:
      ({ mode_annotations = _; ret_mode_annotations = _; ret_type_constraint } :
        Function_constraint.t)
    ~body
    =
    Pexp_function (List.map function_param_to_parsetree params, ret_type_constraint, body)
  ;;

  let rec of_parsetree expr_desc ~loc =
    match expr_desc with
    | Pexp_function (params, ret_type_constraint, body) ->
      let constraint_ : Function_constraint.t =
        { Function_constraint.none with ret_type_constraint }
      in
      Some (List.map function_param_of_parsetree params, constraint_, body)
    | Pexp_newtype (newtype, body) ->
      (match of_parsetree body.pexp_desc ~loc with
       | Some (params, constraint_, function_body) ->
         let function_param =
           { pparam_loc = loc; pparam_desc = Pparam_newtype (newtype, None) }
         in
         Some (function_param :: params, constraint_, function_body)
       | None -> None)
    | _ -> None
  ;;
end

let ptyp_any =
  { ptyp_desc = Ptyp_any
  ; ptyp_loc = Location.none
  ; ptyp_loc_stack = []
  ; ptyp_attributes = []
  }
;;

let add_none_labels l = List.map (fun x -> None, x) l

(* Duplicated from [common.ml] to avoid dependency cycle *)
let as_unlabeled_tuple components =
  if List.for_all (fun (label, _) -> Option.is_none label) components
  then Some (List.map snd components)
  else None
;;

let as_unlabeled_tuple_unconditionally components = List.map snd components

type nonrec access_flag =
  | Immutable_access
  | Mutable_access
  | Atomic_access

type block_access =
  | Baccess_field of Astlib.Longident.t loc
  | Baccess_block of access_flag * expression

type unboxed_access = Uaccess_unboxed_field of Astlib.Longident.t loc

module Core_type_desc = struct
  type t =
    | Ptyp_any of jkind_annotation option
    | Ptyp_var of string * jkind_annotation option
    | Ptyp_arrow of arg_label * core_type * core_type * Modes.t * Modes.t
    | Ptyp_tuple of (string option * core_type) list
    | Ptyp_unboxed_tuple of (string option * core_type) list
    | Ptyp_constr of Astlib.Longident.t loc * core_type list
    | Ptyp_object of object_field list * closed_flag
    | Ptyp_class of Astlib.Longident.t loc * core_type list
    | Ptyp_alias of core_type * string loc option * jkind_annotation option
    | Ptyp_variant of row_field list * closed_flag * label list option
    | Ptyp_poly of (string loc * jkind_annotation option) list * core_type
    | Ptyp_newlayout of string loc list * core_type
    | Ptyp_package of package_type
    | Ptyp_quote of core_type
    | Ptyp_splice of core_type
    | Ptyp_of_kind of jkind_annotation
    | Ptyp_repr of string loc list * core_type
    | Ptyp_extension of extension

  let of_parsetree : core_type_desc -> t = function
    (* changed constructors *)
    | Ptyp_arrow (a, b, c) -> Ptyp_arrow (a, b, c, [], [])
    | Ptyp_tuple a -> Ptyp_tuple (add_none_labels a)
    | Ptyp_any -> Ptyp_any None
    | Ptyp_var s -> Ptyp_var (s, None)
    | Ptyp_alias (a, b) -> Ptyp_alias (a, Some b, None)
    | Ptyp_poly (a, b) -> Ptyp_poly (List.map (fun x -> x, None) a, b)
    (* constructors not representable in the Jane Street AST *)
    | Ptyp_open (_, _) -> failwith "[Ptyp_open] is not supported by ppxlib_jane"
    (* unchanged constructors *)
    | Ptyp_constr (a, b) -> Ptyp_constr (a, b)
    | Ptyp_object (a, b) -> Ptyp_object (a, b)
    | Ptyp_class (a, b) -> Ptyp_class (a, b)
    | Ptyp_variant (a, b, c) -> Ptyp_variant (a, b, c)
    | Ptyp_package a -> Ptyp_package a
    | Ptyp_extension a -> Ptyp_extension a
  ;;

  let fresh_name =
    let r = ref 0 in
    fun name ->
      let i = !r in
      incr r;
      name ^ string_of_int i
  ;;

  let to_parsetree : t -> core_type_desc = function
    (* changed constructors *)
    | Ptyp_arrow (a, b, c, _, _) -> Ptyp_arrow (a, b, c)
    | Ptyp_any (_ : jkind_annotation option) -> Ptyp_any
    | Ptyp_var (s, _) -> Ptyp_var s
    | Ptyp_poly (a, b) -> Ptyp_poly (List.map fst a, b)
    | Ptyp_alias (a, Some b, _) -> Ptyp_alias (a, b)
    | Ptyp_alias (a, None, _) ->
      let ghost_alias_loc = { a.ptyp_loc with loc_ghost = true } in
      Ptyp_alias (a, { txt = fresh_name "_alias"; loc = ghost_alias_loc })
    | Ptyp_tuple labeled_typs ->
      Ptyp_tuple (as_unlabeled_tuple_unconditionally labeled_typs)
    (* new constructors *)
    | Ptyp_unboxed_tuple labeled_typs ->
      Ptyp_tuple (as_unlabeled_tuple_unconditionally labeled_typs)
    (* TODO: if [Ptyp_of_kind] is allowed outside of with-bounds, then erase it here,
       otherwise, this should be unreachable, and the error message should reflect that *)
    | Ptyp_of_kind _ -> failwith "[Ptyp_of_kind] unimplemented in ppxlib_jane"
    (* unchanged constructors *)
    | Ptyp_constr (a, b) -> Ptyp_constr (a, b)
    | Ptyp_object (a, b) -> Ptyp_object (a, b)
    | Ptyp_class (a, b) -> Ptyp_class (a, b)
    | Ptyp_variant (a, b, c) -> Ptyp_variant (a, b, c)
    | Ptyp_quote _ -> failwith "[Ptyp_quote] unimplemented in ppxlib_jane"
    | Ptyp_splice _ -> failwith "[Ptyp_splice] unimplemented in ppxlib_jane"
    | Ptyp_newlayout _ -> failwith "[Ptyp_newlayout] unimplemented in ppxlib_jane"
    | Ptyp_repr _ -> failwith "[Ptyp_repr] unimplemented in ppxlib_jane"
    | Ptyp_package a -> Ptyp_package a
    | Ptyp_extension a -> Ptyp_extension a
  ;;
end

module Core_type = struct
  type t =
    { ptyp_desc : Core_type_desc.t
    ; ptyp_loc : Location.t
    ; ptyp_loc_stack : Location.t list
    ; ptyp_attributes : attributes
    }

  let of_parsetree
    { Ppxlib_ast.Parsetree.ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }
    =
    let ptyp_desc = Core_type_desc.of_parsetree ptyp_desc in
    { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }
  ;;

  let to_parsetree { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes } =
    let ptyp_desc = Core_type_desc.to_parsetree ptyp_desc in
    { Ppxlib_ast.Parsetree.ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }
  ;;
end

module Pattern_desc = struct
  type t =
    | Ppat_any
    | Ppat_var of string loc
    | Ppat_alias of pattern * string loc
    | Ppat_constant of constant
    | Ppat_interval of constant * constant
    | Ppat_unboxed_unit
    | Ppat_unboxed_bool of bool
    | Ppat_tuple of (string option * pattern) list * closed_flag
    | Ppat_unboxed_tuple of (string option * pattern) list * closed_flag
    | Ppat_construct of
        Astlib.Longident.t loc
        * ((string loc * jkind_annotation option) list * pattern) option
    | Ppat_variant of label * pattern option
    | Ppat_record of (Astlib.Longident.t loc * pattern) list * closed_flag
    | Ppat_record_unboxed_product of (Astlib.Longident.t loc * pattern) list * closed_flag
    | Ppat_array of mutable_flag * pattern list
    | Ppat_or of pattern * pattern
    | Ppat_constraint of pattern * core_type option * Modes.t
    | Ppat_type of Astlib.Longident.t loc
    | Ppat_lazy of pattern
    | Ppat_unpack of string option loc
    | Ppat_exception of pattern
    | Ppat_extension of extension
    | Ppat_open of Astlib.Longident.t loc * pattern

  let of_parsetree : pattern_desc -> t = function
    (* changed constructors *)
    | Ppat_constraint (a, b) -> Ppat_constraint (a, Some b, [])
    | Ppat_tuple a -> Ppat_tuple (add_none_labels a, Closed)
    | Ppat_construct (a, b) ->
      let b =
        Option.map (fun (vars, pattern) -> List.map (fun s -> s, None) vars, pattern) b
      in
      Ppat_construct (a, b)
    (* unchanged constructors *)
    | Ppat_any -> Ppat_any
    | Ppat_var a -> Ppat_var a
    | Ppat_alias (a, b) -> Ppat_alias (a, b)
    | Ppat_constant a -> Ppat_constant a
    | Ppat_interval (a, b) -> Ppat_interval (a, b)
    | Ppat_variant (a, b) -> Ppat_variant (a, b)
    | Ppat_record (a, b) -> Ppat_record (a, b)
    | Ppat_array a -> Ppat_array (Mutable, a)
    | Ppat_or (a, b) -> Ppat_or (a, b)
    | Ppat_type a -> Ppat_type a
    | Ppat_lazy a -> Ppat_lazy a
    | Ppat_unpack a -> Ppat_unpack a
    | Ppat_exception a -> Ppat_exception a
    | Ppat_extension a -> Ppat_extension a
    | Ppat_open (a, b) -> Ppat_open (a, b)
  ;;

  let to_parsetree : loc:Location.t -> t -> pattern_desc =
    fun ~loc -> function
    (* changed constructors *)
    | Ppat_constraint (a, Some b, _) -> Ppat_constraint (a, b)
    | Ppat_constraint (a, None, _) -> Ppat_constraint (a, ptyp_any)
    | Ppat_tuple (_, Open) ->
      Location.raise_errorf
        ~loc
        "[Ppat_tuple] with an \"open\" pattern cannot be converted to an upstream \
         [pattern_desc]"
    | Ppat_tuple (labeled_pats, Closed) ->
      (match as_unlabeled_tuple labeled_pats with
       | Some pats -> Ppat_tuple pats
       | None ->
         failwith
           "[Ppat_tuple], when labels are present, cannot be converted to an upstream \
            [pattern_desc]")
    | Ppat_array (Mutable, a) -> Ppat_array a
    | Ppat_array (Immutable, _) ->
      Location.raise_errorf
        ~loc
        "Immutable [Ppat_array] cannot be converted to an usptream [pattern_desc]"
    (* new constructors *)
    | Ppat_unboxed_unit -> Ppat_construct ({ loc; txt = Lident "()" }, None)
    | Ppat_unboxed_bool b ->
      Ppat_construct ({ loc; txt = Lident (if b then "true" else "false") }, None)
    | Ppat_unboxed_tuple (_, Open) ->
      Location.raise_errorf
        ~loc
        "[Ppat_unboxed_tuple] with an \"open\" pattern cannot be converted to an \
         upstream [pattern_desc]"
    | Ppat_unboxed_tuple (labeled_pats, Closed) ->
      (match as_unlabeled_tuple labeled_pats with
       | Some pats -> Ppat_tuple pats
       | None ->
         failwith
           "[Ppat_unboxed_tuple], when labels are present, cannot be converted to an \
            upstream [pattern_desc]")
    | Ppat_record_unboxed_product (a, b) -> Ppat_record (a, b)
    | Ppat_construct (a, b) ->
      let b =
        Option.map
          (fun (vars, p) ->
            let vars = List.map (fun (var, (_ : jkind_annotation option)) -> var) vars in
            vars, p)
          b
      in
      Ppat_construct (a, b)
    (* unchanged constructors *)
    | Ppat_any -> Ppat_any
    | Ppat_var a -> Ppat_var a
    | Ppat_alias (a, b) -> Ppat_alias (a, b)
    | Ppat_constant a -> Ppat_constant a
    | Ppat_interval (a, b) -> Ppat_interval (a, b)
    | Ppat_variant (a, b) -> Ppat_variant (a, b)
    | Ppat_record (a, b) -> Ppat_record (a, b)
    | Ppat_or (a, b) -> Ppat_or (a, b)
    | Ppat_type a -> Ppat_type a
    | Ppat_lazy a -> Ppat_lazy a
    | Ppat_unpack a -> Ppat_unpack a
    | Ppat_exception a -> Ppat_exception a
    | Ppat_extension a -> Ppat_extension a
    | Ppat_open (a, b) -> Ppat_open (a, b)
  ;;
end

module Expression_desc = struct
  type comprehension_expression = private
    | Pcomp_list_comprehension of unit
    | Pcomp_array_comprehension of unit

  type t =
    | Pexp_ident of Astlib.Longident.t loc
    | Pexp_constant of constant
    | Pexp_let of mutable_flag * rec_flag * value_binding list * expression
    | Pexp_function of
        Pexp_function.function_param list
        * Pexp_function.Function_constraint.t
        * function_body
    | Pexp_apply of expression * (arg_label * expression) list
    | Pexp_match of expression * case list
    | Pexp_try of expression * case list
    | Pexp_unboxed_unit
    | Pexp_unboxed_bool of bool
    | Pexp_tuple of (string option * expression) list
    | Pexp_unboxed_tuple of (string option * expression) list
    | Pexp_construct of Astlib.Longident.t loc * expression option
    | Pexp_variant of label * expression option
    | Pexp_record of (Astlib.Longident.t loc * expression) list * expression option
    | Pexp_record_unboxed_product of
        (Astlib.Longident.t loc * expression) list * expression option
    | Pexp_field of expression * Astlib.Longident.t loc
    | Pexp_unboxed_field of expression * Astlib.Longident.t loc
    | Pexp_setfield of expression * Astlib.Longident.t loc * expression
    | Pexp_array of mutable_flag * expression list
    | Pexp_idx of block_access * unboxed_access list
    | Pexp_ifthenelse of expression * expression * expression option
    | Pexp_sequence of expression * expression
    | Pexp_while of expression * expression
    | Pexp_for of pattern * expression * expression * direction_flag * expression
    | Pexp_constraint of expression * core_type option * Modes.t
    | Pexp_coerce of expression * core_type option * core_type
    | Pexp_send of expression * label loc
    | Pexp_new of Astlib.Longident.t loc
    | Pexp_setvar of label loc * expression
    | Pexp_override of (label loc * expression) list
    | Pexp_letmodule of string option loc * module_expr * expression
    | Pexp_letexception of extension_constructor * expression
    | Pexp_assert of expression
    | Pexp_lazy of expression
    | Pexp_poly of expression * core_type option
    | Pexp_object of class_structure
    | Pexp_newtype of string loc * jkind_annotation option * expression
    | Pexp_pack of module_expr
    | Pexp_open of open_declaration * expression
    | Pexp_letop of letop
    | Pexp_extension of extension
    | Pexp_unreachable
    | Pexp_stack of expression
    | Pexp_comprehension of comprehension_expression
    | Pexp_overwrite of expression * expression
    | Pexp_quote of expression
    | Pexp_splice of expression
    | Pexp_hole
    | Pexp_borrow of expression

  let to_parsetree : loc:Location.t -> t -> expression_desc =
    fun ~loc -> function
    (* changed constructors *)
    | Pexp_function (x1, x2, x3) ->
      Pexp_function.to_parsetree ~params:x1 ~constraint_:x2 ~body:x3
    | Pexp_constraint (x1, Some x2, _) -> Pexp_constraint (x1, x2)
    | Pexp_constraint (x1, None, _) -> Pexp_constraint (x1, ptyp_any)
    | Pexp_tuple labeled_exps ->
      Pexp_tuple (as_unlabeled_tuple_unconditionally labeled_exps)
    | Pexp_newtype (x1, _, x2) -> Pexp_newtype (x1, x2)
    | Pexp_array (Mutable, x) -> Pexp_array x
    | Pexp_array (Immutable, _) ->
      Location.raise_errorf
        ~loc
        "Immutable [Pexp_array] cannot be converted to an upstream [expression_desc]"
    | Pexp_record_unboxed_product (x1, x2) -> Pexp_record (x1, x2)
    | Pexp_unboxed_field (x1, x2) -> Pexp_field (x1, x2)
    (* new constructors *)
    | Pexp_unboxed_unit -> Pexp_construct ({ loc; txt = Lident "()" }, None)
    | Pexp_unboxed_bool b ->
      Pexp_construct ({ loc; txt = Lident (if b then "true" else "false") }, None)
    | Pexp_unboxed_tuple labeled_exps ->
      Pexp_tuple (as_unlabeled_tuple_unconditionally labeled_exps)
    | Pexp_idx _ ->
      Location.raise_errorf
        ~loc
        "[Pexp_idx] cannot be converted to an upstream [expression_desc]"
    (* unchanged constructors *)
    | Pexp_ident x -> Pexp_ident x
    | Pexp_constant x -> Pexp_constant x
    | Pexp_let (Immutable, x1, x2, x3) -> Pexp_let (x1, x2, x3)
    | Pexp_let (Mutable, _, _, _) ->
      Location.raise_errorf
        ~loc
        "Mutable [Pexp_let] cannot be converted to an upstream [expression_desc]"
    | Pexp_apply (x1, x2) -> Pexp_apply (x1, x2)
    | Pexp_match (x1, x2) -> Pexp_match (x1, x2)
    | Pexp_try (x1, x2) -> Pexp_try (x1, x2)
    | Pexp_construct (x1, x2) -> Pexp_construct (x1, x2)
    | Pexp_variant (x1, x2) -> Pexp_variant (x1, x2)
    | Pexp_record (x1, x2) -> Pexp_record (x1, x2)
    | Pexp_field (x1, x2) -> Pexp_field (x1, x2)
    | Pexp_setfield (x1, x2, x3) -> Pexp_setfield (x1, x2, x3)
    | Pexp_ifthenelse (x1, x2, x3) -> Pexp_ifthenelse (x1, x2, x3)
    | Pexp_sequence (x1, x2) -> Pexp_sequence (x1, x2)
    | Pexp_while (x1, x2) -> Pexp_while (x1, x2)
    | Pexp_for (x1, x2, x3, x4, x5) -> Pexp_for (x1, x2, x3, x4, x5)
    | Pexp_coerce (x1, x2, x3) -> Pexp_coerce (x1, x2, x3)
    | Pexp_send (x1, x2) -> Pexp_send (x1, x2)
    | Pexp_new x -> Pexp_new x
    | Pexp_setvar (x1, x2) -> Pexp_setinstvar (x1, x2)
    | Pexp_override x -> Pexp_override x
    | Pexp_letmodule (x1, x2, x3) -> Pexp_letmodule (x1, x2, x3)
    | Pexp_letexception (x1, x2) -> Pexp_letexception (x1, x2)
    | Pexp_assert x -> Pexp_assert x
    | Pexp_lazy x -> Pexp_lazy x
    | Pexp_poly (x1, x2) -> Pexp_poly (x1, x2)
    | Pexp_object x -> Pexp_object x
    | Pexp_pack x -> Pexp_pack x
    | Pexp_open (x1, x2) -> Pexp_open (x1, x2)
    | Pexp_letop x -> Pexp_letop x
    | Pexp_extension x -> Pexp_extension x
    | Pexp_unreachable -> Pexp_unreachable
    | Pexp_stack { pexp_desc; pexp_attributes; pexp_loc = _; pexp_loc_stack = _ } ->
      (match pexp_attributes with
       | [] -> pexp_desc
       | _ :: _ ->
         Location.raise_errorf
           ~loc
           "[Pexp_stack] cannot be converted to an upstream [expression_desc] without \
            erasing attributes")
    | Pexp_comprehension _ ->
      Location.raise_errorf
        ~loc
        "[Pexp_comprehension] cannot be converted to an upstream [expression_desc]"
    | Pexp_overwrite _ ->
      Location.raise_errorf
        ~loc
        "[Pexp_overwrite] cannot be converted to an upstream [expression_desc]"
    | Pexp_quote _ ->
      Location.raise_errorf
        ~loc
        "[Pexp_quote] cannot be converted to an upstream [expression_desc]"
    | Pexp_splice _ ->
      Location.raise_errorf
        ~loc
        "[Pexp_slice] cannot be converted to an upstream [expression_desc]"
    | Pexp_hole ->
      Pexp_assert
        { pexp_desc = Pexp_construct ({ loc; txt = Lident "false" }, None)
        ; pexp_loc = loc
        ; pexp_loc_stack = []
        ; pexp_attributes = []
        }
    | Pexp_borrow { pexp_desc; pexp_attributes; pexp_loc = _; pexp_loc_stack = _ } ->
      (match pexp_attributes with
       | [] -> pexp_desc
       | _ :: _ ->
         Location.raise_errorf
           ~loc
           "[Pexp_borrow] cannot be converted to an upstream [expression_desc] without \
            erasing attributes")
  ;;

  let of_parsetree (expr_desc : expression_desc) ~loc : t =
    (* changed constructors *)
    match Pexp_function.of_parsetree expr_desc ~loc with
    | Some (x1, x2, x3) -> Pexp_function (x1, x2, x3)
    | None ->
      (match expr_desc with
       | Pexp_function _ ->
         (* matched by above call to [of_parsetree] *)
         assert false
       | Pexp_constraint (x1, x2) -> Pexp_constraint (x1, Some x2, [])
       | Pexp_tuple x -> Pexp_tuple (add_none_labels x)
       | Pexp_newtype (x1, x2) -> Pexp_newtype (x1, None, x2)
       | Pexp_array x -> Pexp_array (Mutable, x)
       (* unchanged constructors *)
       | Pexp_ident x -> Pexp_ident x
       | Pexp_constant x -> Pexp_constant x
       | Pexp_let (x1, x2, x3) -> Pexp_let (Immutable, x1, x2, x3)
       | Pexp_apply (x1, x2) -> Pexp_apply (x1, x2)
       | Pexp_match (x1, x2) -> Pexp_match (x1, x2)
       | Pexp_try (x1, x2) -> Pexp_try (x1, x2)
       | Pexp_construct (x1, x2) -> Pexp_construct (x1, x2)
       | Pexp_variant (x1, x2) -> Pexp_variant (x1, x2)
       | Pexp_record (x1, x2) -> Pexp_record (x1, x2)
       | Pexp_field (x1, x2) -> Pexp_field (x1, x2)
       | Pexp_setfield (x1, x2, x3) -> Pexp_setfield (x1, x2, x3)
       | Pexp_ifthenelse (x1, x2, x3) -> Pexp_ifthenelse (x1, x2, x3)
       | Pexp_sequence (x1, x2) -> Pexp_sequence (x1, x2)
       | Pexp_while (x1, x2) -> Pexp_while (x1, x2)
       | Pexp_for (x1, x2, x3, x4, x5) -> Pexp_for (x1, x2, x3, x4, x5)
       | Pexp_coerce (x1, x2, x3) -> Pexp_coerce (x1, x2, x3)
       | Pexp_send (x1, x2) -> Pexp_send (x1, x2)
       | Pexp_new x -> Pexp_new x
       | Pexp_setinstvar (x1, x2) -> Pexp_setvar (x1, x2)
       | Pexp_override x -> Pexp_override x
       | Pexp_letmodule (x1, x2, x3) -> Pexp_letmodule (x1, x2, x3)
       | Pexp_letexception (x1, x2) -> Pexp_letexception (x1, x2)
       | Pexp_assert x -> Pexp_assert x
       | Pexp_lazy x -> Pexp_lazy x
       | Pexp_poly (x1, x2) -> Pexp_poly (x1, x2)
       | Pexp_object x -> Pexp_object x
       | Pexp_pack x -> Pexp_pack x
       | Pexp_open (x1, x2) -> Pexp_open (x1, x2)
       | Pexp_letop x -> Pexp_letop x
       | Pexp_extension x -> Pexp_extension x
       | Pexp_unreachable -> Pexp_unreachable)
  ;;
end

module Type_kind = struct
  type t =
    | Ptype_abstract
    | Ptype_variant of constructor_declaration list
    | Ptype_record of label_declaration list
    | Ptype_record_unboxed_product of label_declaration list
    | Ptype_open

  let of_parsetree : type_kind -> t = function
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant x -> Ptype_variant x
    | Ptype_record x -> Ptype_record x
    | Ptype_open -> Ptype_open
  ;;

  let to_parsetree : t -> type_kind = function
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant x -> Ptype_variant x
    | Ptype_record x -> Ptype_record x
    | Ptype_record_unboxed_product x -> Ptype_record x
    | Ptype_open -> Ptype_open
  ;;
end

module Constructor_declaration = struct
  let extract_vars_with_jkind_annotations cd = List.map (fun s -> s, None) cd.pcd_vars

  let create ~name ~vars ~args ~res ~loc =
    { pcd_name = name
    ; pcd_vars = List.map fst vars
    ; pcd_args = args
    ; pcd_res = res
    ; pcd_loc = loc
    ; pcd_attributes = []
    }
  ;;
end

module Include_infos = struct
  type 'a t =
    { pincl_kind : Include_kind.t
    ; pincl_mod : 'a
    ; pincl_loc : Location.t
    ; pincl_attributes : attributes
    }

  let of_parsetree x : 'a t =
    let ({ pincl_mod; pincl_loc; pincl_attributes } : 'a include_infos) = x in
    let pincl_kind : Include_kind.t = Structure in
    { pincl_kind; pincl_mod; pincl_loc; pincl_attributes }
  ;;

  let to_parsetree x : 'a include_infos =
    let ({ pincl_kind; pincl_mod; pincl_loc; pincl_attributes } : 'a t) = x in
    match pincl_kind with
    | Structure -> { pincl_mod; pincl_loc; pincl_attributes }
    | Functor ->
      Location.raise_errorf
        ~loc:pincl_loc
        "[include functor] cannot be converted to an upstream [include_infos]"
  ;;
end

module Signature_item_desc = struct
  type t =
    | Psig_value of value_description
    | Psig_type of rec_flag * type_declaration list
    | Psig_typesubst of type_declaration list
    | Psig_typext of type_extension
    | Psig_exception of type_exception
    | Psig_module of module_declaration
    | Psig_modsubst of module_substitution
    | Psig_recmodule of module_declaration list
    | Psig_modtype of module_type_declaration
    | Psig_modtypesubst of module_type_declaration
    | Psig_open of open_description
    | Psig_include of include_description * Modalities.t
    | Psig_class of class_description list
    | Psig_class_type of class_type_declaration list
    | Psig_attribute of attribute
    | Psig_extension of extension * attributes
    | Psig_jkind of jkind_declaration

  let of_parsetree (sig_desc : signature_item_desc) =
    match sig_desc with
    | Psig_value a -> Psig_value a
    | Psig_type (a, b) -> Psig_type (a, b)
    | Psig_typesubst a -> Psig_typesubst a
    | Psig_typext a -> Psig_typext a
    | Psig_exception a -> Psig_exception a
    | Psig_module a -> Psig_module a
    | Psig_modsubst a -> Psig_modsubst a
    | Psig_recmodule a -> Psig_recmodule a
    | Psig_modtype a -> Psig_modtype a
    | Psig_modtypesubst a -> Psig_modtypesubst a
    | Psig_open a -> Psig_open a
    | Psig_include a -> Psig_include (a, [])
    | Psig_class a -> Psig_class a
    | Psig_class_type a -> Psig_class_type a
    | Psig_attribute a -> Psig_attribute a
    | Psig_extension (a, b) -> Psig_extension (a, b)
  ;;

  let to_parsetree (t : t) : signature_item_desc =
    match t with
    | Psig_value a -> Psig_value a
    | Psig_type (a, b) -> Psig_type (a, b)
    | Psig_typesubst a -> Psig_typesubst a
    | Psig_typext a -> Psig_typext a
    | Psig_exception a -> Psig_exception a
    | Psig_module a -> Psig_module a
    | Psig_modsubst a -> Psig_modsubst a
    | Psig_recmodule a -> Psig_recmodule a
    | Psig_modtype a -> Psig_modtype a
    | Psig_modtypesubst a -> Psig_modtypesubst a
    | Psig_open a -> Psig_open a
    | Psig_include (a, _) -> Psig_include a
    | Psig_class a -> Psig_class a
    | Psig_class_type a -> Psig_class_type a
    | Psig_attribute a -> Psig_attribute a
    | Psig_extension (a, b) -> Psig_extension (a, b)
    | Psig_jkind _ ->
      (* erase to [include sig end] *)
      Psig_include
        { pincl_loc = Location.none
        ; pincl_attributes = []
        ; pincl_mod =
            { pmty_desc = Pmty_signature []
            ; pmty_loc = Location.none
            ; pmty_attributes = []
            }
        }
  ;;
end

module Signature = struct
  type t =
    { psg_modalities : Modalities.t
    ; psg_items : signature_item list
    ; psg_loc : Location.t
    }

  let of_parsetree psg_items = { psg_items; psg_modalities = []; psg_loc = Location.none }

  let to_parsetree { psg_items; psg_modalities = _; psg_loc = _ } = psg_items
end

module Structure_item_desc = struct
  type t =
    | Pstr_eval of expression * attributes
    | Pstr_value of rec_flag * value_binding list
    | Pstr_primitive of value_description
    | Pstr_type of rec_flag * type_declaration list
    | Pstr_typext of type_extension
    | Pstr_exception of type_exception
    | Pstr_module of module_binding
    | Pstr_recmodule of module_binding list
    | Pstr_modtype of module_type_declaration
    | Pstr_open of open_declaration
    | Pstr_class of class_declaration list
    | Pstr_class_type of class_type_declaration list
    | Pstr_include of include_declaration
    | Pstr_attribute of attribute
    | Pstr_extension of extension * attributes
    | Pstr_jkind of jkind_declaration

  let of_parsetree : structure_item_desc -> t = function
    | Pstr_eval (a, b) -> Pstr_eval (a, b)
    | Pstr_value (a, b) -> Pstr_value (a, b)
    | Pstr_primitive a -> Pstr_primitive a
    | Pstr_type (a, b) -> Pstr_type (a, b)
    | Pstr_typext a -> Pstr_typext a
    | Pstr_exception a -> Pstr_exception a
    | Pstr_module a -> Pstr_module a
    | Pstr_recmodule a -> Pstr_recmodule a
    | Pstr_modtype a -> Pstr_modtype a
    | Pstr_open a -> Pstr_open a
    | Pstr_class a -> Pstr_class a
    | Pstr_class_type a -> Pstr_class_type a
    | Pstr_include a -> Pstr_include a
    | Pstr_attribute a -> Pstr_attribute a
    | Pstr_extension (a, b) -> Pstr_extension (a, b)
  ;;

  let to_parsetree : t -> structure_item_desc = function
    | Pstr_eval (a, b) -> Pstr_eval (a, b)
    | Pstr_value (a, b) -> Pstr_value (a, b)
    | Pstr_primitive a -> Pstr_primitive a
    | Pstr_type (a, b) -> Pstr_type (a, b)
    | Pstr_typext a -> Pstr_typext a
    | Pstr_exception a -> Pstr_exception a
    | Pstr_module a -> Pstr_module a
    | Pstr_recmodule a -> Pstr_recmodule a
    | Pstr_modtype a -> Pstr_modtype a
    | Pstr_open a -> Pstr_open a
    | Pstr_class a -> Pstr_class a
    | Pstr_class_type a -> Pstr_class_type a
    | Pstr_include a -> Pstr_include a
    | Pstr_attribute a -> Pstr_attribute a
    | Pstr_extension (a, b) -> Pstr_extension (a, b)
    | Pstr_jkind _ ->
      (* erase to [include struct end] *)
      Pstr_include
        { pincl_loc = Location.none
        ; pincl_attributes = []
        ; pincl_mod =
            { pmod_desc = Pmod_structure []
            ; pmod_loc = Location.none
            ; pmod_attributes = []
            }
        }
  ;;
end

module Functor_parameter = struct
  type t =
    | Unit
    | Named of string option loc * module_type * Modes.t

  let to_parsetree (t : t) : functor_parameter =
    match t with
    | Unit -> Unit
    | Named (name, type_, _) -> Named (name, type_)
  ;;

  let of_parsetree (t : functor_parameter) : t =
    match t with
    | Unit -> Unit
    | Named (name, type_) -> Named (name, type_, [])
  ;;
end

module Module_type_desc = struct
  type t =
    | Pmty_ident of Astlib.Longident.t loc
    | Pmty_signature of signature
    | Pmty_functor of functor_parameter * module_type * Modes.t
    | Pmty_with of module_type * with_constraint list
    | Pmty_typeof of module_expr
    | Pmty_extension of extension
    | Pmty_alias of Astlib.Longident.t loc
    | Pmty_strengthen of module_type * Astlib.Longident.t loc

  let of_parsetree : module_type_desc -> t = function
    | Pmty_ident x -> Pmty_ident x
    | Pmty_signature x -> Pmty_signature x
    | Pmty_functor (x0, x1) -> Pmty_functor (x0, x1, [])
    | Pmty_with (x0, x1) -> Pmty_with (x0, x1)
    | Pmty_typeof x -> Pmty_typeof x
    | Pmty_extension x -> Pmty_extension x
    | Pmty_alias x -> Pmty_alias x
  ;;

  let to_parsetree : loc:Location.t -> t -> module_type_desc =
    fun ~loc -> function
    (* new constructors *)
    | Pmty_strengthen _ ->
      Location.raise_errorf
        ~loc
        "[Pmty_strengthen] cannot be converted to an upstream [module_type_desc]"
    (* unchanged constructors *)
    | Pmty_ident x -> Pmty_ident x
    | Pmty_signature x -> Pmty_signature x
    | Pmty_functor (x0, x1, _) -> Pmty_functor (x0, x1)
    | Pmty_with (x0, x1) -> Pmty_with (x0, x1)
    | Pmty_typeof x -> Pmty_typeof x
    | Pmty_extension x -> Pmty_extension x
    | Pmty_alias x -> Pmty_alias x
  ;;
end

module Module_expr_desc = struct
  type module_instance = private Module_instance

  type t =
    | Pmod_ident of Astlib.Longident.t loc
    | Pmod_structure of structure
    | Pmod_functor of functor_parameter * module_expr
    | Pmod_apply of module_expr * module_expr
    | Pmod_constraint of module_expr * module_type option * Modes.t
    | Pmod_unpack of expression
    | Pmod_extension of extension
    | Pmod_hole
    | Pmod_instance of module_instance

  let of_parsetree : module_expr_desc -> t = function
    | Pmod_ident x -> Pmod_ident x
    | Pmod_structure x -> Pmod_structure x
    | Pmod_functor (x0, x1) -> Pmod_functor (x0, x1)
    | Pmod_apply (x0, x1) -> Pmod_apply (x0, x1)
    | Pmod_apply_unit x ->
      (* [M ()] as it was represented before OCaml 5.1. *)
      let loc = { x.pmod_loc with loc_ghost = true } in
      Pmod_apply
        (x, { pmod_desc = Pmod_structure []; pmod_loc = loc; pmod_attributes = [] })
    | Pmod_constraint (x0, x1) -> Pmod_constraint (x0, Some x1, [])
    | Pmod_unpack x -> Pmod_unpack x
    | Pmod_extension x -> Pmod_extension x
  ;;

  let to_parsetree : loc:Location.t -> t -> module_expr_desc =
    fun ~loc -> function
    (* new constructors *)
    | Pmod_hole ->
      Location.raise_errorf
        ~loc
        "[Pmod_hole] cannot be converted to an upstream [module_expr_desc]"
    | Pmod_instance _ ->
      Location.raise_errorf
        ~loc
        "[Pmod_instance] cannot be converted to an upstream [module_expr_desc]"
    (* changed constructors *)
    | Pmod_constraint (x0, x1, _) ->
      (match x1 with
       | Some x1 -> Pmod_constraint (x0, x1)
       | None ->
         (match x0 with
          | { pmod_desc; pmod_loc = _; pmod_attributes = [] } -> pmod_desc
          | { pmod_attributes = _ :: _; _ } ->
            Location.raise_errorf
              ~loc
              "[Pmod_constraint] without type cannot be converted to an upstream \
               [module_expr_desc] without erasing attributes"))
    (* unchanged constructors *)
    | Pmod_ident x -> Pmod_ident x
    | Pmod_structure x -> Pmod_structure x
    | Pmod_functor (x0, x1) -> Pmod_functor (x0, x1)
    | Pmod_apply (x0, x1) -> Pmod_apply (x0, x1)
    | Pmod_unpack x -> Pmod_unpack x
    | Pmod_extension x -> Pmod_extension x
  ;;
end

module Ast_traverse = struct
  module Deriving_inline = struct
    type location = Location.t
    type longident = Astlib.Longident.t

    type jkind_annotation_desc = T.jkind_annotation_desc =
      | Pjk_default
      | Pjk_abbreviation of longident loc
      | Pjk_operator of jkind_annotation * string loc list
      | Pjk_mod of jkind_annotation * modes
      | Pjk_with of jkind_annotation * core_type * modalities
      | Pjk_kind_of of core_type
      | Pjk_product of jkind_annotation list

    and jkind_annotation = T.jkind_annotation =
      { pjka_loc : location
      ; pjka_desc : jkind_annotation_desc
      }

    and jkind_declaration = T.jkind_declaration =
      { pjkind_name : string loc
      ; pjkind_manifest : jkind_annotation option
      ; pjkind_attributes : attributes
      ; pjkind_loc : location
      }

    (* Unlike the types below, [Pexp_function.function_param] and
       [Pexp_function.function_param_desc] are not part of this deriving block: upstream
       ppxlib's AST has its own (jkind-less) [function_param] type, and the traversal
       methods it generates for it would clash with ones generated here. *)
    and function_constraint = Pexp_function.Function_constraint.t =
      { mode_annotations : modes
      ; ret_mode_annotations : modes
      ; ret_type_constraint : type_constraint option
      }

    and mode = Mode.t = Mode of string [@@unboxed]
    and modes = mode loc list
    and modality = Modality.t = Modality of string [@@unboxed]
    and modalities = modality loc list
    and signature_items = signature_item list
    and signature = signature_items [@@deriving_inline traverse]

    class virtual map =
      object (self)
        method virtual attributes : attributes -> attributes
        method virtual core_type : core_type -> core_type
        method virtual list : 'a. ('a -> 'a) -> 'a list -> 'a list
        method virtual loc : 'a. ('a -> 'a) -> 'a loc -> 'a loc
        method virtual location : location -> location
        method virtual longident : longident -> longident
        method virtual option : 'a. ('a -> 'a) -> 'a option -> 'a option
        method virtual signature_item : signature_item -> signature_item
        method virtual string : string -> string
        method virtual type_constraint : type_constraint -> type_constraint

        method jkind_annotation_desc : jkind_annotation_desc -> jkind_annotation_desc =
          fun x ->
            match x with
            | Pjk_default -> Pjk_default
            | Pjk_abbreviation a ->
              let a = self#loc self#longident a in
              Pjk_abbreviation a
            | Pjk_operator (a, b) ->
              let a = self#jkind_annotation a in
              let b = self#list (self#loc self#string) b in
              Pjk_operator (a, b)
            | Pjk_mod (a, b) ->
              let a = self#jkind_annotation a in
              let b = self#modes b in
              Pjk_mod (a, b)
            | Pjk_with (a, b, c) ->
              let a = self#jkind_annotation a in
              let b = self#core_type b in
              let c = self#modalities c in
              Pjk_with (a, b, c)
            | Pjk_kind_of a ->
              let a = self#core_type a in
              Pjk_kind_of a
            | Pjk_product a ->
              let a = self#list self#jkind_annotation a in
              Pjk_product a

        method jkind_annotation : jkind_annotation -> jkind_annotation =
          fun { pjka_loc; pjka_desc } ->
            let pjka_loc = self#location pjka_loc in
            let pjka_desc = self#jkind_annotation_desc pjka_desc in
            { pjka_loc; pjka_desc }

        method jkind_declaration : jkind_declaration -> jkind_declaration =
          fun { pjkind_name; pjkind_manifest; pjkind_attributes; pjkind_loc } ->
            let pjkind_name = self#loc self#string pjkind_name in
            let pjkind_manifest = self#option self#jkind_annotation pjkind_manifest in
            let pjkind_attributes = self#attributes pjkind_attributes in
            let pjkind_loc = self#location pjkind_loc in
            { pjkind_name; pjkind_manifest; pjkind_attributes; pjkind_loc }

        method function_constraint : function_constraint -> function_constraint =
          fun { mode_annotations; ret_mode_annotations; ret_type_constraint } ->
            let mode_annotations = self#modes mode_annotations in
            let ret_mode_annotations = self#modes ret_mode_annotations in
            let ret_type_constraint =
              self#option self#type_constraint ret_type_constraint
            in
            { mode_annotations; ret_mode_annotations; ret_type_constraint }

        method mode : mode -> mode =
          fun x ->
            match x with
            | Mode a ->
              let a = self#string a in
              Mode a

        method modes : modes -> modes = self#list (self#loc self#mode)

        method modality : modality -> modality =
          fun x ->
            match x with
            | Modality a ->
              let a = self#string a in
              Modality a

        method modalities : modalities -> modalities = self#list (self#loc self#modality)

        method signature_items : signature_items -> signature_items =
          self#list self#signature_item

        method signature : signature -> signature = self#signature_items
      end

    class virtual iter =
      object (self)
        method virtual attributes : attributes -> unit
        method virtual core_type : core_type -> unit
        method virtual list : 'a. ('a -> unit) -> 'a list -> unit
        method virtual loc : 'a. ('a -> unit) -> 'a loc -> unit
        method virtual location : location -> unit
        method virtual longident : longident -> unit
        method virtual option : 'a. ('a -> unit) -> 'a option -> unit
        method virtual signature_item : signature_item -> unit
        method virtual string : string -> unit
        method virtual type_constraint : type_constraint -> unit

        method jkind_annotation_desc : jkind_annotation_desc -> unit =
          fun x ->
            match x with
            | Pjk_default -> ()
            | Pjk_abbreviation a -> self#loc self#longident a
            | Pjk_operator (a, b) ->
              self#jkind_annotation a;
              self#list (self#loc self#string) b
            | Pjk_mod (a, b) ->
              self#jkind_annotation a;
              self#modes b
            | Pjk_with (a, b, c) ->
              self#jkind_annotation a;
              self#core_type b;
              self#modalities c
            | Pjk_kind_of a -> self#core_type a
            | Pjk_product a -> self#list self#jkind_annotation a

        method jkind_annotation : jkind_annotation -> unit =
          fun { pjka_loc; pjka_desc } ->
            self#location pjka_loc;
            self#jkind_annotation_desc pjka_desc

        method jkind_declaration : jkind_declaration -> unit =
          fun { pjkind_name; pjkind_manifest; pjkind_attributes; pjkind_loc } ->
            self#loc self#string pjkind_name;
            self#option self#jkind_annotation pjkind_manifest;
            self#attributes pjkind_attributes;
            self#location pjkind_loc

        method function_constraint : function_constraint -> unit =
          fun { mode_annotations; ret_mode_annotations; ret_type_constraint } ->
            self#modes mode_annotations;
            self#modes ret_mode_annotations;
            self#option self#type_constraint ret_type_constraint

        method mode : mode -> unit =
          fun x ->
            match x with
            | Mode a -> self#string a

        method modes : modes -> unit = self#list (self#loc self#mode)

        method modality : modality -> unit =
          fun x ->
            match x with
            | Modality a -> self#string a

        method modalities : modalities -> unit = self#list (self#loc self#modality)
        method signature_items : signature_items -> unit = self#list self#signature_item
        method signature : signature -> unit = self#signature_items
      end

    class virtual ['acc] fold =
      object (self)
        method virtual attributes : attributes -> 'acc -> 'acc
        method virtual core_type : core_type -> 'acc -> 'acc
        method virtual list : 'a. ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
        method virtual loc : 'a. ('a -> 'acc -> 'acc) -> 'a loc -> 'acc -> 'acc
        method virtual location : location -> 'acc -> 'acc
        method virtual longident : longident -> 'acc -> 'acc
        method virtual option : 'a. ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc
        method virtual signature_item : signature_item -> 'acc -> 'acc
        method virtual string : string -> 'acc -> 'acc
        method virtual type_constraint : type_constraint -> 'acc -> 'acc

        method jkind_annotation_desc : jkind_annotation_desc -> 'acc -> 'acc =
          fun x acc ->
            match x with
            | Pjk_default -> acc
            | Pjk_abbreviation a -> self#loc self#longident a acc
            | Pjk_operator (a, b) ->
              let acc = self#jkind_annotation a acc in
              let acc = self#list (self#loc self#string) b acc in
              acc
            | Pjk_mod (a, b) ->
              let acc = self#jkind_annotation a acc in
              let acc = self#modes b acc in
              acc
            | Pjk_with (a, b, c) ->
              let acc = self#jkind_annotation a acc in
              let acc = self#core_type b acc in
              let acc = self#modalities c acc in
              acc
            | Pjk_kind_of a -> self#core_type a acc
            | Pjk_product a -> self#list self#jkind_annotation a acc

        method jkind_annotation : jkind_annotation -> 'acc -> 'acc =
          fun { pjka_loc; pjka_desc } acc ->
            let acc = self#location pjka_loc acc in
            let acc = self#jkind_annotation_desc pjka_desc acc in
            acc

        method jkind_declaration : jkind_declaration -> 'acc -> 'acc =
          fun { pjkind_name; pjkind_manifest; pjkind_attributes; pjkind_loc } acc ->
            let acc = self#loc self#string pjkind_name acc in
            let acc = self#option self#jkind_annotation pjkind_manifest acc in
            let acc = self#attributes pjkind_attributes acc in
            let acc = self#location pjkind_loc acc in
            acc

        method function_constraint : function_constraint -> 'acc -> 'acc =
          fun { mode_annotations; ret_mode_annotations; ret_type_constraint } acc ->
            let acc = self#modes mode_annotations acc in
            let acc = self#modes ret_mode_annotations acc in
            let acc = self#option self#type_constraint ret_type_constraint acc in
            acc

        method mode : mode -> 'acc -> 'acc =
          fun x acc ->
            match x with
            | Mode a -> self#string a acc

        method modes : modes -> 'acc -> 'acc = self#list (self#loc self#mode)

        method modality : modality -> 'acc -> 'acc =
          fun x acc ->
            match x with
            | Modality a -> self#string a acc

        method modalities : modalities -> 'acc -> 'acc =
          self#list (self#loc self#modality)

        method signature_items : signature_items -> 'acc -> 'acc =
          self#list self#signature_item

        method signature : signature -> 'acc -> 'acc = self#signature_items
      end

    class virtual ['acc] fold_map =
      object (self)
        method virtual attributes : attributes -> 'acc -> attributes * 'acc
        method virtual core_type : core_type -> 'acc -> core_type * 'acc

        method
          virtual list
          : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a list -> 'acc -> 'a list * 'acc

        method
          virtual loc
          : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a loc -> 'acc -> 'a loc * 'acc

        method virtual location : location -> 'acc -> location * 'acc
        method virtual longident : longident -> 'acc -> longident * 'acc

        method
          virtual option
          : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a option -> 'acc -> 'a option * 'acc

        method virtual signature_item : signature_item -> 'acc -> signature_item * 'acc
        method virtual string : string -> 'acc -> string * 'acc
        method virtual type_constraint : type_constraint -> 'acc -> type_constraint * 'acc

        method jkind_annotation_desc
          : jkind_annotation_desc -> 'acc -> jkind_annotation_desc * 'acc =
          fun x acc ->
            match x with
            | Pjk_default -> Pjk_default, acc
            | Pjk_abbreviation a ->
              let a, acc = self#loc self#longident a acc in
              Pjk_abbreviation a, acc
            | Pjk_operator (a, b) ->
              let a, acc = self#jkind_annotation a acc in
              let b, acc = self#list (self#loc self#string) b acc in
              Pjk_operator (a, b), acc
            | Pjk_mod (a, b) ->
              let a, acc = self#jkind_annotation a acc in
              let b, acc = self#modes b acc in
              Pjk_mod (a, b), acc
            | Pjk_with (a, b, c) ->
              let a, acc = self#jkind_annotation a acc in
              let b, acc = self#core_type b acc in
              let c, acc = self#modalities c acc in
              Pjk_with (a, b, c), acc
            | Pjk_kind_of a ->
              let a, acc = self#core_type a acc in
              Pjk_kind_of a, acc
            | Pjk_product a ->
              let a, acc = self#list self#jkind_annotation a acc in
              Pjk_product a, acc

        method jkind_annotation : jkind_annotation -> 'acc -> jkind_annotation * 'acc =
          fun { pjka_loc; pjka_desc } acc ->
            let pjka_loc, acc = self#location pjka_loc acc in
            let pjka_desc, acc = self#jkind_annotation_desc pjka_desc acc in
            { pjka_loc; pjka_desc }, acc

        method jkind_declaration : jkind_declaration -> 'acc -> jkind_declaration * 'acc =
          fun { pjkind_name; pjkind_manifest; pjkind_attributes; pjkind_loc } acc ->
            let pjkind_name, acc = self#loc self#string pjkind_name acc in
            let pjkind_manifest, acc =
              self#option self#jkind_annotation pjkind_manifest acc
            in
            let pjkind_attributes, acc = self#attributes pjkind_attributes acc in
            let pjkind_loc, acc = self#location pjkind_loc acc in
            { pjkind_name; pjkind_manifest; pjkind_attributes; pjkind_loc }, acc

        method function_constraint
          : function_constraint -> 'acc -> function_constraint * 'acc =
          fun { mode_annotations; ret_mode_annotations; ret_type_constraint } acc ->
            let mode_annotations, acc = self#modes mode_annotations acc in
            let ret_mode_annotations, acc = self#modes ret_mode_annotations acc in
            let ret_type_constraint, acc =
              self#option self#type_constraint ret_type_constraint acc
            in
            { mode_annotations; ret_mode_annotations; ret_type_constraint }, acc

        method mode : mode -> 'acc -> mode * 'acc =
          fun x acc ->
            match x with
            | Mode a ->
              let a, acc = self#string a acc in
              Mode a, acc

        method modes : modes -> 'acc -> modes * 'acc = self#list (self#loc self#mode)

        method modality : modality -> 'acc -> modality * 'acc =
          fun x acc ->
            match x with
            | Modality a ->
              let a, acc = self#string a acc in
              Modality a, acc

        method modalities : modalities -> 'acc -> modalities * 'acc =
          self#list (self#loc self#modality)

        method signature_items : signature_items -> 'acc -> signature_items * 'acc =
          self#list self#signature_item

        method signature : signature -> 'acc -> signature * 'acc = self#signature_items
      end

    class virtual ['ctx] map_with_context =
      object (self)
        method virtual attributes : 'ctx -> attributes -> attributes
        method virtual core_type : 'ctx -> core_type -> core_type
        method virtual list : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list
        method virtual loc : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a loc -> 'a loc
        method virtual location : 'ctx -> location -> location
        method virtual longident : 'ctx -> longident -> longident
        method virtual option : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option
        method virtual signature_item : 'ctx -> signature_item -> signature_item
        method virtual string : 'ctx -> string -> string
        method virtual type_constraint : 'ctx -> type_constraint -> type_constraint

        method jkind_annotation_desc
          : 'ctx -> jkind_annotation_desc -> jkind_annotation_desc =
          fun ctx x ->
            match x with
            | Pjk_default -> Pjk_default
            | Pjk_abbreviation a ->
              let a = self#loc self#longident ctx a in
              Pjk_abbreviation a
            | Pjk_operator (a, b) ->
              let a = self#jkind_annotation ctx a in
              let b = self#list (self#loc self#string) ctx b in
              Pjk_operator (a, b)
            | Pjk_mod (a, b) ->
              let a = self#jkind_annotation ctx a in
              let b = self#modes ctx b in
              Pjk_mod (a, b)
            | Pjk_with (a, b, c) ->
              let a = self#jkind_annotation ctx a in
              let b = self#core_type ctx b in
              let c = self#modalities ctx c in
              Pjk_with (a, b, c)
            | Pjk_kind_of a ->
              let a = self#core_type ctx a in
              Pjk_kind_of a
            | Pjk_product a ->
              let a = self#list self#jkind_annotation ctx a in
              Pjk_product a

        method jkind_annotation : 'ctx -> jkind_annotation -> jkind_annotation =
          fun ctx { pjka_loc; pjka_desc } ->
            let pjka_loc = self#location ctx pjka_loc in
            let pjka_desc = self#jkind_annotation_desc ctx pjka_desc in
            { pjka_loc; pjka_desc }

        method jkind_declaration : 'ctx -> jkind_declaration -> jkind_declaration =
          fun ctx { pjkind_name; pjkind_manifest; pjkind_attributes; pjkind_loc } ->
            let pjkind_name = self#loc self#string ctx pjkind_name in
            let pjkind_manifest = self#option self#jkind_annotation ctx pjkind_manifest in
            let pjkind_attributes = self#attributes ctx pjkind_attributes in
            let pjkind_loc = self#location ctx pjkind_loc in
            { pjkind_name; pjkind_manifest; pjkind_attributes; pjkind_loc }

        method function_constraint : 'ctx -> function_constraint -> function_constraint =
          fun ctx { mode_annotations; ret_mode_annotations; ret_type_constraint } ->
            let mode_annotations = self#modes ctx mode_annotations in
            let ret_mode_annotations = self#modes ctx ret_mode_annotations in
            let ret_type_constraint =
              self#option self#type_constraint ctx ret_type_constraint
            in
            { mode_annotations; ret_mode_annotations; ret_type_constraint }

        method mode : 'ctx -> mode -> mode =
          fun ctx x ->
            match x with
            | Mode a ->
              let a = self#string ctx a in
              Mode a

        method modes : 'ctx -> modes -> modes = self#list (self#loc self#mode)

        method modality : 'ctx -> modality -> modality =
          fun ctx x ->
            match x with
            | Modality a ->
              let a = self#string ctx a in
              Modality a

        method modalities : 'ctx -> modalities -> modalities =
          self#list (self#loc self#modality)

        method signature_items : 'ctx -> signature_items -> signature_items =
          self#list self#signature_item

        method signature : 'ctx -> signature -> signature = self#signature_items
      end

    class virtual ['res] lift =
      object (self)
        method virtual record : (string * 'res) list -> 'res
        method virtual constr : string -> 'res list -> 'res
        method virtual attributes : attributes -> 'res
        method virtual core_type : core_type -> 'res
        method virtual list : 'a. ('a -> 'res) -> 'a list -> 'res
        method virtual loc : 'a. ('a -> 'res) -> 'a loc -> 'res
        method virtual location : location -> 'res
        method virtual longident : longident -> 'res
        method virtual option : 'a. ('a -> 'res) -> 'a option -> 'res
        method virtual signature_item : signature_item -> 'res
        method virtual string : string -> 'res
        method virtual type_constraint : type_constraint -> 'res

        method jkind_annotation_desc : jkind_annotation_desc -> 'res =
          fun x ->
            match x with
            | Pjk_default -> self#constr "Pjk_default" []
            | Pjk_abbreviation a ->
              let a = self#loc self#longident a in
              self#constr "Pjk_abbreviation" [ a ]
            | Pjk_operator (a, b) ->
              let a = self#jkind_annotation a in
              let b = self#list (self#loc self#string) b in
              self#constr "Pjk_operator" [ a; b ]
            | Pjk_mod (a, b) ->
              let a = self#jkind_annotation a in
              let b = self#modes b in
              self#constr "Pjk_mod" [ a; b ]
            | Pjk_with (a, b, c) ->
              let a = self#jkind_annotation a in
              let b = self#core_type b in
              let c = self#modalities c in
              self#constr "Pjk_with" [ a; b; c ]
            | Pjk_kind_of a ->
              let a = self#core_type a in
              self#constr "Pjk_kind_of" [ a ]
            | Pjk_product a ->
              let a = self#list self#jkind_annotation a in
              self#constr "Pjk_product" [ a ]

        method jkind_annotation : jkind_annotation -> 'res =
          fun { pjka_loc; pjka_desc } ->
            let pjka_loc = self#location pjka_loc in
            let pjka_desc = self#jkind_annotation_desc pjka_desc in
            self#record [ "pjka_loc", pjka_loc; "pjka_desc", pjka_desc ]

        method jkind_declaration : jkind_declaration -> 'res =
          fun { pjkind_name; pjkind_manifest; pjkind_attributes; pjkind_loc } ->
            let pjkind_name = self#loc self#string pjkind_name in
            let pjkind_manifest = self#option self#jkind_annotation pjkind_manifest in
            let pjkind_attributes = self#attributes pjkind_attributes in
            let pjkind_loc = self#location pjkind_loc in
            self#record
              [ "pjkind_name", pjkind_name
              ; "pjkind_manifest", pjkind_manifest
              ; "pjkind_attributes", pjkind_attributes
              ; "pjkind_loc", pjkind_loc
              ]

        method function_constraint : function_constraint -> 'res =
          fun { mode_annotations; ret_mode_annotations; ret_type_constraint } ->
            let mode_annotations = self#modes mode_annotations in
            let ret_mode_annotations = self#modes ret_mode_annotations in
            let ret_type_constraint =
              self#option self#type_constraint ret_type_constraint
            in
            self#record
              [ "mode_annotations", mode_annotations
              ; "ret_mode_annotations", ret_mode_annotations
              ; "ret_type_constraint", ret_type_constraint
              ]

        method mode : mode -> 'res =
          fun x ->
            match x with
            | Mode a ->
              let a = self#string a in
              self#constr "Mode" [ a ]

        method modes : modes -> 'res = self#list (self#loc self#mode)

        method modality : modality -> 'res =
          fun x ->
            match x with
            | Modality a ->
              let a = self#string a in
              self#constr "Modality" [ a ]

        method modalities : modalities -> 'res = self#list (self#loc self#modality)
        method signature_items : signature_items -> 'res = self#list self#signature_item
        method signature : signature -> 'res = self#signature_items
      end

    class virtual ['ctx, 'res] lift_map_with_context =
      object (self)
        method virtual record : 'ctx -> (string * 'res) list -> 'res
        method virtual constr : 'ctx -> string -> 'res list -> 'res
        method virtual attributes : 'ctx -> attributes -> attributes * 'res
        method virtual core_type : 'ctx -> core_type -> core_type * 'res

        method
          virtual list
          : 'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a list -> 'a list * 'res

        method
          virtual loc
          : 'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a loc -> 'a loc * 'res

        method virtual location : 'ctx -> location -> location * 'res
        method virtual longident : 'ctx -> longident -> longident * 'res

        method
          virtual option
          : 'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a option -> 'a option * 'res

        method virtual signature_item : 'ctx -> signature_item -> signature_item * 'res
        method virtual string : 'ctx -> string -> string * 'res
        method virtual type_constraint : 'ctx -> type_constraint -> type_constraint * 'res

        method jkind_annotation_desc
          : 'ctx -> jkind_annotation_desc -> jkind_annotation_desc * 'res =
          fun ctx x ->
            match x with
            | Pjk_default -> Pjk_default, self#constr ctx "Pjk_default" []
            | Pjk_abbreviation a ->
              let a = self#loc self#longident ctx a in
              ( Pjk_abbreviation (Stdlib.fst a)
              , self#constr ctx "Pjk_abbreviation" [ Stdlib.snd a ] )
            | Pjk_operator (a, b) ->
              let a = self#jkind_annotation ctx a in
              let b = self#list (self#loc self#string) ctx b in
              ( Pjk_operator (Stdlib.fst a, Stdlib.fst b)
              , self#constr ctx "Pjk_operator" [ Stdlib.snd a; Stdlib.snd b ] )
            | Pjk_mod (a, b) ->
              let a = self#jkind_annotation ctx a in
              let b = self#modes ctx b in
              ( Pjk_mod (Stdlib.fst a, Stdlib.fst b)
              , self#constr ctx "Pjk_mod" [ Stdlib.snd a; Stdlib.snd b ] )
            | Pjk_with (a, b, c) ->
              let a = self#jkind_annotation ctx a in
              let b = self#core_type ctx b in
              let c = self#modalities ctx c in
              ( Pjk_with (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
              , self#constr ctx "Pjk_with" [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
            | Pjk_kind_of a ->
              let a = self#core_type ctx a in
              Pjk_kind_of (Stdlib.fst a), self#constr ctx "Pjk_kind_of" [ Stdlib.snd a ]
            | Pjk_product a ->
              let a = self#list self#jkind_annotation ctx a in
              Pjk_product (Stdlib.fst a), self#constr ctx "Pjk_product" [ Stdlib.snd a ]

        method jkind_annotation : 'ctx -> jkind_annotation -> jkind_annotation * 'res =
          fun ctx { pjka_loc; pjka_desc } ->
            let pjka_loc = self#location ctx pjka_loc in
            let pjka_desc = self#jkind_annotation_desc ctx pjka_desc in
            ( { pjka_loc = Stdlib.fst pjka_loc; pjka_desc = Stdlib.fst pjka_desc }
            , self#record
                ctx
                [ "pjka_loc", Stdlib.snd pjka_loc; "pjka_desc", Stdlib.snd pjka_desc ] )

        method jkind_declaration : 'ctx -> jkind_declaration -> jkind_declaration * 'res =
          fun ctx { pjkind_name; pjkind_manifest; pjkind_attributes; pjkind_loc } ->
            let pjkind_name = self#loc self#string ctx pjkind_name in
            let pjkind_manifest = self#option self#jkind_annotation ctx pjkind_manifest in
            let pjkind_attributes = self#attributes ctx pjkind_attributes in
            let pjkind_loc = self#location ctx pjkind_loc in
            ( { pjkind_name = Stdlib.fst pjkind_name
              ; pjkind_manifest = Stdlib.fst pjkind_manifest
              ; pjkind_attributes = Stdlib.fst pjkind_attributes
              ; pjkind_loc = Stdlib.fst pjkind_loc
              }
            , self#record
                ctx
                [ "pjkind_name", Stdlib.snd pjkind_name
                ; "pjkind_manifest", Stdlib.snd pjkind_manifest
                ; "pjkind_attributes", Stdlib.snd pjkind_attributes
                ; "pjkind_loc", Stdlib.snd pjkind_loc
                ] )

        method function_constraint
          : 'ctx -> function_constraint -> function_constraint * 'res =
          fun ctx { mode_annotations; ret_mode_annotations; ret_type_constraint } ->
            let mode_annotations = self#modes ctx mode_annotations in
            let ret_mode_annotations = self#modes ctx ret_mode_annotations in
            let ret_type_constraint =
              self#option self#type_constraint ctx ret_type_constraint
            in
            ( { mode_annotations = Stdlib.fst mode_annotations
              ; ret_mode_annotations = Stdlib.fst ret_mode_annotations
              ; ret_type_constraint = Stdlib.fst ret_type_constraint
              }
            , self#record
                ctx
                [ "mode_annotations", Stdlib.snd mode_annotations
                ; "ret_mode_annotations", Stdlib.snd ret_mode_annotations
                ; "ret_type_constraint", Stdlib.snd ret_type_constraint
                ] )

        method mode : 'ctx -> mode -> mode * 'res =
          fun ctx x ->
            match x with
            | Mode a ->
              let a = self#string ctx a in
              Mode (Stdlib.fst a), self#constr ctx "Mode" [ Stdlib.snd a ]

        method modes : 'ctx -> modes -> modes * 'res = self#list (self#loc self#mode)

        method modality : 'ctx -> modality -> modality * 'res =
          fun ctx x ->
            match x with
            | Modality a ->
              let a = self#string ctx a in
              Modality (Stdlib.fst a), self#constr ctx "Modality" [ Stdlib.snd a ]

        method modalities : 'ctx -> modalities -> modalities * 'res =
          self#list (self#loc self#modality)

        method signature_items : 'ctx -> signature_items -> signature_items * 'res =
          self#list self#signature_item

        method signature : 'ctx -> signature -> signature * 'res = self#signature_items
      end

    [@@@end]
  end

  module Jane_street_extensions0 (T : sig
      type 'a t
    end) =
  struct
    class type t = object
      method jkind_declaration : jkind_declaration T.t
      method jkind_annotation : jkind_annotation T.t
      method jkind_annotation_desc : jkind_annotation_desc T.t
      method function_body : function_body T.t
      method function_constraint : Pexp_function.Function_constraint.t T.t
      method type_constraint : type_constraint T.t
      method mode : Mode.t T.t
      method modes : Modes.t T.t
      method modality : Modality.t T.t
      method modalities : Modalities.t T.t
      method signature_items : signature_item list T.t
    end
  end

  module Jane_street_extensions0_ctx (T : sig
      type ('a, 'b) t
    end) =
  struct
    class type ['ctx] t = object
      method jkind_declaration : ('ctx, jkind_declaration) T.t
      method jkind_annotation : ('ctx, jkind_annotation) T.t
      method jkind_annotation_desc : ('ctx, jkind_annotation_desc) T.t
      method function_body : ('ctx, function_body) T.t
      method function_constraint : ('ctx, Pexp_function.Function_constraint.t) T.t
      method type_constraint : ('ctx, type_constraint) T.t
      method mode : ('ctx, Mode.t) T.t
      method modes : ('ctx, Modes.t) T.t
      method modality : ('ctx, Modality.t) T.t
      method modalities : ('ctx, Modalities.t) T.t
      method signature_items : ('ctx, signature_item list) T.t
    end
  end

  module Jane_street_extensions1 (T : sig
      type ('a, 'b) t
    end) =
  struct
    class type ['a] t = object
      method jkind_declaration : (jkind_declaration, 'a) T.t
      method jkind_annotation : (jkind_annotation, 'a) T.t
      method jkind_annotation_desc : (jkind_annotation_desc, 'a) T.t
      method function_body : (function_body, 'a) T.t
      method function_constraint : (Pexp_function.Function_constraint.t, 'a) T.t
      method type_constraint : (type_constraint, 'a) T.t
      method mode : (Mode.t, 'a) T.t
      method modes : (Modes.t, 'a) T.t
      method modality : (Modality.t, 'a) T.t
      method modalities : (Modalities.t, 'a) T.t
      method signature_items : (signature_item list, 'a) T.t
    end
  end

  module Jane_street_extensions1_ctx (T : sig
      type ('a, 'b, 'c) t
    end) =
  struct
    class type ['ctx, 'res] t = object
      method jkind_declaration : ('ctx, jkind_declaration, 'res) T.t
      method jkind_annotation : ('ctx, jkind_annotation, 'res) T.t
      method jkind_annotation_desc : ('ctx, jkind_annotation_desc, 'res) T.t
      method function_body : ('ctx, function_body, 'res) T.t
      method function_constraint : ('ctx, Pexp_function.Function_constraint.t, 'res) T.t
      method type_constraint : ('ctx, type_constraint, 'res) T.t
      method mode : ('ctx, Mode.t, 'res) T.t
      method modes : ('ctx, Modes.t, 'res) T.t
      method modality : ('ctx, Modality.t, 'res) T.t
      method modalities : ('ctx, Modalities.t, 'res) T.t
      method signature_items : ('ctx, signature_item list, 'res) T.t
    end
  end

  module Ts = struct
    module Map = struct
      type 'a t = 'a Ppxlib_traverse_builtins.T.map
    end

    module Iter = struct
      type 'a t = 'a Ppxlib_traverse_builtins.T.iter
    end

    module Fold = struct
      type ('a, 'b) t = ('b, 'a) Ppxlib_traverse_builtins.T.fold
    end

    module Fold_map = struct
      type ('a, 'b) t = ('b, 'a) Ppxlib_traverse_builtins.T.fold_map
    end

    module Map_with_context = struct
      type ('a, 'b) t = ('a, 'b) Ppxlib_traverse_builtins.T.map_with_context
    end

    module Lift = struct
      type ('a, 'b) t = ('a, 'b) Ppxlib_traverse_builtins.T.lift
    end

    module Lift_map_with_context = struct
      type ('a, 'b, 'c) t = ('a, 'b, 'c) Ppxlib_traverse_builtins.T.lift_map_with_context
    end
  end

  class virtual map =
    object
      inherit Ppxlib_ast.Ast.map
      inherit! Deriving_inline.map
    end

  class virtual iter =
    object
      inherit Ppxlib_ast.Ast.iter
      inherit! Deriving_inline.iter
    end

  class virtual ['ctx] fold =
    object
      inherit ['ctx] Ppxlib_ast.Ast.fold
      inherit! ['ctx] Deriving_inline.fold
    end

  class virtual ['ctx] fold_map =
    object
      inherit ['ctx] Ppxlib_ast.Ast.fold_map
      inherit! ['ctx] Deriving_inline.fold_map
    end

  class virtual ['ctx] map_with_context =
    object
      inherit ['ctx] Ppxlib_ast.Ast.map_with_context
      inherit! ['ctx] Deriving_inline.map_with_context
    end

  class virtual ['res] lift =
    object
      inherit ['res] Ppxlib_ast.Ast.lift
      inherit! ['res] Deriving_inline.lift
    end

  class virtual ['ctx, 'res] lift_map_with_context =
    object
      inherit ['ctx, 'res] Ppxlib_ast.Ast.lift_map_with_context
      inherit! ['ctx, 'res] Deriving_inline.lift_map_with_context
    end
end
