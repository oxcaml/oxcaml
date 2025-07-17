module Uid = Shape.Uid

module Type_shape = struct
  module Predef = struct
    type t =
      | Array
      | Bytes
      | Char
      | Extension_constructor
      | Float
      | Floatarray
      | Int
      | Int32
      | Int64
      | Lazy_t
      | Nativeint
      | String
      | Unboxed_float

    let to_string = function
      | Array -> "array"
      | Bytes -> "bytes"
      | Char -> "char"
      | Extension_constructor -> "extension_constructor"
      | Float -> "float"
      | Floatarray -> "floatarray"
      | Int -> "int"
      | Int32 -> "int32"
      | Int64 -> "int64"
      | Lazy_t -> "lazy_t"
      | Nativeint -> "nativeint"
      | String -> "string"
      | Unboxed_float -> "float#"

    let of_string = function
      | "array" -> Some Array
      | "bytes" -> Some Bytes
      | "char" -> Some Char
      | "extension_constructor" -> Some Extension_constructor
      | "float" -> Some Float
      | "float#" -> Some Unboxed_float
      | "floatarray" -> Some Floatarray
      | "int" -> Some Int
      | "int32" -> Some Int32
      | "int64" -> Some Int64
      | "lazy_t" -> Some Lazy_t
      | "nativeint" -> Some Nativeint
      | "string" -> Some String
      | _ -> None
  end

  type t =
    | Ts_constr of (Uid.t * Path.t) * t list
    | Ts_tuple of t list
    | Ts_var of string option
    | Ts_predef of Predef.t * t list
    | Ts_other

  (* Similarly to [value_kind], we track a set of visited types to avoid cycles
     in the lookup and we, additionally, carry a maximal depth for the recursion.
     We allow a deeper bound than [value_kind]. *)
  (* CR sspies: Consider additionally adding a max size for the set of visited types.
     Also consider reverting to the original value kind depth limit (although 2
     seems low). *)
  let rec of_type_expr_go ~visited ~depth (expr : Types.type_expr) uid_of_path =
    let[@inline] cannot_proceed () =
      Numbers.Int.Set.mem (Types.get_id expr) visited || depth > 10
    in
    if cannot_proceed ()
    then Ts_other
    else
      let visited = Numbers.Int.Set.add (Types.get_id expr) visited in
      let depth = depth + 1 in
      let desc = Types.get_desc expr in
      let of_expr_list (exprs : Types.type_expr list) =
        List.map
          (fun expr -> of_type_expr_go ~depth ~visited expr uid_of_path)
          exprs
      in
      match[@warning "-fragile-match"] desc with
      (* CR sspies: Extend this match to handle more type constructor cases in
         subsequent PRs. *)
      | Tconstr (path, constrs, _abbrev_memo) -> (
        match Predef.of_string (Path.name path) with
        | Some predef -> Ts_predef (predef, of_expr_list constrs)
        | None -> (
          match uid_of_path path with
          | Some uid -> Ts_constr ((uid, path), of_expr_list constrs)
          | None -> Ts_other))
      | Ttuple exprs -> Ts_tuple (of_expr_list (List.map snd exprs))
      | Tvar { name; _ } -> Ts_var name
      | Tpoly (type_expr, []) ->
        of_type_expr_go ~depth ~visited type_expr uid_of_path
      | _ -> Ts_other

  let of_type_expr (expr : Types.type_expr) uid_of_path =
    of_type_expr_go ~visited:Numbers.Int.Set.empty ~depth:0 expr uid_of_path

  let rec print ppf = function
    (* CR sspies: We should figure out pretty printing for shapes. For now, this
       verbose non-boxed version should be fine. *)
    | Ts_predef (predef, shapes) ->
      Format.fprintf ppf "%s (%a)" (Predef.to_string predef)
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_constr ((uid, path), shapes) ->
      Format.fprintf ppf "Ts_constr uid=%a path=%a (%a)" Uid.print uid
        Path.print path
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_tuple shapes ->
      Format.fprintf ppf "Ts_tuple (%a)"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_var name ->
      Format.fprintf ppf "Ts_var (%a)"
        (fun ppf opt -> Format.pp_print_option Format.pp_print_string ppf opt)
        name
    | Ts_other -> Format.fprintf ppf "Ts_other"

  (* CR sspies: This is a hacky "solution" to do type variable substitution in
     type expression shapes. In subsequent PRs, this code should be changed to
     use the shape mechanism instead. *)
  let rec replace_tvar t ~(pairs : (t * t) list) =
    match
      List.find_map
        (fun (from, to_) -> if t = from then Some to_ else None)
        pairs
    with
    | Some new_type -> new_type
    | None -> (
      match t with
      | Ts_constr (uid, shape_list) ->
        Ts_constr (uid, List.map (replace_tvar ~pairs) shape_list)
      | Ts_tuple shape_list ->
        Ts_tuple (List.map (replace_tvar ~pairs) shape_list)
      | Ts_var _ | Ts_predef _ | Ts_other -> t)

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = Stdlib.compare

    let print = print

    let hash = Hashtbl.hash

    let equal (x : t) y = x = y

    let output = Misc.output_of_print print
  end)
end

module Type_decl_shape = struct
  type 'a complex_constructor =
    { name : string;
      args : 'a complex_constructor_argument list
    }

  and 'a complex_constructor_argument =
    { field_name : string option;
      field_value : 'a
    }

  type tds =
    | Tds_variant of
        { simple_constructors : string list;
          (* CR sspies: Deduplicate these cases once type shapes have reached
             a more stable form. *)
          complex_constructors : Type_shape.t complex_constructor list
        }
    | Tds_record of (string * Type_shape.t) list
    | Tds_alias of Type_shape.t
    | Tds_other

  type t =
    { path : Path.t;
      definition : tds;
      type_params : Type_shape.t list
    }

  let complex_constructor_map f { name; args } =
    let args =
      List.map
        (fun { field_name; field_value } ->
          { field_name; field_value = f field_value })
        args
    in
    { name; args }

  let get_constructor_args (cstr_args : Types.constructor_declaration)
      uid_of_path =
    match cstr_args.cd_args with
    | Cstr_tuple list ->
      List.map
        (fun ({ ca_type = type_expr; _ } : Types.constructor_argument) ->
          { field_name = None;
            field_value = Type_shape.of_type_expr type_expr uid_of_path
          })
        list
    | Cstr_record list ->
      List.map
        (fun (lbl : Types.label_declaration) ->
          { field_name = Some (Ident.name lbl.ld_id);
            field_value = Type_shape.of_type_expr lbl.ld_type uid_of_path
          })
        list

  let is_empty_constructor_list (cstr_args : Types.constructor_declaration) =
    match cstr_args.cd_args with
    | Cstr_tuple [] -> true
    | Cstr_tuple (_ :: _)
    | Cstr_record _
    (* Records are not allowed to have an empty list of fields.*) ->
      false

  let of_type_declaration path (type_declaration : Types.type_declaration)
      uid_of_path =
    let definition =
      match type_declaration.type_manifest with
      | Some type_expr ->
        Tds_alias (Type_shape.of_type_expr type_expr uid_of_path)
      | None -> (
        match type_declaration.type_kind with
        | Type_variant (cstr_list, _variant_repr, _unsafe_mode_crossing) ->
          let simple_constructors, complex_constructors =
            List.partition_map
              (fun (cstr : Types.constructor_declaration) ->
                let name = Ident.name cstr.cd_id in
                match is_empty_constructor_list cstr with
                | true -> Left name
                | false ->
                  Right { name; args = get_constructor_args cstr uid_of_path })
              cstr_list
          in
          Tds_variant { simple_constructors; complex_constructors }
        | Type_record (lbl_list, record_repr, _unsafe_mode_crossing) -> (
          match record_repr with
          | Record_boxed _ ->
            Tds_record
              (List.map
                 (fun (lbl : Types.label_declaration) ->
                   ( Ident.name lbl.ld_id,
                     Type_shape.of_type_expr lbl.ld_type uid_of_path ))
                 lbl_list)
          | Record_float ->
            Tds_record
              (List.map
                 (fun (lbl : Types.label_declaration) ->
                   Ident.name lbl.ld_id, Type_shape.Ts_predef (Unboxed_float, []))
                 lbl_list)
          | Record_mixed _ ->
            (* CR sspies: Mixed records are currently not supported. *)
            Tds_other
          | Record_inlined _ | Record_unboxed | Record_ufloat -> Tds_other)
        | Type_abstract _ -> Tds_other
        | Type_open -> Tds_other
        | Type_record_unboxed_product _ ->
          (* CR sspies: Unboxed products are currently not supported.*)
          Tds_other)
    in
    let type_params =
      List.map
        (fun type_expr -> Type_shape.of_type_expr type_expr uid_of_path)
        type_declaration.type_params
    in
    { path; definition; type_params }

  let print_one_entry print_value ppf { field_name; field_value } =
    match field_name with
    | Some name ->
      Format.fprintf ppf "%a=%a" Format.pp_print_string name print_value
        field_value
    | None -> Format.fprintf ppf "%a" print_value field_value

  let print_complex_constructor print_value ppf { name; args } =
    Format.fprintf ppf "(%a: %a)" Format.pp_print_string name
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         (print_one_entry print_value))
      args

  let print_field ppf (name, shape) =
    Format.fprintf ppf "(%a: %a)" Format.pp_print_string name Type_shape.print
      shape

  let print_tds ppf = function
    (* CR sspies: We should figure out pretty printing for shapes. For now, this
       verbose non-boxed version should be fine. *)
    | Tds_variant { simple_constructors; complex_constructors } ->
      Format.fprintf ppf
        "Tds_variant simple_constructors=%a complex_constructors=%a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        simple_constructors
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           (print_complex_constructor Type_shape.print))
        complex_constructors
    | Tds_record field_list ->
      Format.fprintf ppf "Tds_record fields=%a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print_field)
        field_list
    | Tds_alias type_shape ->
      Format.fprintf ppf "Tds_alias %a" Type_shape.print type_shape
    | Tds_other -> Format.fprintf ppf "Tds_other"

  let print ppf t =
    Format.fprintf ppf "path=%a, definition=(%a)" Path.print t.path print_tds
      t.definition

  let map_snd f list = List.map (fun (fst, snd) -> fst, f snd) list

  (* CR sspies: This is a hacky "solution" to do type variable substitution in
     type declaration shapes. In subsequent PRs, this code should be changed to
     use the shape mechanism instead. *)
  let replace_tvar (t : t) (shapes : Type_shape.t list) =
    match List.length t.type_params == List.length shapes with
    | true ->
      let replace_tvar =
        Type_shape.replace_tvar ~pairs:(List.combine t.type_params shapes)
      in
      let ret =
        { type_params = [];
          path = t.path;
          definition =
            (match t.definition with
            | Tds_variant { simple_constructors; complex_constructors } ->
              Tds_variant
                { simple_constructors;
                  complex_constructors =
                    List.map
                      (complex_constructor_map replace_tvar)
                      complex_constructors
                }
            | Tds_record field_list ->
              Tds_record (map_snd replace_tvar field_list)
            | Tds_alias type_shape -> Tds_alias (replace_tvar type_shape)
            | Tds_other -> Tds_other)
        }
      in
      ret
    | false -> { type_params = []; path = t.path; definition = Tds_other }
end

type binder_shape =
  { type_shape : Type_shape.t;
    type_sort : Jkind_types.Sort.Const.t
  }

let (all_type_decls : Type_decl_shape.t Uid.Tbl.t) = Uid.Tbl.create 16

let (all_type_shapes : binder_shape Uid.Tbl.t) = Uid.Tbl.create 16

let add_to_type_decls path (type_decl : Types.type_declaration) uid_of_path =
  let type_decl_shape =
    Type_decl_shape.of_type_declaration path type_decl uid_of_path
  in
  Uid.Tbl.add all_type_decls type_decl.type_uid type_decl_shape

let add_to_type_shapes var_uid type_expr sort uid_of_path =
  let type_shape = Type_shape.of_type_expr type_expr uid_of_path in
  Uid.Tbl.add all_type_shapes var_uid { type_shape; type_sort = sort }

let tuple_to_string (strings : string list) =
  match strings with
  | [] -> ""
  | hd :: [] -> hd
  | _ :: _ :: _ -> "(" ^ String.concat " * " strings ^ ")"

let type_arg_list_to_string (strings : string list) =
  match strings with
  | [] -> ""
  | hd :: [] -> hd ^ " "
  | _ :: _ :: _ -> "(" ^ String.concat ", " strings ^ ") "

let find_in_type_decls (type_uid : Uid.t) =
  Uid.Tbl.find_opt all_type_decls type_uid

let rec type_name (type_shape : Type_shape.t) =
  match type_shape with
  | Ts_predef (predef, shapes) ->
    type_arg_list_to_string (List.map type_name shapes)
    ^ Type_shape.Predef.to_string predef
  | Ts_other -> "unknown"
  | Ts_tuple shapes -> tuple_to_string (List.map type_name shapes)
  | Ts_var name -> "'" ^ Option.value name ~default:"?"
  | Ts_constr ((type_uid, _type_path), shapes) -> (
    match[@warning "-fragile-match"] find_in_type_decls type_uid with
    | None -> "unknown"
    | Some { definition = Tds_other; _ } -> "unknown"
    | Some type_decl_shape ->
      (* We have found type instantiation shapes [shapes] and a typing
         declaration shape [type_decl_shape]. *)
      let type_decl_shape =
        Type_decl_shape.replace_tvar type_decl_shape shapes
      in
      let args = type_arg_list_to_string (List.map type_name shapes) in
      let name = Path.name type_decl_shape.path in
      args ^ name)
