(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Ulysse Gérard, Thomas Refis, Tarides                    *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Layout = Jkind_types.Sort.Const
type base_layout = Jkind_types.Sort.base

module Uid = struct
  type t =
    | Compilation_unit of string
    | Item of { comp_unit: string; id: int; from: Unit_info.intf_or_impl }
    | Internal
    | Predef of string
    | Unboxed_version of t

  include Identifiable.Make(struct
    type nonrec t = t

    let rec compare (x : t) y =
      match x, y with
      | Compilation_unit s1, Compilation_unit s2 -> String.compare s1 s2
      | Item c1, Item c2 ->
        let c = Int.compare c1.id c2.id in
        let c =
          if c <> 0 then c else String.compare c1.comp_unit c2.comp_unit
        in
        if c <> 0 then c else Stdlib.compare c1.from c2.from
      | Internal, Internal -> 0
      | Predef s1, Predef s2 -> String.compare s1 s2
      | Unboxed_version t1, Unboxed_version t2 -> compare t1 t2
      | Compilation_unit _,
        (Item _ | Internal | Predef _ | Unboxed_version _) ->
        -1
      | Item _, (Internal | Predef _| Unboxed_version _) -> -1
      | Internal, (Predef _ | Unboxed_version _) -> -1
      | Predef _, Unboxed_version _ -> -1
      | (Item _ | Internal | Predef _ | Unboxed_version _),
        Compilation_unit _ ->
        1
      | (Internal | Predef _ | Unboxed_version _), Item _ -> 1
      | (Predef _ | Unboxed_version _), Internal -> 1
      | Unboxed_version _, Predef _ -> 1

    let equal x y = compare x y = 0

    let hash (x : t) = Hashtbl.hash x

    let pp_intf_or_impl fmt = function
      | Unit_info.Intf -> Format.pp_print_string fmt "[intf]"
      | Unit_info.Impl -> ()

    let rec print fmt = function
      | Internal -> Format.pp_print_string fmt "<internal>"
      | Predef name -> Format.fprintf fmt "<predef:%s>" name
      | Compilation_unit s -> Format.pp_print_string fmt s
      | Item { comp_unit; id; from } ->
          Format.fprintf fmt "%a%s.%d" pp_intf_or_impl from comp_unit id
      | Unboxed_version t -> Format.fprintf fmt "%a#" print t

    let output oc t =
      let fmt = Format.formatter_of_out_channel oc in
      print fmt t
  end)

  let id = ref (-1)

  let reinit () = id := (-1)

  let mk  ~current_unit =
      let comp_unit, from =
        let open Unit_info in
        match current_unit with
        | None -> "", Impl
        | Some ui ->
          Compilation_unit.full_path_as_string (modname ui), kind ui
      in
      incr id;
      Item { comp_unit; id = !id; from }

  let of_compilation_unit_id id =
    Compilation_unit (id |> Compilation_unit.full_path_as_string)

  let of_compilation_unit_name name =
    Compilation_unit (name |> Compilation_unit.Name.to_string)

  let of_predef_id id =
    if not (Ident.is_predef id) then
      Misc.fatal_errorf "Types.Uid.of_predef_id %S" (Ident.name id);
    Predef (Ident.name id)

  let internal_not_actually_unique = Internal

  let unboxed_version t =
    match t with
    | Unboxed_version _ ->
      Misc.fatal_error "Shape.unboxed_version"
    | _ -> Unboxed_version t

  let for_actual_declaration = function
    | Item _ -> true
    | _ -> false
end

module Sig_component_kind = struct
  type t =
    | Value
    | Type
    | Constructor
    | Label
    | Unboxed_label
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  let to_string = function
    | Value -> "value"
    | Type -> "type"
    | Constructor -> "constructor"
    | Label -> "label"
    | Unboxed_label -> "unboxed label"
    | Module -> "module"
    | Module_type -> "module type"
    | Extension_constructor -> "extension constructor"
    | Class -> "class"
    | Class_type -> "class type"

  let can_appear_in_types = function
    | Value
    | Extension_constructor ->
        false
    | Type
    | Constructor
    | Label
    | Unboxed_label
    | Module
    | Module_type
    | Class
    | Class_type ->
        true

  let rank = function
    | Value -> 0
    | Type -> 1
    | Module -> 2
    | Module_type -> 3
    | Extension_constructor -> 4
    | Class -> 5
    | Class_type -> 6
    | Constructor -> 7
    | Label -> 8
    | Unboxed_label -> 9

  let compare a b =
    let a = rank a in
    let b = rank b in
    Int.compare a b
end

module Item = struct
  module T = struct
    type t = string * Sig_component_kind.t

    let compare (sa, ka) (sb, kb) =
      let c = String.compare sa sb in
      if c <> 0 then c
      else (Sig_component_kind.compare ka kb)

    let name (name, _) = name
    let kind (_, kind) = kind

    let make str ns = str, ns

    let value id = Ident.name id, Sig_component_kind.Value
    let type_ id = Ident.name id, Sig_component_kind.Type
    let constr id = Ident.name id, Sig_component_kind.Constructor
    let label id = Ident.name id, Sig_component_kind.Label
    let unboxed_label id = Ident.name id, Sig_component_kind.Unboxed_label
    let module_ id = Ident.name id, Sig_component_kind.Module
    let module_type id = Ident.name id, Sig_component_kind.Module_type
    let extension_constructor id =
      Ident.name id, Sig_component_kind.Extension_constructor
    let class_ id =
      Ident.name id, Sig_component_kind.Class
    let class_type id =
      Ident.name id, Sig_component_kind.Class_type

    let print fmt (name, ns) =
      Format.fprintf fmt "%S[%s]"
        name
        (Sig_component_kind.to_string ns)

    let hash x = Hashtbl.hash x
  end

  include T

  let is_constructor (_, kind) = match kind with
    | Sig_component_kind.Constructor -> true
    | _ -> false

  let is_label (_, kind) = match kind with
    | Sig_component_kind.Label -> true
    | _ -> false

  let is_unboxed_label (_, kind) = match kind with
    | Sig_component_kind.Unboxed_label -> true
    | _ -> false

  module Map = Map.Make(T)
end

module Predef = struct
  type simd_vec_split =
      (* 128 bit *)
      | Int8x16
      | Int16x8
      | Int32x4
      | Int64x2
      | Float32x4
      | Float64x2
      (* 256 bit *)
      | Int8x32
      | Int16x16
      | Int32x8
      | Int64x4
      | Float32x8
      | Float64x4
      (* 512 bit *)
      | Int8x64
      | Int16x32
      | Int32x16
      | Int64x8
      | Float32x16
      | Float64x8

    type unboxed =
      | Unboxed_float
      | Unboxed_float32
      | Unboxed_nativeint
      | Unboxed_int64
      | Unboxed_int32
      | Unboxed_simd of simd_vec_split

    type t =
      | Array
      | Bytes
      | Char
      | Extension_constructor
      | Float
      | Float32
      | Floatarray
      | Int
      | Int32
      | Int64
      | Lazy_t
      | Nativeint
      | String
      | Simd of simd_vec_split
      | Exception
      | Unboxed of unboxed

    let simd_vec_split_to_string : simd_vec_split -> string = function
      | Int8x16 -> "int8x16"
      | Int16x8 -> "int16x8"
      | Int32x4 -> "int32x4"
      | Int64x2 -> "int64x2"
      | Float32x4 -> "float32x4"
      | Float64x2 -> "float64x2"
      | Int8x32 -> "int8x32"
      | Int16x16 -> "int16x16"
      | Int32x8 -> "int32x8"
      | Int64x4 -> "int64x4"
      | Float32x8 -> "float32x8"
      | Float64x4 -> "float64x4"
      | Int8x64 -> "int8x64"
      | Int16x32 -> "int16x32"
      | Int32x16 -> "int32x16"
      | Int64x8 -> "int64x8"
      | Float32x16 -> "float32x16"
      | Float64x8 -> "float64x8"

    let simd_vec_split_to_byte_size : simd_vec_split -> int = function
      | Int8x16
      | Int16x8
      | Int32x4
      | Int64x2
      | Float32x4
      | Float64x2 -> 16
      | Int8x32
      | Int16x16
      | Int32x8
      | Int64x4
      | Float32x8
      | Float64x4 -> 32
      | Int8x64
      | Int16x32
      | Int32x16
      | Int64x8
      | Float32x16
      | Float64x8 -> 64

    (* name of the type without the hash *)
    let unboxed_to_string (u : unboxed) =
      match u with
      | Unboxed_float -> "float"
      | Unboxed_float32 -> "float32"
      | Unboxed_nativeint -> "nativeint"
      | Unboxed_int64 -> "int64"
      | Unboxed_int32 -> "int32"
      | Unboxed_simd s -> simd_vec_split_to_string s

    let to_string : t -> string = function
      | Array -> "array"
      | Bytes -> "bytes"
      | Char -> "char"
      | Extension_constructor -> "extension_constructor"
      | Float -> "float"
      | Float32 -> "float32"
      | Floatarray -> "floatarray"
      | Int -> "int"
      | Int32 -> "int32"
      | Int64 -> "int64"
      | Lazy_t -> "lazy_t"
      | Nativeint -> "nativeint"
      | String -> "string"
      | Simd s -> simd_vec_split_to_string s
      | Exception -> "exn"
      | Unboxed u -> unboxed_to_string u ^ "#"

    let print fmt t =
      Format.pp_print_string fmt (to_string t)

    let simd_vec_split_to_layout (b : simd_vec_split) : Jkind_types.Sort.base =
      match b with
      | Int8x16 -> Vec128
      | Int16x8 -> Vec128
      | Int32x4 -> Vec128
      | Int64x2 -> Vec128
      | Float32x4 -> Vec128
      | Float64x2 -> Vec128
      | Int8x32 -> Vec256
      | Int16x16 -> Vec256
      | Int32x8 -> Vec256
      | Int64x4 -> Vec256
      | Float32x8 -> Vec256
      | Float64x4 -> Vec256
      | Int8x64 -> Vec512
      | Int16x32 -> Vec512
      | Int32x16 -> Vec512
      | Int64x8 -> Vec512
      | Float32x16 -> Vec512
      | Float64x8 -> Vec512

    let unboxed_type_to_base_layout (b : unboxed) : base_layout =
      match b with
      | Unboxed_float -> Float64
      | Unboxed_float32 -> Float32
      | Unboxed_nativeint -> Word
      | Unboxed_int64 -> Bits64
      | Unboxed_int32 -> Bits32
      | Unboxed_simd s -> simd_vec_split_to_layout s

    let to_layout : t -> Layout.t = function
      | Array -> Base Value
      | Bytes -> Base Value
      | Char -> Base Value
      | Extension_constructor -> Base Value
      | Float -> Base Value
      | Float32 -> Base Value
      | Floatarray -> Base Value
      | Int -> Base Value
      | Int32 -> Base Value
      | Int64 -> Base Value
      | Lazy_t -> Base Value
      | Nativeint -> Base Value
      | String -> Base Value
      | Simd _ -> Base Value
      | Exception -> Base Value
      | Unboxed u -> Base (unboxed_type_to_base_layout u)

    let equal_simd_vec_split s1 s2 =
      match s1, s2 with
      | Int8x16, Int8x16
      | Int16x8, Int16x8
      | Int32x4, Int32x4
      | Int64x2, Int64x2
      | Float32x4, Float32x4
      | Float64x2, Float64x2
      | Int8x32, Int8x32
      | Int16x16, Int16x16
      | Int32x8, Int32x8
      | Int64x4, Int64x4
      | Float32x8, Float32x8
      | Float64x4, Float64x4
      | Int8x64, Int8x64
      | Int16x32, Int16x32
      | Int32x16, Int32x16
      | Int64x8, Int64x8
      | Float32x16, Float32x16
      | Float64x8, Float64x8 -> true
      | (Int8x16 | Int16x8 | Int32x4 | Int64x2 | Float32x4 | Float64x2
        | Int8x32 | Int16x16 | Int32x8 | Int64x4 | Float32x8 | Float64x4
        | Int8x64 | Int16x32 | Int32x16 | Int64x8 | Float32x16 | Float64x8), _
        -> false

    let equal_unboxed u1 u2 =
      match u1, u2 with
      | Unboxed_float, Unboxed_float
      | Unboxed_float32, Unboxed_float32
      | Unboxed_nativeint, Unboxed_nativeint
      | Unboxed_int64, Unboxed_int64
      | Unboxed_int32, Unboxed_int32 -> true
      | Unboxed_simd s1, Unboxed_simd s2 -> equal_simd_vec_split s1 s2
      | (Unboxed_float | Unboxed_float32 | Unboxed_nativeint
        | Unboxed_int64 | Unboxed_int32 | Unboxed_simd _), _ -> false

    let equal p1 p2 =
      match p1, p2 with
      | Array, Array
      | Bytes, Bytes
      | Char, Char
      | Extension_constructor, Extension_constructor
      | Float, Float
      | Float32, Float32
      | Floatarray, Floatarray
      | Int, Int
      | Int32, Int32
      | Int64, Int64
      | Lazy_t, Lazy_t
      | Nativeint, Nativeint
      | String, String -> true
      | Simd s1, Simd s2 -> equal_simd_vec_split s1 s2
      | Exception, Exception -> true
      | Unboxed u1, Unboxed u2 -> equal_unboxed u1 u2
      | (Array | Bytes | Char | Extension_constructor | Float | Float32
        | Floatarray | Int | Int32 | Int64 | Lazy_t | Nativeint | String
        | Simd _ | Exception | Unboxed _), _ -> false
end

type var = Ident.t
type t = { hash:int; uid: Uid.t option; desc: desc; approximated: bool }
and desc =
  | Var of var
  | Abs of var * t
  | App of t * t
  | Struct of t Item.Map.t
  | Alias of t
  | Leaf
  | Proj of t * Item.t
  | Comp_unit of string
  | Error of string

  (* constructors for types  *)
  | Constr of Ident.t * t list
  | Tuple of t list
  | Unboxed_tuple of t list
  | Predef of Predef.t * t list
  | Arrow of t * t
  | Poly_variant of t poly_variant_constructors
  | Mu of t
  | Rec_var of int

  (* constructors for type declarations *)
  | Variant of (t * Layout.t) complex_constructors
  | Variant_unboxed of
    { name : string;
      variant_uid : Uid.t option;
      arg_name : string option;
      arg_uid: Uid.t option;
      arg_shape : t;
      arg_layout : Layout.t
    }
  | Record of
      { fields : (string * Uid.t option * t * Layout.t) list;
        kind : record_kind
      }
  | Mutrec of t Ident.Map.t
  | Proj_decl of t * Ident.t

and 'a poly_variant_constructors = 'a poly_variant_constructor list

and 'a poly_variant_constructor =
  { pv_constr_name : string;
    pv_constr_args : 'a list
  }

and record_kind =
  | Record_unboxed
  | Record_unboxed_product
  | Record_boxed
  | Record_mixed of mixed_product_shape
  | Record_floats

and 'a complex_constructors = 'a complex_constructor list

and 'a complex_constructor =
  { name : string;
    constr_uid: Uid.t option;
    kind : constructor_representation;
    args : 'a complex_constructor_argument list
  }

and 'a complex_constructor_argument =
  { field_name : string option;
    field_uid: Uid.t option;
    field_value : 'a
  }

and constructor_representation = mixed_product_shape

and mixed_product_shape = Layout.t array

let poly_variant_constructors_map f pvs =
  List.map
    (fun pv -> { pv with pv_constr_args = List.map f pv.pv_constr_args })
    pvs

let complex_constructor_map f { name; constr_uid; kind; args } =
  let args =
    List.map
      (fun { field_name; field_uid; field_value } ->
        { field_name; field_uid; field_value = f field_value })
      args
  in
  { name; constr_uid; kind; args }

let complex_constructors_map f = List.map (complex_constructor_map f)

let equal_complex_constructor_arguments eq
    { field_name = field_name1; field_value = field_value1 }
    { field_name = field_name2; field_value = field_value2 } =
  Option.equal String.equal field_name1 field_name2 &&
  eq field_value1 field_value2

let equal_complex_constructor eq
    { name = name1; kind = kind1; args = args1 }
    { name = name2; kind = kind2; args = args2 } =
  String.equal name1 name2 &&
  Misc.Stdlib.Array.equal Layout.equal kind1 kind2 &&
  List.equal (equal_complex_constructor_arguments eq) args1 args2

let rec equal_desc0 d1 d2 =
  match d1, d2 with
  | Var v1, Var v2 -> Ident.equal v1 v2
  | Alias a1, Alias a2 -> equal a1 a2
  | Error s1, Error s2 -> String.equal s1 s2
  | Abs (v1, t1), Abs (v2, t2) ->
    if Ident.equal v1 v2 then equal t1 t2
    else false
  | App (v1, t1), App (v2, t2) ->
    if not (equal t1 t2) then false
    else equal v1 v2
  | Leaf, Leaf -> true
  | Mu (t1_body), Mu (t2_body) ->
    equal t1_body t2_body
  | Rec_var i1, Rec_var i2 -> Int.equal i1 i2
  | Struct t1, Struct t2 ->
    Item.Map.equal equal t1 t2
  | Proj (t1, i1), Proj (t2, i2) ->
    if Item.compare i1 i2 <> 0 then false
    else equal t1 t2
  | Comp_unit c1, Comp_unit c2 -> String.equal c1 c2
  | Constr (c1, ts1), Constr (c2, ts2) ->
    Ident.equal c1 c2
    && List.equal equal ts1 ts2
  | Mutrec t1, Mutrec t2 ->
    Ident.Map.equal equal t1 t2
  | Proj_decl (t1, i1), Proj_decl (t2, i2) ->
    if Ident.equal i1 i2 then
      equal t1 t2
    else false
  | Tuple t1, Tuple t2
  | Unboxed_tuple t1, Unboxed_tuple t2 ->
    List.equal equal t1 t2
  | Predef (p1, ts1), Predef (p2, ts2) ->
    Predef.equal p1 p2 && List.equal equal ts1 ts2
  | Arrow (t1, t1'), Arrow (t2, t2') ->
    equal t1 t2 && equal t1' t2'
  | Poly_variant pvs1, Poly_variant pvs2 ->
    List.equal equal_poly_variant_constructor pvs1 pvs2
  | Variant c1, Variant c2 ->
    List.equal
         (equal_complex_constructor (fun (t1, l1) (t2, l2) ->
           equal t1 t2 && Layout.equal l1 l2))
         c1 c2
  | Variant_unboxed c1, Variant_unboxed c2 ->
    String.equal c1.name c2.name
    && Option.equal String.equal c1.arg_name c2.arg_name
    && equal c1.arg_shape c2.arg_shape
    && Layout.equal c1.arg_layout c2.arg_layout
  | Record r1, Record r2 ->
    equal_record_kind r1.kind r2.kind
    && List.equal equal_field r1.fields r2.fields
  | (Var _ | Abs _ | App _ | Struct _ | Leaf | Proj _ | Comp_unit _
    | Alias _ | Error _ | Variant _ | Variant_unboxed _ | Record _
    | Predef _ | Arrow _ | Poly_variant _ | Tuple _ | Unboxed_tuple _
    | Constr _ | Mutrec _ | Proj_decl _ | Mu _ | Rec_var _), _
    -> false

and equal_desc d1 d2 =
  d1 == d2 || equal_desc0 d1 d2

and equal t1 t2 =
  if t1.hash <> t2.hash then false
  else if not (Bool.equal t1.approximated t2.approximated) then false
  else if not (Option.equal Uid.equal t1.uid t2.uid) then false
  else equal_desc t1.desc t2.desc

and equal_record_kind k1 k2 =
  match k1, k2 with
  | Record_unboxed, Record_unboxed -> true
  | Record_unboxed_product, Record_unboxed_product -> true
  | Record_boxed, Record_boxed -> true
  | Record_mixed lys1, Record_mixed lys2 ->
    Misc.Stdlib.Array.equal Layout.equal lys1 lys2
  | Record_floats, Record_floats -> true
  | (Record_unboxed | Record_unboxed_product | Record_boxed | Record_mixed _ |
     Record_floats), _
    -> false

and equal_field (s1, uid1, sh1, ly1) (s2, uid2, sh2, ly2) =
  String.equal s1 s2 &&
  Option.equal Uid.equal uid1 uid2 &&
  equal sh1 sh2 &&
  Layout.equal ly1 ly2

and equal_poly_variant_constructor
  { pv_constr_name = name1; pv_constr_args = args1 }
  { pv_constr_name = name2; pv_constr_args = args2 } =
  String.equal name1 name2 &&
  List.equal equal args1 args2

let rec print fmt t =
  let print_uid_opt =
    Format.pp_print_option (fun fmt -> Format.fprintf fmt "<%a>" Uid.print)
  in
  let print_nested fmt t =
    match t.desc with
    | Var _ | Leaf | Rec_var _ | Comp_unit _ | Error _ | Predef (_, []) ->
      print fmt t
    | _ -> Format.fprintf fmt "(@[%a@])" print t
  in
  let rec aux fmt { uid; desc; hash = _ } =
    match desc with
    | Var id ->
        Format.fprintf fmt "%s%a" (Ident.name id) print_uid_opt uid
    | Abs (id, t) ->
        let rec collect_idents = function
          | { uid = None; desc = Abs(id, t) } ->
            let (ids, body) = collect_idents t in
            id :: ids, body
          | body ->
            ([], body)
        in
        let (other_idents, body) = collect_idents t in
        let pp_idents fmt idents =
          let idents_names = List.map Ident.name idents in
          let pp_sep fmt () = Format.fprintf fmt ",@ " in
          Format.pp_print_list ~pp_sep Format.pp_print_string fmt idents_names
        in
        Format.fprintf fmt "Abs@[%a@,(@[%a,@ @[%a@]@])@]"
          print_uid_opt uid pp_idents (id :: other_idents) aux body
    | App (t1, t2) ->
        Format.fprintf fmt "@[%a(@,%a)%a@]" aux t1 aux t2 print_uid_opt uid
    | Leaf ->
        Format.fprintf fmt "<%a>" (Format.pp_print_option Uid.print) uid
    | Mu (t_body) ->
      Format.fprintf fmt "Rec@[%a %a@]"
        print_uid_opt uid
        print_nested t_body
    | Rec_var id ->
      Format.fprintf fmt "#%d%a"
      id
      print_uid_opt uid
    | Proj (t, item) ->
        begin match uid with
        | None ->
            Format.fprintf fmt "@[%a@ .@ %a@]"
              aux t
              Item.print item
        | Some uid ->
            Format.fprintf fmt "@[(%a@ .@ %a)<%a>@]"
              aux t
              Item.print item
              Uid.print uid
        end
    | Comp_unit name -> Format.fprintf fmt "CU %s" name
    | Struct map ->
        let print_map fmt =
          Item.Map.iter (fun item t ->
              Format.fprintf fmt "@[<hv 2>%a ->@ %a;@]@,"
                Item.print item
                aux t
            )
        in
        if Item.Map.is_empty map then
          Format.fprintf fmt "@[<hv>{%a}@]" print_uid_opt uid
        else
          Format.fprintf fmt "{@[<v>%a@,%a@]}" print_uid_opt uid print_map map
    | Alias t ->
        Format.fprintf fmt "Alias@[(@[<v>%a@,%a@])@]" print_uid_opt uid aux t
    | Error s ->
        Format.fprintf fmt "Error %s" s
    | Constr (id, args) ->
        Format.fprintf fmt "@[%a@ %a@]"
          Ident.print id
          (Format.pp_print_list print_nested) args
    | Tuple shapes ->
      Format.fprintf fmt "@[%a@]"
        (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " * ")
            print_nested)
        shapes
    | Unboxed_tuple shapes ->
      Format.fprintf fmt "Unboxed_tuple (%a)"
        (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
            print)
        shapes
    | Predef (predef, args) ->
      Format.fprintf fmt "%a%a"
        Predef.print predef
        (fun fmt -> function
          | [] -> ()
          | args -> Format.fprintf fmt "(@[%a@])"
              (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",@ ")
                print) args) args
    | Arrow (arg, ret) ->
      Format.fprintf fmt "Arrow (%a, %a)" print arg print ret
    | Poly_variant fields ->
      Format.fprintf fmt "Poly_variant (%a)"
        (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
            (fun fmt { pv_constr_name; pv_constr_args } ->
              Format.fprintf fmt "%s (%a)" pv_constr_name
                (Format.pp_print_list
                  ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
                  print)
                pv_constr_args))
        fields
    | Variant constructors ->
      let print_constructor =
        print_constructor (fun fmt (t, _) -> print_nested fmt t)
      in
      Format.fprintf fmt
        "Variant %a"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
            print_constructor)
        constructors
  | Variant_unboxed { name; variant_uid; arg_name; arg_uid; arg_shape;
                      arg_layout = _ } ->
    Format.fprintf fmt "Variant_unboxed %s%a of %a"
      name
      print_uid_opt variant_uid
      (fun fmt -> function
        | Some arg_name ->
          Format.fprintf fmt "{ %s: %a%a }" arg_name print arg_shape
            print_uid_opt arg_uid
        | None -> print fmt arg_shape) arg_name
  | Record { fields; kind } ->
    Format.fprintf fmt "Record%s { %a }" (print_record_type kind)
      (Format.pp_print_list ~pp_sep:(print_sep_string "; ") print_field)
      fields
  | Mutrec m ->
    let print_decls fmt =
        Ident.Map.iter (fun id t ->
            Format.fprintf fmt "@[<hv 2>%a :=@ %a;@]@,"
              Ident.print id
              aux t
          )
      in
    Format.fprintf fmt "Mutrec @[%a@]" print_decls m
  | Proj_decl (t, id) ->
    Format.fprintf fmt "%a.%a"
      print_nested t
      Ident.print id

  in
  if t.approximated then
    Format.fprintf fmt "@[(approx)@ %a@]@;" aux t
  else
    Format.fprintf fmt "@[%a@]@;" aux t

(* We use custom strings as separators instead of pp_print_space, because the
   latter introduces line breaks that can mess up the tables with all shapes. *)
and print_sep_string str fmt () = Format.pp_print_string fmt str

and print_one_entry print_value ppf { field_name; field_uid; field_value } =
  let print_uid_opt =
    Format.pp_print_option (fun fmt -> Format.fprintf fmt "<%a>" Uid.print)
  in
  match field_name with
  | Some name ->
    Format.fprintf ppf "%a%a=%a" Format.pp_print_string name print_uid_opt
      field_uid print_value field_value
  | None -> Format.fprintf ppf "%a%a" print_value field_value print_uid_opt
      field_uid

and print_constructor print_value ppf { name; constr_uid; kind = _; args } =
  let print_uid_opt =
    Format.pp_print_option (fun fmt -> Format.fprintf fmt "<%a>" Uid.print)
  in
  if List.length args = 0 then
    Format.fprintf ppf "%a%a" Format.pp_print_string name print_uid_opt
      constr_uid
  else
    Format.fprintf ppf "@[%a%a of @[%a@]@]" Format.pp_print_string name
      print_uid_opt constr_uid
      (Format.pp_print_list ~pp_sep:(print_sep_string " * ")
          (print_one_entry print_value))
      args

and print_field ppf
    ((name, uid_opt, shape, _) : _ * _ * t * _) =
  let print_uid_opt =
    Format.pp_print_option (fun fmt -> Format.fprintf fmt "<%a>" Uid.print)
  in
  Format.fprintf ppf "%a%a: %a" Format.pp_print_string name print_uid_opt
    uid_opt print shape

and print_record_type = function
  | Record_boxed -> "_boxed"
  | Record_floats -> "_floats"
  | Record_mixed _ -> "_mixed"
  | Record_unboxed -> " [@@unboxed]"
  | Record_unboxed_product -> "_unboxed_product"

let rec strip_head_aliases = function
  | { desc = Alias t; _ } -> strip_head_aliases t
  | t -> t

let hash_var = 1
let hash_abs = 2
let hash_struct = 3
let hash_leaf = 4
let hash_proj = 5
let hash_app = 6
let hash_comp_unit = 7
let hash_alias = 8
let hash_error = 9
let hash_mu = 10
let hash_rec_var = 11
let hash_tuple = 12
let hash_unboxed_tuple = 13
let hash_predef = 14
let hash_arrow = 15
let hash_poly_variant = 16
let hash_variant = 17
let hash_variant_unboxed = 18
let hash_record = 19
let hash_constr = 20
let hash_mutrec = 21
let hash_proj_decl = 22

let fresh_var ?(name="shape-var") uid =
  let var = Ident.create_local name in
  var, { uid = Some uid; desc = Var var;
         hash = Hashtbl.hash (hash_var, uid, var);
         approximated = false }

let for_unnamed_functor_param = Ident.create_local "()"

let var uid id =
  { uid = Some uid; desc = Var id;
    hash = Hashtbl.hash (hash_var, Some uid, id);
    approximated = false }

(* variable without uid *)
let var' uid id =
  { uid; desc = Var id;
    hash = Hashtbl.hash (hash_var, uid, id);
    approximated = false }

let abs ?uid var body =
  { uid; desc = Abs (var, body);
    hash = Hashtbl.hash (hash_abs, uid, body.hash);
    approximated = false }

let str ?uid map =
  let h = Item.Map.fold (fun key t acc ->
    Hashtbl.hash (acc, Item.hash key, t.hash)) map 0
  in
  { uid; desc = Struct map; hash = Hashtbl.hash (hash_struct, uid, h);
    approximated = false }

let alias ?uid t =
  { uid; desc = Alias t;
    hash = Hashtbl.hash (hash_alias, uid, t.hash);
    approximated = false }

let error ?uid s =
  { uid; desc = Error s;
    hash = Hashtbl.hash (hash_error, uid, s);
    approximated = false }

let leaf' uid =
  { uid; desc = Leaf; hash = Hashtbl.hash (hash_leaf, uid);
    approximated = false }

let leaf uid = leaf' (Some uid)

let approx t = { t with approximated = true}

let set_approximated ~approximated t = { t with approximated}

let proj ?uid t item =
  match t.desc with
  | Leaf ->
      (* When stuck projecting in a leaf we propagate the leaf
        as a best effort *)
      approx t
  | Struct map ->
      begin try Item.Map.find item map
      with Not_found -> approx t (* ill-typed program *)
      end
  | _ ->
      { uid; desc = Proj (t, item);
        hash = Hashtbl.hash (hash_proj, t.hash, item); approximated = false }

let app ?uid f ~arg =
  { uid; desc = App (f, arg);
    hash = Hashtbl.hash (hash_app, f.hash, uid, arg.hash);
    approximated = false }

let comp_unit ?uid s =
  { uid; desc = Comp_unit s;
    hash = Hashtbl.hash (hash_comp_unit, uid, s);
    approximated = false }

let mu ?uid t_body =
  { uid; desc = Mu (t_body);
    hash = Hashtbl.hash (hash_mu, uid, t_body.hash);
    approximated = false }

let rec_var ?uid n =
  { uid; desc = Rec_var n;
    hash = Hashtbl.hash (hash_rec_var, uid, n);
    approximated = false }

let app_list (base_shape : t) (args : t list) : t =
  List.fold_left (fun shape arg -> app shape ~arg) base_shape args

let abs_list (base_shape : t) (binders : Ident.t list) : t =
  List.fold_right (fun shape id -> abs shape id) binders base_shape

let tuple ?uid (ts : t list) =
  { uid; desc = Tuple ts;
    hash = Hashtbl.hash (hash_tuple, uid, List.map (fun t -> t.hash) ts);
    approximated = false }

let unboxed_tuple ?uid (ts : t list) =
  { uid; desc = Unboxed_tuple ts;
    hash = Hashtbl.hash (hash_unboxed_tuple, uid,
      List.map (fun t -> t.hash) ts);
    approximated = false }

let predef ?uid (p : Predef.t) (ts : t list) =
  { uid; desc = Predef (p, ts);
    hash = Hashtbl.hash (hash_predef, uid, p,
      List.map (fun t -> t.hash) ts);
    approximated = false }

let arrow ?uid t1 t2 =
  { uid; desc = Arrow (t1, t2);
    hash = Hashtbl.hash (hash_arrow, uid, t1.hash, t2.hash);
    approximated = false }

let poly_variant ?uid t =
  { uid; desc = Poly_variant t;
    hash = Hashtbl.hash (hash_poly_variant, uid,
      poly_variant_constructors_map (fun t -> t.hash) t);
    approximated = false }

let variant ?uid constructors =
  { uid; desc = Variant constructors;
    hash = Hashtbl.hash (hash_variant, uid,
      complex_constructors_map
        (fun (t, ly) -> (t.hash, ly)) constructors);
    approximated = false }

let variant_unboxed ?uid ~variant_uid ~arg_uid name arg_name arg_shape
    arg_layout =
  { uid;
    desc =
      Variant_unboxed
        { name; variant_uid; arg_name; arg_uid; arg_shape; arg_layout };
    hash = Hashtbl.hash (hash_variant_unboxed, uid, name, variant_uid,
      arg_name, arg_uid, arg_shape.hash, arg_layout);
    approximated = false }

let record ?uid kind fields =
  { uid; desc = Record { fields; kind };
    hash = Hashtbl.hash (hash_record, uid,
      List.map (fun (i, uid_opt, t, ly) -> (i, uid_opt, t.hash, ly)) fields,
      kind);
  approximated = false }

let constr ?uid constr_uid args =
  { uid; desc = Constr (constr_uid, args);
    hash = Hashtbl.hash (hash_constr, uid, constr_uid,
      List.map (fun t -> t.hash) args);
    approximated = false }

let mutrec ?uid t =
  { uid; desc = Mutrec t;
    hash = Hashtbl.hash (hash_mutrec, uid,
      Ident.Map.map (fun t -> t.hash) t);
    approximated = false }

let proj_decl ?uid t id =
  { uid; desc = Proj_decl (t, id);
    hash = Hashtbl.hash (hash_proj_decl, uid, t.hash, id);
    approximated = false }


let decompose_abs t =
  match t.desc with
  | Abs (x, t) -> Some (x, t)
  | _ -> None

let dummy_mod = str Item.Map.empty

let of_path ~find_shape ~namespace path =
  (* We need to handle the following cases:
    Path of constructor:
      M.t.C
    Path of label:
      M.t.lbl
    Path of label of inline record:
      M.t.C.lbl
    Path of label of implicit unboxed record:
      M.t#.lbl
  *)
  let rec aux : Sig_component_kind.t -> Path.t -> t = fun ns -> function
    | Pident id -> find_shape ns id
    | Pdot (Pextra_ty (path, Punboxed_ty), name) ->
      (match ns with
       Unboxed_label -> ()
       | _ -> Misc.fatal_error "Shape.of_path");
      proj (aux Type path) (name, Label)
    | Pdot (path, name) ->
      let namespace :  Sig_component_kind.t =
        match (ns : Sig_component_kind.t) with
        | Constructor -> Type
        | Label -> Type
        | Unboxed_label -> Type
        | _ -> Module
      in
      proj (aux namespace path) (name, ns)
    | Papply (p1, p2) -> app (aux Module p1) ~arg:(aux Module p2)
    | Pextra_ty (path, extra) -> begin
        match extra with
          Pcstr_ty name -> proj (aux Type path) (name, Constructor)
        | Pext_ty -> aux Extension_constructor path
        | Punboxed_ty -> aux ns path
      end
  in
  aux namespace path

let for_persistent_unit s =
  comp_unit ~uid:(Compilation_unit s) s

let leaf_for_unpack = leaf' None

let set_uid_if_none t uid =
  (* CR sspies: This function clears the approximated field of the shape.
     However, the alternative is setting the record field, which will result in
     wrong hash values. Perhaps we should fix this instead by removing the UIDs
     from the hash value computation. *)
  let uid = Option.value ~default:uid t.uid in
  match t.desc with
  | Var v -> var uid v
  | Abs (x, t) -> abs ~uid x t
  | App (t1, t2) -> app ~uid t1 ~arg:t2
  | Struct t -> str ~uid t
  | Alias t -> alias ~uid t
  | Leaf -> leaf uid
  | Proj (t, i) -> proj ~uid t i
  | Comp_unit c -> comp_unit ~uid c
  | Error s -> error ~uid s
  | Mu t -> mu ~uid t
  | Rec_var i -> rec_var ~uid i
  | Constr (c, ts) -> constr ~uid c ts
  | Tuple ts -> tuple ~uid ts
  | Unboxed_tuple ts -> unboxed_tuple ~uid ts
  | Predef (p, ts) -> predef ~uid p ts
  | Arrow (t1, t2) -> arrow ~uid t1 t2
  | Poly_variant t -> poly_variant ~uid t
  | Variant cs -> variant ~uid cs
  | Variant_unboxed t ->
    variant_unboxed ~uid ~variant_uid:t.variant_uid ~arg_uid:t.arg_uid
      t.name t.arg_name t.arg_shape t.arg_layout
  | Record t -> record ~uid t.kind t.fields
  | Mutrec ts -> mutrec ~uid ts
  | Proj_decl (t, i) -> proj_decl ~uid t i


let is_mu_closed t =
  let rec debruijn_closed_shape (bound: int) (t: t) =
    match t.desc with
    | Var _ | Leaf | Comp_unit _ | Error _ -> true
      (* we only care about DeBruijn indices in recursive variables *)
    | Abs (_, t) | Alias t | Proj (t, _) | Proj_decl (t, _) ->
      debruijn_closed_shape bound t
    | Variant_unboxed t -> debruijn_closed_shape bound t.arg_shape
    | App (t1, t2) | Arrow (t1, t2) ->
      debruijn_closed_shape bound t1 && debruijn_closed_shape bound t2
    | Constr (_, ts) | Tuple ts | Unboxed_tuple ts | Predef (_, ts) ->
      List.for_all (debruijn_closed_shape bound) ts
    | Struct t -> Item.Map.for_all (fun _ -> debruijn_closed_shape bound) t
    | Mu t -> debruijn_closed_shape (bound + 1) t
    | Rec_var i -> i <= bound
    | Poly_variant t ->
      List.for_all (fun { pv_constr_name = _; pv_constr_args = c } ->
        List.for_all (debruijn_closed_shape bound) c) t
    | Variant t ->
      List.for_all (fun { kind = _; name = _; constr_uid = _; args = c } ->
        List.for_all
          (fun { field_value = t, _; field_name = _; field_uid = _ } ->
          debruijn_closed_shape bound t) c) t
    | Record t -> List.for_all (fun (_, _, t, _) ->
      debruijn_closed_shape bound t) t.fields
    | Mutrec ts -> Ident.Map.for_all (fun _ -> debruijn_closed_shape bound) ts
  in debruijn_closed_shape (-1) t


module Map = struct
  type shape = t
  type nonrec t = t Item.Map.t

  let empty = Item.Map.empty

  let add t item shape = Item.Map.add item shape t

  let add_value t id uid = Item.Map.add (Item.value id) (leaf uid) t
  let add_value_proj t id shape =
    let item = Item.value id in
    Item.Map.add item (proj shape item) t

  let add_type t id shape = Item.Map.add (Item.type_ id) shape t
  let add_type_proj t id shape =
    let item = Item.type_ id in
    Item.Map.add item (proj shape item) t

  let add_constr t id shape = Item.Map.add (Item.constr id) shape t
  let add_constr_proj t id shape =
    let item = Item.constr id in
    Item.Map.add item (proj shape item) t

  let add_label t id uid = Item.Map.add (Item.label id) (leaf uid) t
  let add_label_proj t id shape =
    let item = Item.label id in
    Item.Map.add item (proj shape item) t

  let add_unboxed_label t id uid =
    Item.Map.add (Item.unboxed_label id) (leaf uid) t
  let add_unboxed_label_proj t id shape =
    let item = Item.unboxed_label id in
    Item.Map.add item (proj shape item) t

  let add_module t id shape = Item.Map.add (Item.module_ id) shape t
  let add_module_proj t id shape =
    let item = Item.module_ id in
    Item.Map.add item (proj shape item) t

  let add_module_type t id uid =
    Item.Map.add (Item.module_type id) (leaf uid) t
  let add_module_type_proj t id shape =
    let item = Item.module_type id in
    Item.Map.add item (proj shape item) t

  let add_extcons t id shape =
    Item.Map.add (Item.extension_constructor id) shape t
  let add_extcons_proj t id shape =
    let item = Item.extension_constructor id in
    Item.Map.add item (proj shape item) t

  let add_class t id uid = Item.Map.add (Item.class_ id) (leaf uid) t
  let add_class_proj t id shape =
    let item = Item.class_ id in
    Item.Map.add item (proj shape item) t

  let add_class_type t id uid = Item.Map.add (Item.class_type id) (leaf uid) t
  let add_class_type_proj t id shape =
    let item = Item.class_type id in
    Item.Map.add item (proj shape item) t
end
