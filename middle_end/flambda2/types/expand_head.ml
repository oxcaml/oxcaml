(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module K = Flambda_kind
module MTC = More_type_creators
module TE = Typing_env
module TG = Type_grammar
module TEEV = Typing_env_extension.With_extra_variables

module Expanded_type : sig
  type t

  val of_non_alias_type : ?coercion:Coercion.t -> TG.t -> t

  val create_value : Type_grammar.head_of_kind_value -> t

  val create_naked_immediate : Type_grammar.head_of_kind_naked_immediate -> t

  val create_naked_float32 : Type_grammar.head_of_kind_naked_float32 -> t

  val create_naked_float : Type_grammar.head_of_kind_naked_float -> t

  val create_naked_int8 : Type_grammar.head_of_kind_naked_int8 -> t

  val create_naked_int16 : Type_grammar.head_of_kind_naked_int16 -> t

  val create_naked_int32 : Type_grammar.head_of_kind_naked_int32 -> t

  val create_naked_int64 : Type_grammar.head_of_kind_naked_int64 -> t

  val create_naked_nativeint : Type_grammar.head_of_kind_naked_nativeint -> t

  val create_naked_vec128 : Type_grammar.head_of_kind_naked_vec128 -> t

  val create_naked_vec256 : Type_grammar.head_of_kind_naked_vec256 -> t

  val create_naked_vec512 : Type_grammar.head_of_kind_naked_vec512 -> t

  val create_rec_info : Type_grammar.head_of_kind_rec_info -> t

  val create_region : Type_grammar.head_of_kind_region -> t

  val create_bottom : Flambda_kind.t -> t

  val create_unknown : Flambda_kind.t -> t

  val create_const : Reg_width_const.t -> t

  val bottom_like : t -> t

  val unknown_like : t -> t

  val is_bottom : t -> bool

  val is_unknown : t -> bool

  val is_unknown_maybe_null : t -> bool

  val to_type : t -> Type_grammar.t

  type descr = private
    | Value of Type_grammar.head_of_kind_value
    | Naked_immediate of Type_grammar.head_of_kind_naked_immediate
    | Naked_float32 of Type_grammar.head_of_kind_naked_float32
    | Naked_float of Type_grammar.head_of_kind_naked_float
    | Naked_int8 of Type_grammar.head_of_kind_naked_int8
    | Naked_int16 of Type_grammar.head_of_kind_naked_int16
    | Naked_int32 of Type_grammar.head_of_kind_naked_int32
    | Naked_int64 of Type_grammar.head_of_kind_naked_int64
    | Naked_nativeint of Type_grammar.head_of_kind_naked_nativeint
    | Naked_vec128 of Type_grammar.head_of_kind_naked_vec128
    | Naked_vec256 of Type_grammar.head_of_kind_naked_vec256
    | Naked_vec512 of Type_grammar.head_of_kind_naked_vec512
    | Rec_info of Type_grammar.head_of_kind_rec_info
    | Region of Type_grammar.head_of_kind_region

  val descr : t -> descr Or_unknown_or_bottom.t

  type descr_oub = private
    | Value of Type_grammar.head_of_kind_value Or_unknown_or_bottom.t
    | Naked_immediate of
        Type_grammar.head_of_kind_naked_immediate Or_unknown_or_bottom.t
    | Naked_float32 of
        Type_grammar.head_of_kind_naked_float32 Or_unknown_or_bottom.t
    | Naked_float of
        Type_grammar.head_of_kind_naked_float Or_unknown_or_bottom.t
    | Naked_int8 of Type_grammar.head_of_kind_naked_int8 Or_unknown_or_bottom.t
    | Naked_int16 of
        Type_grammar.head_of_kind_naked_int16 Or_unknown_or_bottom.t
    | Naked_int32 of
        Type_grammar.head_of_kind_naked_int32 Or_unknown_or_bottom.t
    | Naked_int64 of
        Type_grammar.head_of_kind_naked_int64 Or_unknown_or_bottom.t
    | Naked_nativeint of
        Type_grammar.head_of_kind_naked_nativeint Or_unknown_or_bottom.t
    | Naked_vec128 of
        Type_grammar.head_of_kind_naked_vec128 Or_unknown_or_bottom.t
    | Naked_vec256 of
        Type_grammar.head_of_kind_naked_vec256 Or_unknown_or_bottom.t
    | Naked_vec512 of
        Type_grammar.head_of_kind_naked_vec512 Or_unknown_or_bottom.t
    | Rec_info of Type_grammar.head_of_kind_rec_info Or_unknown_or_bottom.t
    | Region of Type_grammar.head_of_kind_region Or_unknown_or_bottom.t

  val descr_oub : t -> descr_oub
end = struct
  type descr =
    | Value of TG.head_of_kind_value
    | Naked_immediate of TG.head_of_kind_naked_immediate
    | Naked_float32 of TG.head_of_kind_naked_float32
    | Naked_float of TG.head_of_kind_naked_float
    | Naked_int8 of TG.head_of_kind_naked_int8
    | Naked_int16 of TG.head_of_kind_naked_int16
    | Naked_int32 of TG.head_of_kind_naked_int32
    | Naked_int64 of TG.head_of_kind_naked_int64
    | Naked_nativeint of TG.head_of_kind_naked_nativeint
    | Naked_vec128 of TG.head_of_kind_naked_vec128
    | Naked_vec256 of TG.head_of_kind_naked_vec256
    | Naked_vec512 of TG.head_of_kind_naked_vec512
    | Rec_info of TG.head_of_kind_rec_info
    | Region of TG.head_of_kind_region

  type t =
    { kind : K.t;
      descr : descr Or_unknown_or_bottom.t
    }

  let descr t = t.descr

  let create_value head = { kind = K.value; descr = Ok (Value head) }

  let create_naked_immediate head =
    { kind = K.naked_immediate; descr = Ok (Naked_immediate head) }

  let create_naked_float32 head =
    { kind = K.naked_float32; descr = Ok (Naked_float32 head) }

  let create_naked_float head =
    { kind = K.naked_float; descr = Ok (Naked_float head) }

  let create_naked_int8 head =
    { kind = K.naked_int8; descr = Ok (Naked_int8 head) }

  let create_naked_int16 head =
    { kind = K.naked_int16; descr = Ok (Naked_int16 head) }

  let create_naked_int32 head =
    { kind = K.naked_int32; descr = Ok (Naked_int32 head) }

  let create_naked_int64 head =
    { kind = K.naked_int64; descr = Ok (Naked_int64 head) }

  let create_naked_nativeint head =
    { kind = K.naked_nativeint; descr = Ok (Naked_nativeint head) }

  let create_naked_vec128 head =
    { kind = K.naked_vec128; descr = Ok (Naked_vec128 head) }

  let create_naked_vec256 head =
    { kind = K.naked_vec256; descr = Ok (Naked_vec256 head) }

  let create_naked_vec512 head =
    { kind = K.naked_vec512; descr = Ok (Naked_vec512 head) }

  let create_rec_info head = { kind = K.rec_info; descr = Ok (Rec_info head) }

  let create_region head = { kind = K.region; descr = Ok (Region head) }

  let create_bottom kind = { kind; descr = Bottom }

  let create_unknown kind = { kind; descr = Unknown }

  let create_const const =
    match Reg_width_const.descr const with
    | Naked_immediate i ->
      create_naked_immediate
        (TG.Head_of_kind_naked_immediate.create_naked_immediate i)
    | Tagged_immediate i ->
      create_value (TG.Head_of_kind_value.create_tagged_immediate i)
    | Naked_float32 f ->
      create_naked_float32 (TG.Head_of_kind_naked_float32.create f)
    | Naked_float f -> create_naked_float (TG.Head_of_kind_naked_float.create f)
    | Naked_int8 i -> create_naked_int8 (TG.Head_of_kind_naked_int8.create i)
    | Naked_int16 i -> create_naked_int16 (TG.Head_of_kind_naked_int16.create i)
    | Naked_int32 i -> create_naked_int32 (TG.Head_of_kind_naked_int32.create i)
    | Naked_int64 i -> create_naked_int64 (TG.Head_of_kind_naked_int64.create i)
    | Naked_nativeint i ->
      create_naked_nativeint (TG.Head_of_kind_naked_nativeint.create i)
    | Naked_vec128 i ->
      create_naked_vec128 (TG.Head_of_kind_naked_vec128.create i)
    | Naked_vec256 i ->
      create_naked_vec256 (TG.Head_of_kind_naked_vec256.create i)
    | Naked_vec512 i ->
      create_naked_vec512 (TG.Head_of_kind_naked_vec512.create i)
    | Null -> create_value TG.Head_of_kind_value.null

  let bottom_like t = create_bottom t.kind

  let unknown_like t = create_unknown t.kind

  let is_bottom t =
    match t.descr with Bottom -> true | Unknown | Ok _ -> false

  let is_unknown t =
    match t.descr with Unknown -> true | Bottom | Ok _ -> false

  let is_unknown_maybe_null t =
    match t.descr with
    | Unknown | Ok (Value { is_null = _; non_null = Unknown }) -> true
    | Bottom
    | Ok
        ( Value _ | Naked_immediate _ | Naked_float32 _ | Naked_float _
        | Naked_int8 _ | Naked_int16 _ | Naked_int32 _ | Naked_int64 _
        | Naked_nativeint _ | Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _
        | Rec_info _ | Region _ ) ->
      false

  let of_non_alias_type ?coercion ty : t =
    match TG.descr ty with
    | Value Unknown -> create_unknown K.value
    | Value Bottom -> create_bottom K.value
    | Value (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_value head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_value head coercion with
        | Bottom -> create_bottom K.value
        | Ok head -> create_value head))
    | Naked_immediate Unknown -> create_unknown K.naked_immediate
    | Naked_immediate Bottom -> create_bottom K.naked_immediate
    | Naked_immediate (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_immediate head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_immediate head coercion with
        | Bottom -> create_bottom K.naked_immediate
        | Ok head -> create_naked_immediate head))
    | Naked_float32 Unknown -> create_unknown K.naked_float32
    | Naked_float32 Bottom -> create_bottom K.naked_float32
    | Naked_float32 (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_float32 head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_float32 head coercion with
        | Bottom -> create_bottom K.naked_float32
        | Ok head -> create_naked_float32 head))
    | Naked_float Unknown -> create_unknown K.naked_float
    | Naked_float Bottom -> create_bottom K.naked_float
    | Naked_float (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_float head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_float head coercion with
        | Bottom -> create_bottom K.naked_float
        | Ok head -> create_naked_float head))
    | Naked_vec128 Unknown -> create_unknown K.naked_vec128
    | Naked_vec128 Bottom -> create_bottom K.naked_vec128
    | Naked_vec128 (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_vec128 head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_vec128 head coercion with
        | Bottom -> create_bottom K.naked_vec128
        | Ok head -> create_naked_vec128 head))
    | Naked_vec256 Unknown -> create_unknown K.naked_vec256
    | Naked_vec256 Bottom -> create_bottom K.naked_vec256
    | Naked_vec256 (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_vec256 head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_vec256 head coercion with
        | Bottom -> create_bottom K.naked_vec256
        | Ok head -> create_naked_vec256 head))
    | Naked_vec512 Unknown -> create_unknown K.naked_vec512
    | Naked_vec512 Bottom -> create_bottom K.naked_vec512
    | Naked_vec512 (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_vec512 head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_vec512 head coercion with
        | Bottom -> create_bottom K.naked_vec512
        | Ok head -> create_naked_vec512 head))
    | Naked_int8 Unknown -> create_unknown K.naked_int8
    | Naked_int8 Bottom -> create_bottom K.naked_int8
    | Naked_int8 (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_int8 head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_int8 head coercion with
        | Bottom -> create_bottom K.naked_int8
        | Ok head -> create_naked_int8 head))
    | Naked_int16 Unknown -> create_unknown K.naked_int16
    | Naked_int16 Bottom -> create_bottom K.naked_int16
    | Naked_int16 (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_int16 head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_int16 head coercion with
        | Bottom -> create_bottom K.naked_int16
        | Ok head -> create_naked_int16 head))
    | Naked_int32 Unknown -> create_unknown K.naked_int32
    | Naked_int32 Bottom -> create_bottom K.naked_int32
    | Naked_int32 (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_int32 head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_int32 head coercion with
        | Bottom -> create_bottom K.naked_int32
        | Ok head -> create_naked_int32 head))
    | Naked_int64 Unknown -> create_unknown K.naked_int64
    | Naked_int64 Bottom -> create_bottom K.naked_int64
    | Naked_int64 (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_int64 head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_int64 head coercion with
        | Bottom -> create_bottom K.naked_int64
        | Ok head -> create_naked_int64 head))
    | Naked_nativeint Unknown -> create_unknown K.naked_nativeint
    | Naked_nativeint Bottom -> create_bottom K.naked_nativeint
    | Naked_nativeint (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_nativeint head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_nativeint head coercion with
        | Bottom -> create_bottom K.naked_nativeint
        | Ok head -> create_naked_nativeint head))
    | Rec_info Unknown -> create_unknown K.rec_info
    | Rec_info Bottom -> create_bottom K.rec_info
    | Rec_info (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_rec_info head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_rec_info head coercion with
        | Bottom -> create_bottom K.rec_info
        | Ok head -> create_rec_info head))
    | Region Unknown -> create_unknown K.region
    | Region Bottom -> create_bottom K.region
    | Region (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_region head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_region head coercion with
        | Bottom -> create_bottom K.region
        | Ok head -> create_region head))
    | Value (Ok (Equals _))
    | Naked_immediate (Ok (Equals _))
    | Naked_float (Ok (Equals _))
    | Naked_float32 (Ok (Equals _))
    | Naked_vec128 (Ok (Equals _))
    | Naked_vec256 (Ok (Equals _))
    | Naked_vec512 (Ok (Equals _))
    | Naked_int8 (Ok (Equals _))
    | Naked_int16 (Ok (Equals _))
    | Naked_int32 (Ok (Equals _))
    | Naked_int64 (Ok (Equals _))
    | Naked_nativeint (Ok (Equals _))
    | Rec_info (Ok (Equals _))
    | Region (Ok (Equals _)) ->
      Misc.fatal_errorf "Type cannot be an alias type:@ %a" TG.print ty

  let to_type (t : t) =
    match t.descr with
    | Unknown -> MTC.unknown t.kind
    | Bottom -> MTC.bottom t.kind
    | Ok descr -> (
      match descr with
      | Value head -> TG.create_from_head_value head
      | Naked_immediate head -> TG.create_from_head_naked_immediate head
      | Naked_float32 head -> TG.create_from_head_naked_float32 head
      | Naked_float head -> TG.create_from_head_naked_float head
      | Naked_int8 head -> TG.create_from_head_naked_int8 head
      | Naked_int16 head -> TG.create_from_head_naked_int16 head
      | Naked_int32 head -> TG.create_from_head_naked_int32 head
      | Naked_int64 head -> TG.create_from_head_naked_int64 head
      | Naked_nativeint head -> TG.create_from_head_naked_nativeint head
      | Naked_vec128 head -> TG.create_from_head_naked_vec128 head
      | Naked_vec256 head -> TG.create_from_head_naked_vec256 head
      | Naked_vec512 head -> TG.create_from_head_naked_vec512 head
      | Rec_info head -> TG.create_from_head_rec_info head
      | Region head -> TG.create_from_head_region head)

  type descr_oub =
    | Value of Type_grammar.head_of_kind_value Or_unknown_or_bottom.t
    | Naked_immediate of
        Type_grammar.head_of_kind_naked_immediate Or_unknown_or_bottom.t
    | Naked_float32 of
        Type_grammar.head_of_kind_naked_float32 Or_unknown_or_bottom.t
    | Naked_float of
        Type_grammar.head_of_kind_naked_float Or_unknown_or_bottom.t
    | Naked_int8 of Type_grammar.head_of_kind_naked_int8 Or_unknown_or_bottom.t
    | Naked_int16 of
        Type_grammar.head_of_kind_naked_int16 Or_unknown_or_bottom.t
    | Naked_int32 of
        Type_grammar.head_of_kind_naked_int32 Or_unknown_or_bottom.t
    | Naked_int64 of
        Type_grammar.head_of_kind_naked_int64 Or_unknown_or_bottom.t
    | Naked_nativeint of
        Type_grammar.head_of_kind_naked_nativeint Or_unknown_or_bottom.t
    | Naked_vec128 of
        Type_grammar.head_of_kind_naked_vec128 Or_unknown_or_bottom.t
    | Naked_vec256 of
        Type_grammar.head_of_kind_naked_vec256 Or_unknown_or_bottom.t
    | Naked_vec512 of
        Type_grammar.head_of_kind_naked_vec512 Or_unknown_or_bottom.t
    | Rec_info of Type_grammar.head_of_kind_rec_info Or_unknown_or_bottom.t
    | Region of Type_grammar.head_of_kind_region Or_unknown_or_bottom.t

  let descr_oub t : descr_oub =
    match t.descr with
    | Unknown -> (
      match t.kind with
      | Value -> Value Unknown
      | Naked_number Naked_immediate -> Naked_immediate Unknown
      | Naked_number Naked_float32 -> Naked_float32 Unknown
      | Naked_number Naked_float -> Naked_float Unknown
      | Naked_number Naked_int8 -> Naked_int8 Unknown
      | Naked_number Naked_int16 -> Naked_int16 Unknown
      | Naked_number Naked_int32 -> Naked_int32 Unknown
      | Naked_number Naked_int64 -> Naked_int64 Unknown
      | Naked_number Naked_nativeint -> Naked_nativeint Unknown
      | Naked_number Naked_vec128 -> Naked_vec128 Unknown
      | Naked_number Naked_vec256 -> Naked_vec256 Unknown
      | Naked_number Naked_vec512 -> Naked_vec512 Unknown
      | Rec_info -> Rec_info Unknown
      | Region -> Region Unknown)
    | Bottom -> (
      match t.kind with
      | Value -> Value Bottom
      | Naked_number Naked_immediate -> Naked_immediate Bottom
      | Naked_number Naked_float32 -> Naked_float32 Bottom
      | Naked_number Naked_float -> Naked_float Bottom
      | Naked_number Naked_int8 -> Naked_int8 Bottom
      | Naked_number Naked_int16 -> Naked_int16 Bottom
      | Naked_number Naked_int32 -> Naked_int32 Bottom
      | Naked_number Naked_int64 -> Naked_int64 Bottom
      | Naked_number Naked_nativeint -> Naked_nativeint Bottom
      | Naked_number Naked_vec128 -> Naked_vec128 Bottom
      | Naked_number Naked_vec256 -> Naked_vec256 Bottom
      | Naked_number Naked_vec512 -> Naked_vec512 Bottom
      | Rec_info -> Rec_info Bottom
      | Region -> Region Bottom)
    | Ok (Value head) -> Value (Ok head)
    | Ok (Naked_immediate head) -> Naked_immediate (Ok head)
    | Ok (Naked_float32 head) -> Naked_float32 (Ok head)
    | Ok (Naked_float head) -> Naked_float (Ok head)
    | Ok (Naked_int8 head) -> Naked_int8 (Ok head)
    | Ok (Naked_int16 head) -> Naked_int16 (Ok head)
    | Ok (Naked_int32 head) -> Naked_int32 (Ok head)
    | Ok (Naked_int64 head) -> Naked_int64 (Ok head)
    | Ok (Naked_nativeint head) -> Naked_nativeint (Ok head)
    | Ok (Naked_vec128 head) -> Naked_vec128 (Ok head)
    | Ok (Naked_vec256 head) -> Naked_vec256 (Ok head)
    | Ok (Naked_vec512 head) -> Naked_vec512 (Ok head)
    | Ok (Rec_info head) -> Rec_info (Ok head)
    | Ok (Region head) -> Region (Ok head)
end

module ET = Expanded_type

let expand_head_of_alias_type env kind
    ~known_canonical_simple_at_in_types_mode:simple =
  let[@inline always] name name ~coercion =
    let ty = TE.find env name (Some kind) in
    match TG.get_alias_exn ty with
    | exception Not_found ->
      let coercion = if Coercion.is_id coercion then None else Some coercion in
      ET.of_non_alias_type ?coercion ty
    | _alias ->
      Misc.fatal_errorf
        "Canonical alias %a should never have [Equals] type %a:@\n\n%a"
        Simple.print simple TG.print ty TE.print env
  in
  Simple.pattern_match simple ~const:ET.create_const ~name

let expand_head0 env ty ~known_canonical_simple_at_in_types_mode =
  match TG.get_alias_exn ty with
  | exception Not_found -> ET.of_non_alias_type ty
  | _ -> (
    match known_canonical_simple_at_in_types_mode with
    | Some simple ->
      expand_head_of_alias_type env (TG.kind ty)
        ~known_canonical_simple_at_in_types_mode:simple
    | None ->
      (* See comment below in [expand_head] about this case. *)
      ET.of_non_alias_type (MTC.unknown (TG.kind ty)))

let expand_head env ty =
  match TG.get_alias_opt ty with
  | None -> ET.of_non_alias_type ty
  | Some simple -> (
    let kind = TG.kind ty in
    match
      TE.get_canonical_simple_exn env simple ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found ->
      (* This can happen when [simple] is of [Phantom] name mode. We're not
         interested in propagating types for phantom variables, so [Unknown] is
         fine here. *)
      ET.of_non_alias_type (MTC.unknown kind)
    | simple ->
      expand_head_of_alias_type ~known_canonical_simple_at_in_types_mode:simple
        env kind)

let is_bottom env t = ET.is_bottom (expand_head env t)

let is_unknown env t = ET.is_unknown (expand_head env t)

let is_unknown_maybe_null env t = ET.is_unknown_maybe_null (expand_head env t)

let is_alias_to_a_symbol t =
  match TG.get_alias_opt t with
  | None -> false
  | Some simple -> Simple.is_symbol simple

let missing_kind env free_names =
  Name_occurrences.fold_variables free_names ~init:false
    ~f:(fun missing_kind var ->
      missing_kind || TE.variable_is_from_missing_cmx_file env (Name.var var))

type to_erase =
  | Everything_not_in of Typing_env.t
  | All_variables_except of Variable.Set.t

exception Missing_cmx_file

let free_variables_transitive ~free_names_of_type env free_vars_acc ty =
  let rec free_variables_transitive0 ty ~free_vars_acc =
    (* We don't need to look at symbols because the assumption (see the .mli) is
       that all symbols have valid types in the target environment. *)
    let free_vars =
      free_names_of_type ty |> Name_occurrences.with_only_variables
    in
    if missing_kind env free_vars
    then raise Missing_cmx_file
    else
      let to_traverse =
        Name_occurrences.diff free_vars ~without:free_vars_acc
      in
      let free_vars_acc = Name_occurrences.union free_vars_acc free_vars in
      Name_occurrences.fold_names to_traverse ~init:free_vars_acc
        ~f:(fun free_vars_acc name ->
          let ty = TE.find env name None in
          free_variables_transitive0 ty ~free_vars_acc)
  in
  free_variables_transitive0 ty ~free_vars_acc

let make_suitable_for_environment env (to_erase : to_erase) bind_to_and_types =
  (match to_erase with
  | Everything_not_in suitable_for ->
    List.iter
      (fun (bind_to, _ty) ->
        if not (TE.mem suitable_for bind_to)
        then
          Misc.fatal_errorf
            "Variable to be bound %a is expected to already be\n\
            \   bound in the [suitable_for] environment:@ %a"
            Name.print bind_to TE.print suitable_for)
      bind_to_and_types
  | All_variables_except _ -> ());
  (* Do a quick free variables check first to try to catch easy cases. *)
  let free_vars =
    List.fold_left
      (fun free_vars (_bind_to, ty) ->
        Name_occurrences.union free_vars
          (Name_occurrences.with_only_variables (TG.free_names ty)))
      Name_occurrences.empty bind_to_and_types
  in
  if Name_occurrences.is_empty free_vars
  then
    List.fold_left
      (fun result (bind_to, ty) ->
        TEEV.add_or_replace_equation result bind_to ty)
      TEEV.empty bind_to_and_types
  else
    (* Now collect all of the free variables, transitively (see comment on
       function above). *)
    let root_types = List.map snd bind_to_and_types in
    match
      ( List.fold_left
          (free_variables_transitive ~free_names_of_type:TG.free_names env)
          Name_occurrences.empty root_types,
        List.fold_left
          (free_variables_transitive
             ~free_names_of_type:TG.free_names_except_through_value_slots env)
          Name_occurrences.empty root_types )
    with
    | exception Missing_cmx_file ->
      (* Just forget everything if there is a .cmx file missing. *)
      List.fold_left
        (fun result (bind_to, ty) ->
          TEEV.add_or_replace_equation result bind_to (MTC.unknown_like ty))
        TEEV.empty bind_to_and_types
    | free_vars, free_vars_except_through_value_slots ->
      (* Determine which variables will be unavailable and thus need fresh ones
         assigning to them. *)
      let ( unavailable_vars_renamed,
            unavailable_vars_expanded,
            unavailable_vars_removed ) =
        let erase var =
          match to_erase with
          | Everything_not_in suitable_for ->
            not (TE.mem suitable_for (Name.var var))
          | All_variables_except to_keep -> not (Variable.Set.mem var to_keep)
        in
        Name_occurrences.fold_variables free_vars ~init:([], [], [])
          ~f:(fun
              (( unavailable_vars_renamed,
                 unavailable_vars_expanded,
                 unavailable_vars_removed ) as unavailable_vars)
              var
            ->
            if erase var
            then
              if
                Name_occurrences.mem_var free_vars_except_through_value_slots
                  var
              then
                match Name_occurrences.count_variable free_vars var with
                | Zero ->
                  Misc.fatal_errorf
                    "Inconsistent occurrences of %a in free names"
                    Variable.print var
                | One ->
                  ( unavailable_vars_renamed,
                    var :: unavailable_vars_expanded,
                    unavailable_vars_removed )
                | More_than_one ->
                  ( var :: unavailable_vars_renamed,
                    unavailable_vars_expanded,
                    unavailable_vars_removed )
              else
                ( unavailable_vars_renamed,
                  unavailable_vars_expanded,
                  var :: unavailable_vars_removed )
            else unavailable_vars)
      in
      (* Fetch the type equation for each free variable. Also add in the
         equations about the "bind-to" names provided to this function. If any
         of the "bind-to" names are already defined in [env], the type given in
         [bind_to_and_types] takes precedence over such definition. All
         occurrences of variables that only occur once are expanded directly.
         All occurrences of variables that are only reachable through closure
         variables are replaced with an Unknown type. *)
      let to_expand = Variable.Set.of_list unavailable_vars_expanded in
      let to_remove = Variable.Set.of_list unavailable_vars_removed in
      let to_project = Variable.Set.union to_expand to_remove in
      let expand_type ty =
        let rec expand var =
          let ty = TE.find env (Name.var var) None in
          if Variable.Set.mem var to_remove
          then MTC.unknown_like ty
          else
            match TG.get_alias_exn ty with
            | exception Not_found ->
              TG.project_variables_out ~to_project ~expand ty
            | simple ->
              Simple.pattern_match' simple
                ~const:(fun _ -> ty)
                ~symbol:(fun _ ~coercion:_ -> ty)
                ~var:(fun var ~coercion ->
                  if Variable.Set.mem var to_expand
                  then TG.apply_coercion (expand var) coercion
                  else ty)
        in
        TG.project_variables_out ~to_project ~expand ty
      in
      let equations =
        ListLabels.fold_left unavailable_vars_renamed ~init:[]
          ~f:(fun equations var ->
            let name = Name.var var in
            let ty = TE.find env name None in
            (name, expand_type ty) :: equations)
      in
      let equations =
        List.fold_left
          (fun equations (bind_to, ty) ->
            (* The [bind_to] variables are not expected to be unavailable, so
               this shouldn't cause duplicates. *)
            (bind_to, expand_type ty) :: equations)
          equations bind_to_and_types
      in
      (* Make fresh variables for the unavailable variables and form a
         renaming. *)
      let unavailable_to_fresh_vars =
        List.map (fun var -> var, Variable.rename var) unavailable_vars_renamed
        |> Variable.Map.of_list
      in
      let renaming =
        Variable.Map.fold
          (fun unavailable_var fresh_var renaming ->
            Renaming.add_fresh_variable renaming unavailable_var
              ~guaranteed_fresh:fresh_var)
          unavailable_to_fresh_vars Renaming.empty
      in
      (* Now replace any unavailable variables with their fresh counterparts, on
         both sides of the equations map. At the same time identify which
         equations now have fresh variables on their left-hand sides. *)
      let equations =
        List.map
          (fun (lhs, ty) ->
            let lhs' = Renaming.apply_name renaming lhs in
            let ty = TG.apply_renaming ty renaming in
            lhs', ty)
          equations
      in
      (* Finally form an environment extension with extra variables: the
         existentials are the fresh variables. *)
      let bind_to_vars =
        List.fold_left
          (fun bind_to_vars (bind_to, _) ->
            Name.pattern_match bind_to
              ~var:(fun var -> Variable.Set.add var bind_to_vars)
              ~symbol:(fun _ -> bind_to_vars))
          Variable.Set.empty bind_to_and_types
      in
      List.fold_left
        (fun env_extension (lhs, ty) ->
          let env_extension =
            Name.pattern_match lhs
              ~var:(fun lhs ->
                if not (Variable.Set.mem lhs bind_to_vars)
                then TEEV.add_definition env_extension lhs (TG.kind ty)
                else env_extension)
              ~symbol:(fun _ -> env_extension)
          in
          if TG.is_obviously_unknown ty
          then env_extension
          else TEEV.add_or_replace_equation env_extension lhs ty)
        TEEV.empty equations

let[@inline] is_useful_or_unknown_or_bottom is_useful_head kind env
    (input : _ Or_unknown_or_bottom.t) =
  match input with
  | Unknown | Bottom -> false
  | Ok head -> is_useful_head kind env head

let is_useful_set (type a) (module S : Container_types_intf.Set with type t = a)
    _env (s : a) =
  match S.get_singleton s with Some _ -> true | None -> false

let rec is_useful full_kind env t =
  let expanded = expand_head env t in
  is_useful_or_unknown_or_bottom is_useful_expanded_head_descr full_kind env
    (ET.descr expanded)

and is_useful_expanded_head_descr full_kind env (descr : ET.descr) =
  match descr, K.With_subkind.kind full_kind with
  | Value head, Value ->
    is_useful_head_of_kind_value env
      (K.With_subkind.nullable full_kind)
      (K.With_subkind.non_null_value_subkind full_kind)
      head
  | Naked_immediate head, Naked_number Naked_immediate ->
    is_useful_head_of_kind_naked_immediate Or_unknown.Unknown env head
  | Naked_float32 head, Naked_number Naked_float32 ->
    is_useful_head_of_kind_naked_float32 env head
  | Naked_float head, Naked_number Naked_float ->
    is_useful_head_of_kind_naked_float env head
  | Naked_int8 head, Naked_number Naked_int8 ->
    is_useful_head_of_kind_naked_int8 env head
  | Naked_int16 head, Naked_number Naked_int16 ->
    is_useful_head_of_kind_naked_int16 env head
  | Naked_int32 head, Naked_number Naked_int32 ->
    is_useful_head_of_kind_naked_int32 env head
  | Naked_int64 head, Naked_number Naked_int64 ->
    is_useful_head_of_kind_naked_int64 env head
  | Naked_nativeint head, Naked_number Naked_nativeint ->
    is_useful_head_of_kind_naked_nativeint env head
  | Naked_vec128 head, Naked_number Naked_vec128 ->
    is_useful_head_of_kind_naked_vec128 env head
  | Naked_vec256 head, Naked_number Naked_vec256 ->
    is_useful_head_of_kind_naked_vec256 env head
  | Naked_vec512 head, Naked_number Naked_vec512 ->
    is_useful_head_of_kind_naked_vec512 env head
  | Rec_info head, Rec_info -> is_useful_head_of_kind_rec_info env head
  | Region head, Region -> is_useful_head_of_kind_region env head
  | ( ( Value _ | Naked_immediate _ | Naked_float32 _ | Naked_float _
      | Naked_int8 _ | Naked_int16 _ | Naked_int32 _ | Naked_int64 _
      | Naked_nativeint _ | Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _
      | Rec_info _ | Region _ ),
      ( Value
      | Naked_number
          ( Naked_immediate | Naked_float32 | Naked_float | Naked_int8
          | Naked_int16 | Naked_int32 | Naked_int64 | Naked_nativeint
          | Naked_vec128 | Naked_vec256 | Naked_vec512 )
      | Rec_info | Region ) ) ->
    false

and is_useful_head_of_kind_value env (nullable : K.With_subkind.Nullable.t)
    non_null_value_subkind ({ non_null; is_null } : TG.head_of_kind_value) =
  match is_null, nullable with
  | Not_null, Nullable ->
    (* If the kind thinks we are nullable but the type knows we are not null,
       that's maybe useful. *)
    true
  | (Not_null | Maybe_null _), (Nullable | Non_nullable) ->
    is_useful_or_unknown_or_bottom is_useful_head_of_kind_value_non_null env
      non_null_value_subkind non_null

and is_useful_head_of_kind_value_non_null env
    (non_null_value_subkind : K.With_subkind.Non_null_value_subkind.t)
    (non_null : TG.head_of_kind_value_non_null) =
  match non_null_value_subkind, non_null with
  | ( Anything,
      ( Variant _ | Mutable_block _ | Boxed_float32 _ | Boxed_float _
      | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _ | Boxed_vec128 _
      | Boxed_vec256 _ | Boxed_vec512 _ | Closures _ | String _ | Array _ ) ) ->
    true
  | Boxed_float32, Boxed_float32 (ty, _alloc_mode) ->
    is_useful K.With_subkind.naked_float32 env ty
  | Boxed_float, Boxed_float (ty, _alloc_mode) ->
    is_useful K.With_subkind.naked_float env ty
  | Boxed_int32, Boxed_int32 (ty, _alloc_mode) ->
    is_useful K.With_subkind.naked_int32 env ty
  | Boxed_int64, Boxed_int64 (ty, _alloc_mode) ->
    is_useful K.With_subkind.naked_int64 env ty
  | Boxed_nativeint, Boxed_nativeint (ty, _alloc_mode) ->
    is_useful K.With_subkind.naked_nativeint env ty
  | Boxed_vec128, Boxed_vec128 (ty, _alloc_mode) ->
    is_useful K.With_subkind.naked_vec128 env ty
  | Boxed_vec256, Boxed_vec256 (ty, _alloc_mode) ->
    is_useful K.With_subkind.naked_vec256 env ty
  | Boxed_vec512, Boxed_vec512 (ty, _alloc_mode) ->
    is_useful K.With_subkind.naked_vec512 env ty
  | Tagged_immediate, Variant { immediates; _ } -> (
    match immediates with
    | Unknown -> false
    | Known ty -> is_useful K.With_subkind.naked_immediate env ty)
  | Variant { consts; non_consts }, Variant { immediates; blocks; _ } ->
    is_useful_variant ~consts ~non_consts env ~immediates ~blocks
  | Variant _, Mutable_block _ -> false
  | Float_block { num_fields }, Variant { blocks; _ } ->
    is_useful_float_block ~num_fields env ~blocks
  | Float_block { num_fields = _ }, Mutable_block _ -> false
  | Float_array, Array { element_kind; length; contents; alloc_mode } ->
    is_useful_array (Or_unknown.Known K.With_subkind.naked_float) env
      ~element_kind ~length ~contents ~alloc_mode
  | Immediate_array, Array { element_kind; length; contents; alloc_mode } ->
    is_useful_array (Or_unknown.Known K.With_subkind.tagged_immediate) env
      ~element_kind ~length ~contents ~alloc_mode
  | Value_array, Array { element_kind; length; contents; alloc_mode } ->
    is_useful_array (Or_unknown.Known K.With_subkind.any_value) env
      ~element_kind ~length ~contents ~alloc_mode
  | Generic_array, Array { element_kind; length; contents; alloc_mode } ->
    is_useful_array Or_unknown.Unknown env ~element_kind ~length ~contents
      ~alloc_mode
  | Unboxed_float32_array, Array { element_kind; length; contents; alloc_mode }
    ->
    is_useful_array (Or_unknown.Known K.With_subkind.naked_float32) env
      ~element_kind ~length ~contents ~alloc_mode
  | Untagged_int_array, Array { element_kind; length; contents; alloc_mode } ->
    is_useful_array (Or_unknown.Known K.With_subkind.naked_immediate) env
      ~element_kind ~length ~contents ~alloc_mode
  | Untagged_int8_array, Array { element_kind; length; contents; alloc_mode } ->
    is_useful_array (Or_unknown.Known K.With_subkind.naked_int8) env
      ~element_kind ~length ~contents ~alloc_mode
  | Untagged_int16_array, Array { element_kind; length; contents; alloc_mode }
    ->
    is_useful_array (Or_unknown.Known K.With_subkind.naked_int16) env
      ~element_kind ~length ~contents ~alloc_mode
  | Unboxed_int32_array, Array { element_kind; length; contents; alloc_mode } ->
    is_useful_array (Or_unknown.Known K.With_subkind.naked_int32) env
      ~element_kind ~length ~contents ~alloc_mode
  | Unboxed_int64_array, Array { element_kind; length; contents; alloc_mode } ->
    is_useful_array (Or_unknown.Known K.With_subkind.naked_int64) env
      ~element_kind ~length ~contents ~alloc_mode
  | ( Unboxed_nativeint_array,
      Array { element_kind; length; contents; alloc_mode } ) ->
    is_useful_array (Or_unknown.Known K.With_subkind.naked_nativeint) env
      ~element_kind ~length ~contents ~alloc_mode
  | Unboxed_vec128_array, Array { element_kind; length; contents; alloc_mode }
    ->
    is_useful_array (Or_unknown.Known K.With_subkind.naked_vec128) env
      ~element_kind ~length ~contents ~alloc_mode
  | Unboxed_vec256_array, Array { element_kind; length; contents; alloc_mode }
    ->
    is_useful_array (Or_unknown.Known K.With_subkind.naked_vec256) env
      ~element_kind ~length ~contents ~alloc_mode
  | Unboxed_vec512_array, Array { element_kind; length; contents; alloc_mode }
    ->
    is_useful_array (Or_unknown.Known K.With_subkind.naked_vec512) env
      ~element_kind ~length ~contents ~alloc_mode
  | Unboxed_product_array, Array { element_kind; length; contents; alloc_mode }
    ->
    is_useful_array Or_unknown.Unknown env ~element_kind ~length ~contents
      ~alloc_mode
  | ( ( Boxed_float32 | Boxed_float | Boxed_int32 | Boxed_int64
      | Boxed_nativeint | Boxed_vec128 | Boxed_vec256 | Boxed_vec512
      | Tagged_immediate | Variant _ | Float_block _ | Float_array
      | Immediate_array | Value_array | Generic_array | Unboxed_float32_array
      | Untagged_int_array | Untagged_int8_array | Untagged_int16_array
      | Unboxed_int32_array | Unboxed_int64_array | Unboxed_nativeint_array
      | Unboxed_vec128_array | Unboxed_vec256_array | Unboxed_vec512_array
      | Unboxed_product_array ),
      _ ) ->
    false

and is_useful_variant ~consts ~non_consts env ~immediates ~blocks =
  match is_useful_tagged_immediate ~consts env ~immediates with
  | true -> true
  | false -> is_useful_block ~non_consts env ~blocks

and is_useful_tagged_immediate ~consts env ~immediates =
  if Target_ocaml_int.Set.is_empty consts
  then false
  else
    match (immediates : _ Or_unknown.t) with
    | Unknown -> false
    | Known ty -> (
      let expanded = expand_head env ty in
      match ET.descr expanded with
      | Unknown -> false
      | Bottom -> true
      | Ok (Naked_immediate head) ->
        is_useful_head_of_kind_naked_immediate (Or_unknown.Known consts) env
          head
      | Ok
          ( Value _ | Naked_float32 _ | Naked_float _ | Naked_int8 _
          | Naked_int16 _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
          | Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _ | Rec_info _
          | Region _ ) ->
        false)

and is_useful_block ~non_consts env ~blocks =
  if Tag.Scannable.Map.is_empty non_consts
  then false
  else
    match (blocks : TG.row_like_for_blocks Or_unknown.t) with
    | Unknown -> false
    | Known row_like_for_blocks ->
      if TG.Row_like_for_blocks.is_bottom row_like_for_blocks
      then true
      else if
        Tag.Scannable.Map.exists
          (fun tag (_block_shape, field_kinds) ->
            let tag = Tag.Scannable.to_tag tag in
            let[@local] process_case
                (row_like_block_case : TG.row_like_block_case) =
              (* Note: if there is a mismatch in the number of fields, it is OK
                 to return [true]; we should be able to prove [Bottom] during
                 inlining. *)
              let types = row_like_block_case.maps_to in
              try
                List.iteri
                  (fun ix field_kind ->
                    if ix < Array.length types
                    then
                      match is_useful field_kind env types.(ix) with
                      | true -> raise Exit
                      | false -> ()
                    else raise Exit)
                  field_kinds;
                false
              with Exit -> true
            in
            match Tag.Map.find tag row_like_for_blocks.known_tags with
            | Unknown -> false
            | Known row_like_block_case -> process_case row_like_block_case
            | exception Not_found -> (
              match row_like_for_blocks.other_tags with
              | Bottom -> true
              | Ok row_like_block_case -> process_case row_like_block_case))
          non_consts
      then true
      else false

and is_useful_float_block ~num_fields:_ env ~blocks =
  match (blocks : TG.row_like_for_blocks Or_unknown.t) with
  | Unknown -> false
  | Known row_like_for_blocks -> (
    let[@local] process_case (row_like_block_case : TG.row_like_block_case) =
      if
        Array.exists
          (fun ty ->
            match is_useful K.With_subkind.naked_float env ty with
            | true -> true
            | false -> false)
          row_like_block_case.maps_to
      then true
      else false
    in
    match Tag.Map.find Tag.double_array_tag row_like_for_blocks.known_tags with
    | Unknown -> false
    | Known row_like_block_case -> process_case row_like_block_case
    | exception Not_found -> (
      match row_like_for_blocks.other_tags with
      | Bottom -> true
      | Ok row_like_block_case -> process_case row_like_block_case))

and is_useful_array (known_element_kind : _ Or_unknown.t) env ~element_kind
    ~length ~contents:_ ~alloc_mode:_ =
  (* If we know the length, that's maybe useful (we could eliminate some bounds
     check). *)
  match is_useful K.With_subkind.tagged_immediate env length with
  | true -> true
  | false -> (
    (* If the [known_element_kind] from the kind can be used as the
       [element_kind] from the types, the [element_kind] is not more precise.

       But if it can't (not compatible), then the [element_kind] from the types
       is more precise than the one from the kind -- that's maybe useful.

       If the element kind from the kind is not known, but the kind from the
       type is known (including if it is bottom, in which case this must be an
       empty array), it is maybe useful. *)
    match known_element_kind, (element_kind : _ Or_unknown_or_bottom.t) with
    | Unknown, (Ok _ | Bottom) -> true
    | Known known_element_kind, Ok element_kind
      when not
             (K.With_subkind.compatible known_element_kind
                ~when_used_at:element_kind) ->
      true
    | (Unknown | Known _), (Unknown | Bottom | Ok _) -> false)

and is_useful_head_of_kind_naked_immediate
    (consts : Target_ocaml_int.Set.t Or_unknown.t) env
    (head : TG.head_of_kind_naked_immediate) =
  let module I = Target_ocaml_int in
  match consts, head with
  | Unknown, Naked_immediates set -> is_useful_set (module I.Set) env set
  | Known consts, Naked_immediates set ->
    (* The type is useful as long as we can eliminate at least one of the
       constructors. *)
    if I.Set.subset consts set then false else true
  | (Known _ | Unknown), (Is_null _ | Is_int _ | Get_tag _) -> false

and is_useful_head_of_kind_naked_float32 env head =
  is_useful_set
    (module Numeric_types.Float32_by_bit_pattern.Set)
    env
    (head
      : TG.head_of_kind_naked_float32
      :> Numeric_types.Float32_by_bit_pattern.Set.t)

and is_useful_head_of_kind_naked_float env head =
  is_useful_set
    (module Numeric_types.Float_by_bit_pattern.Set)
    env
    (head
      : TG.head_of_kind_naked_float
      :> Numeric_types.Float_by_bit_pattern.Set.t)

and is_useful_head_of_kind_naked_int8 env head =
  is_useful_set
    (module Numeric_types.Int8.Set)
    env
    (head : TG.head_of_kind_naked_int8 :> Numeric_types.Int8.Set.t)

and is_useful_head_of_kind_naked_int16 env head =
  is_useful_set
    (module Numeric_types.Int16.Set)
    env
    (head : TG.head_of_kind_naked_int16 :> Numeric_types.Int16.Set.t)

and is_useful_head_of_kind_naked_int32 env head =
  is_useful_set
    (module Numeric_types.Int32.Set)
    env
    (head : TG.head_of_kind_naked_int32 :> Numeric_types.Int32.Set.t)

and is_useful_head_of_kind_naked_int64 env head =
  is_useful_set
    (module Numeric_types.Int64.Set)
    env
    (head : TG.head_of_kind_naked_int64 :> Numeric_types.Int64.Set.t)

and is_useful_head_of_kind_naked_nativeint env head =
  is_useful_set
    (module Targetint_32_64.Set)
    env
    (head : TG.head_of_kind_naked_nativeint :> Targetint_32_64.Set.t)

and is_useful_head_of_kind_naked_vec128 env head =
  is_useful_set
    (module Vector_types.Vec128.Bit_pattern.Set)
    env
    (head
      : TG.head_of_kind_naked_vec128
      :> Vector_types.Vec128.Bit_pattern.Set.t)

and is_useful_head_of_kind_naked_vec256 env head =
  is_useful_set
    (module Vector_types.Vec256.Bit_pattern.Set)
    env
    (head
      : TG.head_of_kind_naked_vec256
      :> Vector_types.Vec256.Bit_pattern.Set.t)

and is_useful_head_of_kind_naked_vec512 env head =
  is_useful_set
    (module Vector_types.Vec512.Bit_pattern.Set)
    env
    (head
      : TG.head_of_kind_naked_vec512
      :> Vector_types.Vec512.Bit_pattern.Set.t)

and is_useful_head_of_kind_rec_info _env _head = false

and is_useful_head_of_kind_region _env () = false

let type_is_useful full_kind env name =
  let ty = TE.find env name (Some (K.With_subkind.kind full_kind)) in
  is_useful full_kind env ty
