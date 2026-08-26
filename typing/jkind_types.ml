(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* See Note [Kind properties] in [jkind_intf.ml]. *)
module type Property = sig
  (** A value of this type denotes a *property-enforcing operator*: an
      idempotent, monotone function on kinds whose fixed points are exactly the
      kinds satisfying some property. Any two such operators commute, so
      [compose] is commutative and associative. *)
  type t

  (** The identity operator. *)
  val id : t

  val is_id : t -> bool

  val equal : t -> t -> bool

  (** [compose t1 t2] denotes [t1 ° t2] (equivalently [t2 ° t1]). *)
  val compose : t -> t -> t

  (** The order for which [compose] is the meet and [id] is the top: [t1] is
      less than [t2] when [t1] enforces at least as much as [t2]. *)
  val less_or_equal : t -> t -> Misc.Le_result.t

  (** [residual ~have t] is the part of [t] that [have] does not already
      enforce. It satisfies [compose (residual ~have t) have = compose t have],
      and [is_id (residual ~have t)] exactly when [have x] is a fixed point of
      [t] for every [x]. *)
  val residual : have:t -> t -> t

  (** The kind modifiers spelling out [t], in the order they are printed. *)
  val to_string_list : t -> string list
end

module Addressability = struct
  type t =
    | Id
    | Addressable

  let id = Id

  let is_id = function Id -> true | Addressable -> false

  let equal t1 t2 =
    match t1, t2 with
    | Id, Id | Addressable, Addressable -> true
    | (Id | Addressable), _ -> false

  let compose t1 t2 =
    match t1, t2 with
    | Id, t | t, Id -> t
    | Addressable, Addressable -> Addressable

  let less_or_equal t1 t2 : Misc.Le_result.t =
    match t1, t2 with
    | Id, Id | Addressable, Addressable -> Equal
    | Addressable, Id -> Less
    | Id, Addressable -> Not_le

  let residual ~have t =
    match have, t with
    | Addressable, _ | _, Id -> Id
    | Id, Addressable -> Addressable

  let to_string_list = function Id -> [] | Addressable -> ["addressable"]
end

module Scannable_axes = struct
  open Jkind_axis

  type t =
    { nullability : Nullability.t;
      separability : Separability.t
    }

  let max = { nullability = Nullability.max; separability = Separability.max }

  let id = max

  let value_axes = { nullability = Non_null; separability = Separable }

  let equal { nullability = n1; separability = s1 }
      { nullability = n2; separability = s2 } =
    Nullability.equal n1 n2 && Separability.equal s1 s2

  let is_id t = t == id || equal t max

  let less_or_equal { nullability = n1; separability = s1 }
      { nullability = n2; separability = s2 } =
    Misc.Le_result.combine
      (Nullability.less_or_equal n1 n2)
      (Separability.less_or_equal s1 s2)

  let le t1 t2 = Misc.Le_result.is_le (less_or_equal t1 t2)

  let meet { nullability = n1; separability = s1 }
      { nullability = n2; separability = s2 } =
    { nullability = Nullability.meet n1 n2;
      separability = Separability.meet s1 s2
    }

  let compose t1 t2 =
    if is_id t1 then t2 else if is_id t2 then t1 else meet t1 t2

  (* Computed axis-by-axis: an axis that [have] already lowers far enough
     contributes nothing to the residual, independently of the other axes. *)
  let residual ~have t =
    if is_id t
    then id
    else
      let nullability =
        if
          Misc.Le_result.is_le
            (Nullability.less_or_equal have.nullability t.nullability)
        then Nullability.max
        else t.nullability
      in
      let separability =
        if
          Misc.Le_result.is_le
            (Separability.less_or_equal have.separability t.separability)
        then Separability.max
        else t.separability
      in
      let t' = { nullability; separability } in
      if is_id t' then id else t'

  (* A scannable axis annotation can only lower, so [base] is only a valid
     prefix when [actual <= base] on every axis. If it's not, return [None]. *)
  let to_string_list_diff
      ~base:{ nullability = n_against; separability = s_against }
      { nullability; separability } =
    let nullability_diff =
      match Nullability.less_or_equal nullability n_against with
      | Equal -> Some []
      | Less -> Some [Nullability.to_string nullability]
      | Not_le -> None
    in
    let separability_diff =
      match Separability.less_or_equal separability s_against with
      | Equal -> Some []
      | Less -> Some [Separability.to_string separability]
      | Not_le -> None
    in
    Misc.Stdlib.List.some_if_all_elements_are_some
      [separability_diff; nullability_diff]
    |> Option.map List.concat

  let to_string_list t =
    Option.value (to_string_list_diff ~base:max t) ~default:[]
end

(* The properties a kind may be asked to satisfy, bundled together. Since the
   components commute, everything here is computed componentwise. *)
module Prop = struct
  type t =
    { addressability : Addressability.t;
      scannable_axes : Scannable_axes.t
    }

  let id =
    { addressability = Addressability.id; scannable_axes = Scannable_axes.id }

  let create ~addressability ~scannable_axes =
    if
      Addressability.is_id addressability && Scannable_axes.is_id scannable_axes
    then id
    else { addressability; scannable_axes }

  let addressable = { id with addressability = Addressable }

  let of_scannable_axes scannable_axes =
    if Scannable_axes.is_id scannable_axes
    then id
    else { id with scannable_axes }

  let is_id t =
    t == id
    || Addressability.is_id t.addressability
       && Scannable_axes.is_id t.scannable_axes

  let is_addressable t =
    match t.addressability with Addressable -> true | Id -> false

  let equal t1 t2 =
    t1 == t2
    || Addressability.equal t1.addressability t2.addressability
       && Scannable_axes.equal t1.scannable_axes t2.scannable_axes

  let compose t1 t2 =
    if is_id t1
    then t2
    else if is_id t2
    then t1
    else
      { addressability =
          Addressability.compose t1.addressability t2.addressability;
        scannable_axes =
          Scannable_axes.compose t1.scannable_axes t2.scannable_axes
      }

  let less_or_equal t1 t2 =
    Misc.Le_result.combine
      (Addressability.less_or_equal t1.addressability t2.addressability)
      (Scannable_axes.less_or_equal t1.scannable_axes t2.scannable_axes)

  let residual ~have t =
    if is_id t
    then id
    else
      create
        ~addressability:
          (Addressability.residual ~have:have.addressability t.addressability)
        ~scannable_axes:
          (Scannable_axes.residual ~have:have.scannable_axes t.scannable_axes)

  (** The residual of [t] for a term that can never be [Scannable] (a product,
      or a base other than [Scannable]): the scannable axes are meaningless on
      such terms, so they are automatically fixed points of that component. *)
  let on_unscannable t =
    if Scannable_axes.is_id t.scannable_axes
    then t
    else
      create ~addressability:t.addressability ~scannable_axes:Scannable_axes.id

  let to_string_list t =
    Scannable_axes.to_string_list t.scannable_axes
    @ Addressability.to_string_list t.addressability
end

module Sort = struct
  type base =
    | Void
    | Scannable
    | Untagged_immediate
    | Float64
    | Float32
    | Word
    | Bits8
    | Bits16
    | Bits32
    | Bits64
    | Vec128
    | Vec256
    | Vec512
    | Mask

  type univar = { name : string option }

  (* Tracking univar correspondences for Trepr unification *)
  let univar_pairs : (univar * univar) list ref = ref []

  let equal_univar_univar uv1 uv2 =
    uv1 == uv2
    || List.exists
         (fun (p1, p2) -> (p1 == uv1 && p2 == uv2) || (p1 == uv2 && p2 == uv1))
         !univar_pairs

  (* Establish correspondence between sort univars positionally.
     Since Trepr respects order, we just pair them up directly. *)
  let enter_repr pairs f =
    let old_univars = !univar_pairs in
    univar_pairs := pairs @ old_univars;
    Misc.try_finally f ~always:(fun () -> univar_pairs := old_univars)

  (* Special sentinel levels stored in [var.level] when [contents = None]:
     - [level_generic]: a generalized sort variable (genvar), used for layout
       polymorphism and must be quantified. That is, they can only appear under
       [instance_map], etc.)
     - [level_rigid]: a rigid sort variable that cannot be unified.
     - [level_fresh]: a freshly-created unifiable sort variable whose level has
       not yet been set; it will be lowered via [update_level] as soon as it is
       unified with another variable.
     When [contents = Some t], [level] is meaningless. *)
  (* CR-soon zqian: Add the invariant that, when [contents = Some v], we have
    [level >= v.level]. This can improve performance. *)
  let level_generic = Ident.highest_scope

  let level_rigid = Ident.highest_scope - 1

  let level_fresh = Ident.highest_scope - 2

  (* See Note [Kind properties] in [jkind_intf.ml]: a sort [{ prop; data }]
     denotes the property-enforcing operator [prop] applied to [data]. *)
  type data =
    | Var of var
    | Base of base
    | Product of t list
    | Univar of univar

  and t =
    { prop : Prop.t;
      data : data
    }

  and var =
    { mutable contents : t option;
      mutable level : int;  (** See comments on [level_generic] *)
      id : int
    }

  let[@inline] of_data data = { prop = Prop.id; data }

  (* Applying an operator is just composition: the two encodings of, say,
     addressability on a sort variable ([{ prop = addressable; data = Var v }]
     versus [v] filled in with an addressable sort) are both allowed, and the
     judgments below see through both by taking residuals. *)
  let[@inline] apply_prop prop t =
    if Prop.is_id prop then t else { t with prop = Prop.compose prop t.prop }

  let is_rigidvar var =
    assert (Option.is_none var.contents);
    var.level = level_rigid

  let is_genvar var =
    assert (Option.is_none var.contents);
    var.level = level_generic

  let equal_base b1 b2 =
    match b1, b2 with
    | Void, Void
    | Scannable, Scannable
    | Untagged_immediate, Untagged_immediate
    | Float64, Float64
    | Float32, Float32
    | Word, Word
    | Bits8, Bits8
    | Bits16, Bits16
    | Bits32, Bits32
    | Bits64, Bits64
    | Vec128, Vec128
    | Vec256, Vec256
    | Vec512, Vec512
    | Mask, Mask ->
      true
    | ( ( Void | Scannable | Untagged_immediate | Float64 | Float32 | Word
        | Bits8 | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 | Mask ),
        _ ) ->
      false

  let to_string_base = function
    | Scannable -> "value" (* printed as "value" to users *)
    | Void -> "void"
    | Untagged_immediate -> "untagged_immediate"
    | Float64 -> "float64"
    | Float32 -> "float32"
    | Word -> "word"
    | Bits8 -> "bits8"
    | Bits16 -> "bits16"
    | Bits32 -> "bits32"
    | Bits64 -> "bits64"
    | Vec128 -> "vec128"
    | Vec256 -> "vec256"
    | Vec512 -> "vec512"
    | Mask -> "mask"

  let base_is_addressable = function
    | Scannable | Word | Bits64 | Vec128 | Vec256 | Vec512 | Mask -> true
    | Void | Untagged_immediate | Float64 | Float32 | Bits8 | Bits16 | Bits32 ->
      false

  let base_is_scannable = function
    | Scannable -> true
    | Void | Untagged_immediate | Float64 | Float32 | Word | Bits8 | Bits16
    | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 | Mask ->
      false

  (** The residual of [prop] for the term [Base b]: the components [Base b] is
      not automatically a fixed point of. A base is not mutable, so [Base b] is
      a fixed point of [prop] exactly when this is the identity. *)
  let prop_on_base prop b =
    let ({ addressability; scannable_axes } : Prop.t) = prop in
    Prop.create
      ~addressability:
        (if base_is_addressable b then Addressability.id else addressability)
      ~scannable_axes:
        (if base_is_scannable b then scannable_axes else Scannable_axes.id)

  (* Global association list mapping poly vars to names for printing *)
  let sort_poly_var_names : (var * string) list ref = ref []

  let to_string_genvar v =
    (* CR-soon zqian: raise if [v] is not found in [sort_poly_var_names],
       i.e. if this is called outside the dynamic extent of
       [print_with_genvars]. *)
    match List.assq_opt v !sort_poly_var_names with
    | Some name -> name
    | None -> "<genvar>"

  let print_with_genvar (v : var) callback =
    let saved = !sort_poly_var_names in
    let is_used s = List.exists (fun (_, name) -> name = s) saved in
    let find_name s =
      let rec loop idx =
        let name = s ^ string_of_int idx in
        if is_used name then loop (idx + 1) else name
      in
      if is_used s then loop 0 else s
    in
    let name = find_name "l" in
    sort_poly_var_names := (v, name) :: saved;
    Misc.try_finally
      (fun () -> callback name)
      ~always:(fun () -> sort_poly_var_names := saved)

  let print_with_genvars vars callback =
    let rec loop vars names_acc =
      match vars with
      | [] -> callback (List.rev names_acc)
      | v :: rest ->
        print_with_genvar v (fun name -> loop rest (name :: names_acc))
    in
    loop vars []

  module Const = struct
    type t =
      | Base of base
      | Product of t list
      | Univar of univar
      | Genvar of var
      | Addressable of t

    let base b = Base b

    let product cs = Product cs

    let univar uv = Univar uv

    let genvar v = Genvar v

    let rec equal c1 c2 =
      match c1, c2 with
      | Base b1, Base b2 -> equal_base b1 b2
      | Product cs1, Product cs2 -> List.equal equal cs1 cs2
      | Univar uv1, Univar uv2 -> equal_univar_univar uv1 uv2
      | Genvar v1, Genvar v2 -> v1.id = v2.id
      | Addressable c1, Addressable c2 ->
        (* Relies on constants not having redundant addressability wrappers. *)
        equal c1 c2
      | (Base _ | Product _ | Univar _ | Genvar _ | Addressable _), _ -> false

    let format ppf c =
      let module Fmt = Format_doc in
      let rec pp_element ~nested ppf = function
        | Base b -> Fmt.fprintf ppf "%s" (to_string_base b)
        | Product cs ->
          let pp_sep ppf () = Fmt.fprintf ppf "@ & " in
          Fmt.pp_nested_list ~nested ~pp_element ~pp_sep ppf cs
        | Univar { name = Some n } -> Fmt.fprintf ppf "%s" n
        | Univar { name = None } -> Fmt.fprintf ppf "_"
        | Genvar v -> Fmt.fprintf ppf "%s" (to_string_genvar v)
        | Addressable c ->
          Fmt.fprintf ppf "%a addressable" (pp_element ~nested:true) c
      in
      pp_element ~nested:false ppf c

    let rec all_void = function
      | Base Void -> true
      | Base
          ( Scannable | Untagged_immediate | Float64 | Float32 | Bits8 | Bits16
          | Bits32 | Bits64 | Word | Vec128 | Vec256 | Vec512 | Mask ) ->
        false
      | Univar _ -> Misc.fatal_error "Sort.Const.all_void: Univar"
      | Genvar _ -> Misc.fatal_error "Sort.Const.all_void: Genvar"
      | Product ts -> List.for_all all_void ts
      | Addressable t ->
        (* CR box: This may have to be updated once addressability affects boxed
           representations *)
        all_void t

    let rec is_surely_addressable = function
      | Base b -> base_is_addressable b
      | Product cs -> List.for_all is_surely_addressable cs
      | Univar _ | Genvar _ -> false
      | Addressable _ -> true

    (* Maintains invariant of no redundant [Addressable] constructors in
       constants *)
    let addressable c = if is_surely_addressable c then c else Addressable c

    let rec maybe_all_void = function
      | Base Void -> true
      | Base
          ( Scannable | Untagged_immediate | Float64 | Float32 | Bits8 | Bits16
          | Bits32 | Bits64 | Word | Vec128 | Vec256 | Vec512 | Mask ) ->
        false
      | Univar _ | Genvar _ -> true
      | Product ts -> List.for_all maybe_all_void ts
      | Addressable t -> maybe_all_void t

    let rec is_concrete = function
      | Base _ -> true
      | Product ts -> List.for_all is_concrete ts
      | Univar _ | Genvar _ -> false
      | Addressable t -> is_concrete t

    let scannable = Base Scannable

    let untagged_immediate = Base Untagged_immediate

    let void = Base Void

    let float64 = Base Float64

    let float32 = Base Float32

    let word = Base Word

    let bits8 = Base Bits8

    let bits16 = Base Bits16

    let bits32 = Base Bits32

    let bits64 = Base Bits64

    let vec128 = Base Vec128

    let vec256 = Base Vec256

    let vec512 = Base Vec512

    let mask = Base Mask

    module Debug_printers = struct
      let t ppf c =
        let rec pp_element ~nested ppf = function
          | Base b ->
            Format.fprintf ppf "%s"
              (match b with
              | Void -> "Void"
              | Scannable -> "Value"
              | Untagged_immediate -> "Untagged_immediate"
              | Float64 -> "Float64"
              | Float32 -> "Float32"
              | Word -> "Word"
              | Bits8 -> "Bits8"
              | Bits16 -> "Bits16"
              | Bits32 -> "Bits32"
              | Bits64 -> "Bits64"
              | Vec128 -> "Vec128"
              | Vec256 -> "Vec256"
              | Vec512 -> "Vec512"
              | Mask -> "Mask")
          | Product cs ->
            let pp_sep ppf () = Format.fprintf ppf "@ , " in
            Format.fprintf ppf "Product [%a]"
              (Misc.pp_nested_list ~nested ~pp_element ~pp_sep)
              cs
          | Univar { name = Some n } -> Format.fprintf ppf "Univar '%s" n
          | Univar { name = None } -> Format.fprintf ppf "Univar '_"
          | Genvar v -> Format.fprintf ppf "Genvar %d" v.id
          | Addressable c ->
            Format.fprintf ppf "Addressable (%a)" (pp_element ~nested:false) c
        in
        pp_element ~nested:false ppf c
    end

    let for_function = scannable

    let for_predef_scannable = scannable

    let for_block_element = scannable

    let for_boxed_record = scannable

    let for_object = scannable

    let for_lazy_body = scannable

    let for_tuple_element = scannable

    let for_variant_arg = scannable

    let for_instance_var = scannable

    let for_class_arg = scannable

    let for_module = scannable

    let for_tuple = scannable

    let for_array_comprehension_element = scannable

    let for_list_element = scannable

    let for_loop_index = scannable

    let for_constructor = scannable

    let for_boxed_variant = scannable

    let for_exception = scannable

    let for_type_extension = scannable

    let for_class = scannable

    let for_effect = scannable

    let for_continuation = scannable

    (* Pre-allocated [Some]-wrappings of the base sort constants, evaluated
       once at module initialization and shared by [some] /
       [some_of_base] to avoid allocating a fresh [Some] block per
       call. Not exposed: callers go through [some]. *)
    let some_scannable = Some scannable

    let some_void = Some void

    let some_untagged_immediate = Some untagged_immediate

    let some_float64 = Some float64

    let some_float32 = Some float32

    let some_word = Some word

    let some_bits8 = Some bits8

    let some_bits16 = Some bits16

    let some_bits32 = Some bits32

    let some_bits64 = Some bits64

    let some_vec128 = Some vec128

    let some_vec256 = Some vec256

    let some_vec512 = Some vec512

    let some_mask = Some mask

    let[@inline] some_of_base = function
      | Scannable -> some_scannable
      | Void -> some_void
      | Untagged_immediate -> some_untagged_immediate
      | Float64 -> some_float64
      | Float32 -> some_float32
      | Word -> some_word
      | Bits8 -> some_bits8
      | Bits16 -> some_bits16
      | Bits32 -> some_bits32
      | Bits64 -> some_bits64
      | Vec128 -> some_vec128
      | Vec256 -> some_vec256
      | Vec512 -> some_vec512
      | Mask -> some_mask

    let[@inline] some : t -> t option = function
      | Base b -> some_of_base b
      | (Product _ | Univar _ | Genvar _ | Addressable _) as t -> Some t
  end

  module Var = struct
    type id = int

    let get_id { id; _ } = id

    let is_cmi_var { id; _ } = id < 0

    let is_root { contents; _ } = Option.is_none contents

    (* Map var ids to smaller numbers for more consistent printing. *)
    let next_id = ref 1

    let names : (int, int) Hashtbl.t = Hashtbl.create 16

    let get_print_number id =
      match Hashtbl.find_opt names id with
      | Some n -> n
      | None ->
        let counter = !next_id in
        incr next_id;
        Hashtbl.add names id counter;
        counter

    let name { id; _ } =
      "'_representable_layout_" ^ Int.to_string (get_print_number id)
  end

  (*** debug printing **)
  module Debug_printers = struct
    open Format

    let base ppf b =
      fprintf ppf "%s"
        (match b with
        | Void -> "Void"
        | Scannable -> "Value"
        | Untagged_immediate -> "Untagged_immediate"
        | Float64 -> "Float64"
        | Float32 -> "Float32"
        | Word -> "Word"
        | Bits8 -> "Bits8"
        | Bits16 -> "Bits16"
        | Bits32 -> "Bits32"
        | Bits64 -> "Bits64"
        | Vec128 -> "Vec128"
        | Vec256 -> "Vec256"
        | Vec512 -> "Vec512"
        | Mask -> "Mask")

    let rec t ppf { prop; data } =
      match Prop.to_string_list prop with
      | [] -> t_data ppf data
      | strs -> fprintf ppf "%a %s" t_data data (String.concat " " strs)

    and t_data ppf = function
      | Var v -> fprintf ppf "Var %a" var v
      | Base b -> base ppf b
      | Product ts ->
        fprintf ppf "Product [ %a ]"
          (pp_print_list ~pp_sep:(fun ppf () -> pp_print_text ppf "; ") t)
          ts
      | Univar { name = Some n } -> fprintf ppf "Univar '%s" n
      | Univar { name = None } -> fprintf ppf "Univar '_"

    and opt_t ppf = function
      | Some s -> fprintf ppf "Some %a" t s
      | None -> fprintf ppf "None"

    and var ppf v =
      fprintf ppf "{@[@ contents = %a;@ id = %d@ @]}" opt_t v.contents v.id
  end

  (* To record changes to sorts, for use with `Types.{snapshot, backtrack}` *)
  type sort_change =
    | Ccontents of t option
    | Clevel of int

  type change = var * sort_change

  let change_log : (change -> unit) ref = ref (fun _ -> ())

  let set_change_log cl = change_log := cl

  let log_change change = !change_log change

  let undo_change (v, ch) =
    match ch with
    | Ccontents t_op -> v.contents <- t_op
    | Clevel level -> v.level <- level

  let rec update_level level { prop = _; data } =
    match data with
    | Var v -> update_level_var level v
    | Base _ | Univar _ -> ()
    | Product ts -> List.iter (update_level level) ts

  and update_level_var level u =
    match u.contents with
    | Some t -> update_level level t
    | None ->
      let new_level = min level u.level in
      if u.level <> new_level
      then (
        log_change (u, Clevel u.level);
        u.level <- new_level)

  let[@inline] set_without_level : var -> t option -> unit =
   fun v t_op ->
    log_change (v, Ccontents v.contents);
    v.contents <- t_op

  let[@inline] set : var -> t option -> unit =
   fun v t_op ->
    assert (Option.is_none v.contents);
    (* [t_op] is always [Some _]. Takes [option] only for performance. *)
    let t = Option.get t_op in
    (* [v.level] is meaningful and should affect all variables in [t]. *)
    update_level v.level t;
    (* [v.contents] is set, which renders [v.level] meaningless, so we don't
       need to update that. *)
    set_without_level v t_op

  let[@inline] set_to_compress : var -> t option -> unit =
   fun v t_op ->
    assert (Option.is_some v.contents);
    (* [v.contents] is [Some _], hence [v.level] safe to ignore *)
    set_without_level v t_op

  module Static = struct
    (* Statically allocated values of various consts and sorts to save
       allocations in in the core hot path functions. [T] is also included in
       the outer module to provide the core sorts. *)

    module T = struct
      let void = of_data (Base Void)

      let scannable = of_data (Base Scannable)

      let untagged_immediate = of_data (Base Untagged_immediate)

      let float64 = of_data (Base Float64)

      let float32 = of_data (Base Float32)

      let word = of_data (Base Word)

      let bits8 = of_data (Base Bits8)

      let bits16 = of_data (Base Bits16)

      let bits32 = of_data (Base Bits32)

      let bits64 = of_data (Base Bits64)

      let vec128 = of_data (Base Vec128)

      let vec256 = of_data (Base Vec256)

      let vec512 = of_data (Base Vec512)

      let mask = of_data (Base Mask)

      let of_base = function
        | Void -> void
        | Scannable -> scannable
        | Untagged_immediate -> untagged_immediate
        | Float64 -> float64
        | Float32 -> float32
        | Word -> word
        | Bits8 -> bits8
        | Bits16 -> bits16
        | Bits32 -> bits32
        | Bits64 -> bits64
        | Vec128 -> vec128
        | Vec256 -> vec256
        | Vec512 -> vec512
        | Mask -> mask

      let rec of_const : Const.t -> t = function
        | Base b -> of_base b
        | Product cs -> of_data (Product (List.map of_const cs))
        | Univar uv -> of_data (Univar uv)
        | Genvar v -> of_data (Var v)
        | Addressable c -> apply_prop Prop.addressable (of_const c)
    end

    module T_option = struct
      (* Pre-allocated [Some]-wrappings, to avoid allocating a fresh [Some]
         block when filling in a sort variable. *)
      let scannable = Some T.scannable
    end

    module Const = struct
      open Const

      let scannable = Base Scannable

      let void = Base Void

      let untagged_immediate = Base Untagged_immediate

      let float64 = Base Float64

      let float32 = Base Float32

      let word = Base Word

      let bits8 = Base Bits8

      let bits16 = Base Bits16

      let bits32 = Base Bits32

      let bits64 = Base Bits64

      let vec128 = Base Vec128

      let vec256 = Base Vec256

      let vec512 = Base Vec512

      let mask = Base Mask

      let of_base : base -> Const.t = function
        | Scannable -> scannable
        | Void -> void
        | Untagged_immediate -> untagged_immediate
        | Float64 -> float64
        | Float32 -> float32
        | Word -> word
        | Bits8 -> bits8
        | Bits16 -> bits16
        | Bits32 -> bits32
        | Bits64 -> bits64
        | Vec128 -> vec128
        | Vec256 -> vec256
        | Vec512 -> vec512
        | Mask -> mask
    end
  end

  let of_var v = of_data (Var v)

  let of_univar uv = of_data (Univar uv)

  let last_var_id = ref 0

  let last_var_cmi_id = ref 0

  let reset_cmi_sort_id () = last_var_cmi_id := 0

  let new_var_unsafe ~level =
    incr last_var_id;
    { contents = None; level; id = !last_var_id }

  let new_var ~level =
    (* Guard against accidentally creating a genvar or rigidvar via this path:
       those require special handling (instance_map registration for genvars;
       refusal to unify for rigidvars). [level_fresh] is intentionally
       not guarded here — it behaves like any other unifiable variable and its
       level is simply lowered by [update_level] upon unification. *)
    if level >= level_rigid
    then Misc.fatal_error "Jkind_types.new_var: level >= level_rigid";
    new_var_unsafe ~level

  let new_genvar () = new_var_unsafe ~level:level_generic

  let new_genvar_for_cmi () =
    decr last_var_cmi_id;
    { contents = None; level = level_generic; id = !last_var_cmi_id }

  let new_rigidvar () = new_var_unsafe ~level:level_rigid

  let instance_map : (var * var) list ref = ref []

  let instance_with ~level vars f =
    let new_vars =
      List.map
        (fun v ->
          assert (is_genvar v);
          (* ensure the variable is not a CMI serialised variable *)
          assert (v.id > 0);
          let v' = new_var_unsafe ~level in
          v, v')
        vars
    in
    let old_map = !instance_map in
    instance_map := new_vars @ old_map;
    Misc.try_finally
      (fun () ->
        let result = f () in
        List.map snd new_vars, result)
      ~always:(fun () -> instance_map := old_map)

  let rec instance_var v =
    match v.contents with
    | None when is_genvar v ->
      begin match List.assq_opt v !instance_map with
      | Some v' -> of_data (Var v')
      | None ->
        (* If the caller didn't set up layout instantiation, conservatively
           return a rigid variable (which is not equal to anything) *)
        (* CR-someday zqian: explicitly distinguish among three cases:
        - instantiating layouts properly
        - knowingly instantiating to rigidvar conservatively
        - unknown context, in which case we should crash *)
        of_data (Var (new_rigidvar ()))
      end
    | None -> of_data (Var v)
    | Some t -> instance t

  and instance : t -> t = function
    | { prop; data = Var v } -> apply_prop prop (instance_var v)
    | { prop = _; data = Base _ | Univar _ } as s -> s
    | { prop; data = Product ts } ->
      { prop; data = Product (List.map instance ts) }

  let rec get : t -> t = function
    | { prop = _; data = Base _ | Univar _ } as t -> t
    | { prop; data = Product ts } as t ->
      let ts' = List.map get ts in
      if List.for_all2 ( == ) ts ts' then t else { prop; data = Product ts' }
    | { prop; data = Var r } as t -> (
      match r.contents with
      | None -> t
      | Some s ->
        let result = get s in
        if result != s then set_to_compress r (Some result);
        (* path compression *)
        apply_prop prop result)

  let rec get_representable : t -> t option = function
    | { prop = _; data = Base _ | Univar _ } as t -> Some t
    | { prop; data = Product ts } ->
      begin match get_representable_product ts with
      | None -> None
      | Some ts' -> Some { prop; data = Product ts' }
      end
    | { prop; data = Var v } ->
      Option.map (apply_prop prop) (get_representable_var v)

  and get_representable_product : t list -> t list option =
   fun ts ->
    List.fold_right
      (fun t acc ->
        match acc, get_representable t with
        | None, _ | _, None -> None
        | Some ts, Some t -> Some (t :: ts))
      ts (Some [])

  and get_representable_var : var -> t option =
   fun v ->
    match v.contents with
    | None ->
      begin if is_rigidvar v then Some (of_data (Var v)) else None
      end
    | Some t -> get_representable t

  (** Split [t] into the operator applied at its head — following through filled
      variables, whose contents may apply further operators — and the
      operator-free remainder. *)
  let rec split_head_prop : t -> Prop.t * t = function
    | { prop; data = Var { contents = Some s; _ } } ->
      let prop', s' = split_head_prop s in
      Prop.compose prop prop', s'
    | { prop; data } as t ->
      if Prop.is_id prop then prop, t else prop, { prop = Prop.id; data }

  let strip_head_prop t = snd (split_head_prop t)

  (** The operator applied at the head of [t]. *)
  let head_prop t = fst (split_head_prop t)

  let rec subst s t =
    match t.data with
    | Var v ->
      begin match v.contents with
      | None ->
        begin match List.assq_opt v s with
        | Some t' -> apply_prop t.prop t'
        | None -> t
        end
      | Some t' -> apply_prop t.prop (subst s t')
      end
    | Base _ | Univar _ -> t
    | Product ts -> { t with data = Product (List.map (subst s) ts) }

  (* Sort generalization context for let poly_ *)
  let in_sort_generalization_context : var list ref option ref = ref None

  (* Generalize sort variables when in sort generalization context.
     This is called from Ctype.generalize when processing let poly_ bindings.
     For each free sort variable, the level is set to Ident.highest_scope,
     making it a generic sort variable (genvar), and the var is accumulated. *)
  let rec generalize_rec ~current_level ~vars_ref sort =
    match sort.data with
    | Var v ->
      assert (Option.is_none v.contents);
      if v.level > current_level && v.level <> Ident.highest_scope
      then begin
        v.level <- Ident.highest_scope;
        vars_ref := v :: !vars_ref
      end
    | Product sorts -> List.iter (generalize_rec ~current_level ~vars_ref) sorts
    | Base _ | Univar _ -> ()

  let generalize ~current_level sort =
    match !in_sort_generalization_context with
    | None -> () (* Not in generalization context *)
    | Some vars_ref -> generalize_rec ~current_level ~vars_ref (get sort)

  (* Wrapper to run a function in sort generalization context. Returns the
     result of [f] and the vars generalized during [f]. *)
  let generalize_with f =
    let vars_ref = ref [] in
    let old_context = !in_sort_generalization_context in
    in_sort_generalization_context := Some vars_ref;
    let result =
      Misc.try_finally f ~always:(fun () ->
          in_sort_generalization_context := old_context)
    in
    result, List.rev !vars_ref

  (* [Sort.Const.t] has no place to record scannable axes, so that component of
     the operator is dropped here; only addressability survives. *)
  let const_apply_prop (prop : Prop.t) (c : Const.t) =
    if Prop.is_addressable prop then Const.addressable c else c

  (* Fills in every unfilled variable with [scannable], applying path
     compression along the way. Genvars are left alone. Note that this must not
     go via [Const.t], which cannot record scannable axes: compressing to a
     lossy sort would silently drop them. *)
  let rec default_to_scannable (t : t) : t =
    match t.data with
    | Base _ | Univar _ -> t
    | Product ts ->
      let ts' = List.map default_to_scannable ts in
      if List.for_all2 ( == ) ts ts' then t else { t with data = Product ts' }
    | Var r -> apply_prop t.prop (var_default_to_scannable r)

  and var_default_to_scannable r : t =
    match r.contents with
    | None when is_genvar r -> of_data (Var r)
    | None when is_rigidvar r ->
      Misc.fatal_error
        "Jkind_types.default_to_scannable: cannot default rigid variables"
    | None ->
      set r Static.T_option.scannable;
      Static.T.scannable
    | Some s ->
      let result = default_to_scannable s in
      if result != s then set_to_compress r (Some result);
      (* path compression *)
      result

  (* Pre-condition: [t] has been defaulted, so its only unfilled variables are
     genvars. *)
  let rec const_of_defaulted ({ prop; data } : t) : Const.t =
    const_apply_prop prop
      (match data with
      | Base b -> Static.Const.of_base b
      | Product ts -> Product (List.map const_of_defaulted ts)
      | Univar uv -> Const.Univar uv
      | Var r -> (
        match r.contents with
        | None when is_genvar r -> Genvar r
        | None ->
          Misc.fatal_error
            "Jkind_types.const_of_defaulted: unfilled sort variable"
        | Some s -> const_of_defaulted s))

  let default_to_scannable_and_get t =
    const_of_defaulted (default_to_scannable t)

  let var_default_to_scannable_and_get r =
    const_of_defaulted (var_default_to_scannable r)

  let get_concrete_defaulting_to_scannable s =
    let const = default_to_scannable_and_get s in
    if Const.is_concrete const then Const.some const else None

  (* CR layouts v12: Default to void instead. *)
  let default_for_transl_and_get s = default_to_scannable_and_get s

  let rec to_const_opt ({ prop; data } : t) : Const.t option =
    Option.map (const_apply_prop prop)
      (match data with
      | Base b -> Some (Static.Const.of_base b)
      | Product ts ->
        Misc.Stdlib.List.map_option to_const_opt ts
        |> Option.map (fun cs : Const.t -> Const.Product cs)
      | Univar uv -> Some (Const.Univar uv)
      | Var r -> (
        match r.contents with None -> None | Some s -> to_const_opt s))

  let is_scannable_or_var s =
    match (get s).data with
    | Base Scannable | Var _ -> true
    | Base _ | Product _ | Univar _ -> false

  (***********************)
  (* equality *)

  type equate_result =
    | Unequal
    | Equal_mutated_first
    | Equal_mutated_second
    | Equal_mutated_both
    | Equal_no_mutation

  let join_equate_result r1 r2 =
    match r1, r2 with
    | Unequal, _ | _, Unequal -> Unequal
    | Equal_no_mutation, r | r, Equal_no_mutation -> r
    | Equal_mutated_both, _ | _, Equal_mutated_both -> Equal_mutated_both
    | Equal_mutated_first, Equal_mutated_first -> Equal_mutated_first
    | Equal_mutated_second, Equal_mutated_second -> Equal_mutated_second
    | Equal_mutated_first, Equal_mutated_second
    | Equal_mutated_second, Equal_mutated_first ->
      Equal_mutated_both

  type constrain_result =
    | Constrained_mutated
    | Constrained_no_mutation
    | Not_constrained

  let join_constrain_result r1 r2 =
    match r1, r2 with
    | Not_constrained, _ | _, Not_constrained -> Not_constrained
    | Constrained_mutated, _ | _, Constrained_mutated -> Constrained_mutated
    | Constrained_no_mutation, Constrained_no_mutation ->
      Constrained_no_mutation

  (* [constrain_fixpoint ~prop t] establishes [t = prop t], i.e. that [t]
     satisfies the property that [prop] enforces, mutating sort variables where
     that is allowed and necessary. See Note [Kind properties] in
     [jkind_intf.ml]. *)
  let rec constrain_fixpoint ~allow_mutation ~prop t =
    let prop = Prop.residual ~have:t.prop prop in
    if Prop.is_id prop
    then Constrained_no_mutation
    else constrain_data_fixpoint ~allow_mutation ~prop t.data

  (* Here [prop] is a residual, and in particular not the identity. *)
  and constrain_data_fixpoint ~allow_mutation ~prop = function
    | Base b ->
      (* A base is not mutable, so it either already is a fixed point or the
         constraint is unsatisfiable. *)
      if Prop.is_id (prop_on_base prop b)
      then Constrained_no_mutation
      else Not_constrained
    | Product ts ->
      let prop = Prop.on_unscannable prop in
      if Prop.is_id prop
      then Constrained_no_mutation
      else
        List.fold_left
          (fun acc t ->
            match acc with
            | Not_constrained -> Not_constrained
            | Constrained_mutated | Constrained_no_mutation ->
              join_constrain_result acc
                (constrain_fixpoint ~allow_mutation ~prop t))
          Constrained_no_mutation ts
    | Univar _ ->
      if Prop.is_id (Prop.on_unscannable prop)
      then Constrained_no_mutation
      else Not_constrained
    | Var v -> (
      match v.contents with
      | Some s -> constrain_fixpoint ~allow_mutation ~prop s
      | None when is_rigidvar v -> Not_constrained
      | None when not allow_mutation -> Not_constrained
      | None ->
        set v (Some { prop; data = Var (new_var ~level:level_fresh) });
        Constrained_mutated)

  let is_surely_fixpoint ~prop t =
    match constrain_fixpoint ~allow_mutation:false ~prop t with
    | Not_constrained -> false
    | Constrained_no_mutation | Constrained_mutated -> true

  let is_surely_addressable t = is_surely_fixpoint ~prop:Prop.addressable t

  (** The components of [t]'s head operator that are not already implied by
      [t]'s structure: the modifiers that need printing. *)
  let visible_prop t =
    if Prop.is_id t.prop
    then Prop.id
    else
      let ({ addressability; scannable_axes } : Prop.t) = t.prop in
      let stripped = strip_head_prop t in
      let addressability : Addressability.t =
        match addressability with
        | Id -> Id
        | Addressable ->
          if is_surely_fixpoint ~prop:Prop.addressable stripped
          then Id
          else Addressable
      in
      let scannable_axes =
        if is_scannable_or_var stripped
        then scannable_axes
        else Scannable_axes.id
      in
      Prop.create ~addressability ~scannable_axes

  let constraining equate_result ~prop x f =
    if Prop.is_id prop
    then f ()
    else
      match constrain_fixpoint ~allow_mutation:true ~prop x with
      | Not_constrained -> Unequal
      | Constrained_no_mutation -> f ()
      | Constrained_mutated -> join_equate_result equate_result (f ())

  (* To solve [p1 d1 = p2 d2] we first constrain each side to be a fixed point
     of the other side's operator, and then reduce to [d1 = d2]. That last step
     is incomplete when a side is a variable:

     Consider [s1 = 'var addressable] and [s2 = bits8 addressable]. We could
     unify ['var = bits8] or ['var = bits8 addressable], but neither is more
     general. See Note [Kind properties] in [jkind_intf.ml]. *)
  let rec equate s1 s2 =
    match s1.data, s2.data with
    (* The same variable on both sides: comparing it against itself would set
       it to a sort containing itself, so handle it before the cases that fill
       variables in. *)
    | Var v1, Var v2 when v1.id = v2.id ->
      if Prop.equal s1.prop s2.prop
      then Equal_no_mutation
      else equate_discharging_props s1 s2
    | Var { contents = Some s1'; _ }, _ -> equate (apply_prop s1.prop s1') s2
    | _, Var { contents = Some s2'; _ } -> equate s1 (apply_prop s2.prop s2')
    | Var ({ contents = None; _ } as v1), _
      when Prop.is_id s1.prop && not (is_rigidvar v1) ->
      set v1 (Some s2);
      Equal_mutated_first
    | _, Var ({ contents = None; _ } as v2)
      when Prop.is_id s2.prop && not (is_rigidvar v2) ->
      set v2 (Some s1);
      Equal_mutated_second
    | _ ->
      if Prop.is_id s1.prop && Prop.is_id s2.prop
      then equate_data s1 s2
      else equate_discharging_props s1 s2

  and equate_discharging_props s1 s2 =
    let p1 = head_prop s1 and p2 = head_prop s2 in
    constraining Equal_mutated_first ~prop:p2 s1 (fun () ->
        constraining Equal_mutated_second ~prop:p1 s2 (fun () ->
            (* Strip only now: constraining may have filled variables in with
               sorts that themselves carry operators. *)
            equate_data (strip_head_prop s1) (strip_head_prop s2)))

  (* Precondition: the head operators of [s1] and [s2] are the identity, having
     already been discharged by [equate]. *)
  and equate_data s1 s2 =
    match s1.data, s2.data with
    | Var v1, Var v2 when v1.id = v2.id -> Equal_no_mutation
    | Var { contents = Some s1'; _ }, _ -> equate s1' s2
    | _, Var { contents = Some s2'; _ } -> equate s1 s2'
    | Var ({ contents = None; _ } as v1), _ when not (is_rigidvar v1) ->
      set v1 (Some s2);
      Equal_mutated_first
    | _, Var ({ contents = None; _ } as v2) when not (is_rigidvar v2) ->
      set v2 (Some s1);
      Equal_mutated_second
    | Var _, _ | _, Var _ ->
      (* rigid *)
      Unequal
    | Base b1, Base b2 ->
      if equal_base b1 b2 then Equal_no_mutation else Unequal
    | Product sorts1, Product sorts2 -> equate_list sorts1 sorts2
    | Univar uv1, Univar uv2 ->
      if equal_univar_univar uv1 uv2 then Equal_no_mutation else Unequal
    | (Base _ | Product _ | Univar _), (Base _ | Product _ | Univar _) ->
      Unequal

  and equate_list sorts1 sorts2 =
    let rec go sorts1 sorts2 acc =
      match sorts1, sorts2, acc with
      | _, _, Unequal -> Unequal
      | _ :: _, [], _ -> Unequal
      | [], _ :: _, _ -> Unequal
      | [], [], acc -> acc
      | sort1 :: sorts1, sort2 :: sorts2, acc ->
        go sorts1 sorts2 (join_equate_result acc (equate sort1 sort2))
    in
    go sorts1 sorts2 Equal_no_mutation

  let equate_tracking_mutation = equate

  (* Don't expose whether or not mutation happened; we just need that for
     [Jkind] *)
  let equate s1 s2 =
    match equate_tracking_mutation s1 s2 with
    | Unequal -> false
    | Equal_mutated_first | Equal_mutated_second | Equal_no_mutation
    | Equal_mutated_both ->
      true

  let decompose_into_product t n =
    let ts = List.init n (fun _ -> of_var (new_var ~level:level_fresh)) in
    if equate t (of_data (Product ts)) then Some ts else None

  (*** pretty printing ***)

  let format ppf t =
    let module Fmt = Format_doc in
    let rec pp_element ~nested ppf t =
      let t = get t in
      match Prop.to_string_list (visible_prop t) with
      | [] -> pp_data ~nested ppf t.data
      | strs ->
        Fmt.fprintf ppf "%a %s" (pp_data ~nested:true) t.data
          (String.concat " " strs)
    and pp_data ~nested ppf = function
      | Base b -> Fmt.fprintf ppf "%s" (to_string_base b)
      | Var v -> Fmt.fprintf ppf "%s" (Var.name v)
      | Product ts ->
        let pp_sep ppf () = Fmt.fprintf ppf " & " in
        Fmt.pp_nested_list ~nested ~pp_element ~pp_sep ppf ts
      | Univar { name = Some n } -> Fmt.fprintf ppf "%s" n
      | Univar { name = None } -> Fmt.fprintf ppf "_"
    in
    pp_element ~nested:false ppf t

  include Static.T

  module Flat = struct
    type t =
      | Var of Var.id
      | Genvar of var
      | Univar of univar
      | Base of base
  end
end

module Layout = struct
  open Jkind_axis

  (* Like [Sort.t], a layout [{ prop; data }] denotes the property-enforcing
     operator [prop] applied to [data]. When [data] is [Sort s], [prop] composes
     with [s]'s own operator. See Note [Kind properties] in [jkind_intf.ml]. *)
  type 'sort data =
    | Sort of 'sort
    | Product of 'sort t list
    | Any

  and 'sort t =
    { prop : Prop.t;
      data : 'sort data
    }

  let[@inline] of_data data = { prop = Prop.id; data }

  let[@inline] apply_prop prop t =
    if Prop.is_id prop then t else { t with prop = Prop.compose prop t.prop }

  let[@inline] of_sort s = of_data (Sort s)

  let[@inline] any prop = { prop; data = Any }

  module Const = struct
    type t =
      | Any of Scannable_axes.t
      | Base of Sort.base * Scannable_axes.t
      | Product of t list
      | Univar of Sort.univar
      | Genvar of Sort.var
      | Addressable of t

    let any sa = Any sa

    let product cs = Product cs

    let univar uv = Univar uv

    let genvar v = Genvar v

    let max = Any Scannable_axes.max

    let rec equal c1 c2 =
      match c1, c2 with
      | Base (Scannable, sa1), Base (Scannable, sa2) ->
        Scannable_axes.equal sa1 sa2
      | Base (b1, _), Base (b2, _) -> Sort.equal_base b1 b2
      | Any sa1, Any sa2 -> Scannable_axes.equal sa1 sa2
      | Product cs1, Product cs2 -> List.equal equal cs1 cs2
      | Univar uv1, Univar uv2 -> Sort.equal_univar_univar uv1 uv2
      | Genvar v1, Genvar v2 -> v1.id = v2.id
      | Addressable c1, Addressable c2 ->
        (* Relies on invariant that constants don't have redundant [Addressable] *)
        equal c1 c2
      | (Base _ | Any _ | Product _ | Univar _ | Genvar _ | Addressable _), _ ->
        false

    let rec get_sort : t -> Sort.Const.t option = function
      | Any _ -> None
      | Base (b, _) -> Sort.Const.some (Base b)
      | Product ts ->
        Option.map
          (fun x -> Sort.Const.Product x)
          (Misc.Stdlib.List.map_option get_sort ts)
      | Univar uv -> Some (Sort.Const.Univar uv)
      | Genvar v -> Some (Sort.Const.Genvar v)
      | Addressable t -> Option.map Sort.Const.addressable (get_sort t)

    let rec is_scannable_or_any = function
      | Any _ | Base (Scannable, _) -> true
      | Base
          ( ( Void | Untagged_immediate | Float64 | Float32 | Word | Bits8
            | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 | Mask ),
            _ ) ->
        false
      | Product _ -> false
      | Univar _ -> false
      | Genvar _ -> false
      | Addressable t -> is_scannable_or_any t

    let rec is_surely_addressable = function
      | Base (b, _) -> Sort.base_is_addressable b
      | Product cs -> List.for_all is_surely_addressable cs
      | Any _ | Univar _ | Genvar _ -> false
      | Addressable _ -> true

    let addressable c = if is_surely_addressable c then c else Addressable c

    let apply_addressability c : Addressability.t -> t = function
      | Id -> c
      | Addressable -> addressable c

    let rec get_root_scannable_axes t =
      match t with
      | Any sa -> Some sa
      | Base (_, sa) -> if is_scannable_or_any t then Some sa else None
      | Product _ -> None
      | Univar _ -> None
      | Genvar _ -> None
      | Addressable t -> get_root_scannable_axes t

    let rec set_root_scannable_axes t sa =
      match t with
      | Any _ -> Any sa
      | Base (b, _) -> if is_scannable_or_any t then Base (b, sa) else t
      | Product _ -> t
      | Univar _ -> t
      | Genvar _ -> t
      | Addressable t' -> Addressable (set_root_scannable_axes t' sa)

    let meet_root_scannable_axes t sa =
      if Scannable_axes.is_id sa
      then t
      else
        match get_root_scannable_axes t with
        | None -> t
        | Some sa' ->
          let sa'' = Scannable_axes.meet sa sa' in
          (* Preserve physical equality when nothing changes; callers use it to
             avoid reallocating jkinds. *)
          if Scannable_axes.equal sa'' sa'
          then t
          else set_root_scannable_axes t sa''

    let apply_prop c (prop : Prop.t) =
      apply_addressability
        (meet_root_scannable_axes c prop.scannable_axes)
        prop.addressability

    module Static = struct
      let scannable_non_null_non_pointer =
        Base
          ( Sort.Scannable,
            { nullability = Non_null; separability = Non_pointer } )

      let scannable_non_null_non_pointer64 =
        Base
          ( Sort.Scannable,
            { nullability = Non_null; separability = Non_pointer64 } )

      let scannable_non_null_non_float =
        Base
          (Sort.Scannable, { nullability = Non_null; separability = Non_float })

      let scannable_non_null_separable =
        Base
          (Sort.Scannable, { nullability = Non_null; separability = Separable })

      let scannable_non_null_maybe_separable =
        Base
          ( Sort.Scannable,
            { nullability = Non_null; separability = Maybe_separable } )

      let scannable_maybe_null_non_pointer =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Non_pointer } )

      let scannable_maybe_null_non_pointer64 =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Non_pointer64 } )

      let scannable_maybe_null_non_float =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Non_float } )

      let scannable_maybe_null_separable =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Separable } )

      let scannable_maybe_null_maybe_separable =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Maybe_separable } )

      (* For all non-[Scannable] layouts, the scannable axes are ignored. We
         have to pick something, though, so we pick [Scannable_axes.max]. *)

      let void = Base (Sort.Void, Scannable_axes.max)

      let float64 = Base (Sort.Float64, Scannable_axes.max)

      let float32 = Base (Sort.Float32, Scannable_axes.max)

      let word = Base (Sort.Word, Scannable_axes.max)

      let untagged_immediate = Base (Sort.Untagged_immediate, Scannable_axes.max)

      let bits8 = Base (Sort.Bits8, Scannable_axes.max)

      let bits16 = Base (Sort.Bits16, Scannable_axes.max)

      let bits32 = Base (Sort.Bits32, Scannable_axes.max)

      let bits64 = Base (Sort.Bits64, Scannable_axes.max)

      let vec128 = Base (Sort.Vec128, Scannable_axes.max)

      let vec256 = Base (Sort.Vec256, Scannable_axes.max)

      let vec512 = Base (Sort.Vec512, Scannable_axes.max)

      let mask = Base (Sort.Mask, Scannable_axes.max)

      let of_base (b : Sort.base) (sa : Scannable_axes.t) =
        match b, sa with
        | Scannable, sa -> (
          match sa with
          | { nullability = Nullability.Non_null;
              separability = Separability.Non_pointer
            } ->
            scannable_non_null_non_pointer
          | { nullability = Nullability.Non_null;
              separability = Separability.Non_pointer64
            } ->
            scannable_non_null_non_pointer64
          | { nullability = Nullability.Non_null;
              separability = Separability.Non_float
            } ->
            scannable_non_null_non_float
          | { nullability = Nullability.Non_null;
              separability = Separability.Separable
            } ->
            scannable_non_null_separable
          | { nullability = Nullability.Non_null;
              separability = Separability.Maybe_separable
            } ->
            scannable_non_null_maybe_separable
          | { nullability = Nullability.Maybe_null;
              separability = Separability.Non_pointer
            } ->
            scannable_maybe_null_non_pointer
          | { nullability = Nullability.Maybe_null;
              separability = Separability.Non_pointer64
            } ->
            scannable_maybe_null_non_pointer64
          | { nullability = Nullability.Maybe_null;
              separability = Separability.Non_float
            } ->
            scannable_maybe_null_non_float
          | { nullability = Nullability.Maybe_null;
              separability = Separability.Separable
            } ->
            scannable_maybe_null_separable
          | { nullability = Nullability.Maybe_null;
              separability = Separability.Maybe_separable
            } ->
            scannable_maybe_null_maybe_separable)
        | Void, _ -> void
        | Untagged_immediate, _ -> untagged_immediate
        | Float64, _ -> float64
        | Float32, _ -> float32
        | Word, _ -> word
        | Bits8, _ -> bits8
        | Bits16, _ -> bits16
        | Bits32, _ -> bits32
        | Bits64, _ -> bits64
        | Vec128, _ -> vec128
        | Vec256, _ -> vec256
        | Vec512, _ -> vec512
        | Mask, _ -> mask
    end

    (* [prop] is the operator the enclosing layout applies to [s]; it composes
       with the operators [s] itself carries. *)
    let of_sort s prop =
      let rec of_sort (s : Sort.t) prop =
        let prop = Prop.compose prop s.prop in
        let const =
          match s.data with
          | Var v when Sort.is_genvar v -> Some (Genvar v)
          | Var _ -> None
          | Base b -> Some (Static.of_base b prop.Prop.scannable_axes)
          | Product sorts ->
            Option.map
              (fun x -> Product x)
              (* [Sort.get] is deep, so no need to repeat it here *)
              (* Scannable axes are meaningless on a product, so the components
                 are converted with the identity operator. *)
              (Misc.Stdlib.List.map_option (fun s -> of_sort s Prop.id) sorts)
          | Univar uv -> Some (Univar uv)
        in
        Option.map
          (fun c -> apply_addressability c prop.Prop.addressability)
          const
      in
      of_sort (Sort.get s) prop

    let of_univar uv = Univar uv

    let of_flat_sort (s : Sort.Flat.t) (prop : Prop.t) =
      let const =
        match s with
        | Var _ -> None
        | Genvar v -> Some (Genvar v)
        | Univar uv -> Some (of_univar uv)
        | Base b -> Some (Static.of_base b prop.scannable_axes)
      in
      Option.map (fun c -> apply_addressability c prop.addressability) const
  end

  let rec of_const (const : Const.t) : _ t =
    match const with
    | Any sa -> any (Prop.of_scannable_axes sa)
    | Base (b, sa) ->
      { prop = Prop.of_scannable_axes sa; data = Sort (Sort.of_base b) }
    | Product cs -> of_data (Product (List.map of_const cs))
    | Univar uv -> of_sort (Sort.of_univar uv)
    | Genvar v -> of_sort (Sort.of_var v)
    | Addressable c -> apply_prop Prop.addressable (of_const c)

  let product = function
    | [] -> Misc.fatal_error "Layout.product: empty product"
    | [lay] -> lay
    | lays -> of_data (Product lays)

  let rec get_const of_sort ({ prop; data } : _ t) : Const.t option =
    match data with
    | Any -> Some (Const.apply_prop Const.max prop)
    | Sort s -> of_sort s prop
    | Product layouts ->
      Option.map
        (fun x -> Const.apply_prop (Const.Product x) prop)
        (Misc.Stdlib.List.map_option (get_const of_sort) layouts)

  let get_flat_const t = get_const Const.of_flat_sort t

  let get_const t = get_const Const.of_sort t

  let of_new_sort_var ~level prop =
    let sort = Sort.(of_var (new_var ~level)) in
    { prop; data = Sort sort }, sort
end
