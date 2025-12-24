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

  type t =
    | Var of var
    | Base of base
    | Product of t list

  and var =
    { mutable contents : t option;
      mutable level : int;
      uid : int (* For debugging / printing only *)
    }

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
    | Vec512, Vec512 ->
      true
    | ( ( Void | Scannable | Untagged_immediate | Float64 | Float32 | Word
        | Bits8 | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 ),
        _ ) ->
      false

  let to_string_base = function
    (* The word "scannable" should not get exposed to a user. The printing of
       layouts handles this (which rewrite using scannable axes), but sorts
       have no such scannable axes; we print "value" to stay consistent.
       [Sort]s aren't frequently printed, and if these error messages aren't
       descriptive enough then printing [Layout]s should be considered. *)
    | Scannable -> "value"
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

  module Const = struct
    type t =
      | Base of base
      | Product of t list

    let rec equal c1 c2 =
      match c1, c2 with
      | Base b1, Base b2 -> equal_base b1 b2
      | Product cs1, Product cs2 -> List.equal equal cs1 cs2
      | (Base _ | Product _), _ -> false

    let format ppf c =
      let rec pp_element ~nested ppf = function
        | Base b -> Format.fprintf ppf "%s" (to_string_base b)
        | Product cs ->
          let pp_sep ppf () = Format.fprintf ppf "@ & " in
          Misc.pp_nested_list ~nested ~pp_element ~pp_sep ppf cs
      in
      pp_element ~nested:false ppf c

    let rec all_void = function
      | Base Void -> true
      | Base
          ( Scannable | Untagged_immediate | Float64 | Float32 | Bits8 | Bits16
          | Bits32 | Bits64 | Word | Vec128 | Vec256 | Vec512 ) ->
        false
      | Product ts -> List.for_all all_void ts

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

    module Debug_printers = struct
      let t ppf c =
        let rec pp_element ~nested ppf = function
          | Base b ->
            Format.fprintf ppf "%s"
              (match b with
              | Void -> "Void"
              | Scannable -> "Scannable"
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
              | Vec512 -> "Vec512")
          | Product cs ->
            let pp_sep ppf () = Format.fprintf ppf "@ , " in
            Format.fprintf ppf "Product [%a]"
              (Misc.pp_nested_list ~nested ~pp_element ~pp_sep)
              cs
        in
        pp_element ~nested:false ppf c
    end

    let for_function = scannable

    let for_predef_scannable = scannable

    let for_block_element = scannable

    let for_probe_body = scannable

    let for_poly_variant = scannable

    let for_boxed_record = scannable

    let for_object = scannable

    let for_lazy_body = scannable

    let for_tuple_element = scannable

    let for_variant_arg = scannable

    let for_instance_var = scannable

    let for_class_arg = scannable

    let for_method = scannable

    let for_initializer = scannable

    let for_module = scannable

    let for_tuple = scannable

    let for_array_get_result = scannable

    let for_array_comprehension_element = scannable

    let for_list_element = scannable

    let for_idx = bits64

    let for_loop_index = scannable

    let for_constructor = scannable

    let for_boxed_variant = scannable

    let for_exception = scannable

    let for_type_extension = scannable

    let for_class = scannable
  end

  module Var = struct
    type id = int

    let get_id { uid; _ } = uid

    (* Map var uids to smaller numbers for more consistent printing. *)
    let next_id = ref 1

    let names : (int, int) Hashtbl.t = Hashtbl.create 16

    let get_print_number uid =
      match Hashtbl.find_opt names uid with
      | Some n -> n
      | None ->
        let id = !next_id in
        incr next_id;
        Hashtbl.add names uid id;
        id

    let name { uid; _ } =
      "'_representable_layout_" ^ Int.to_string (get_print_number uid)
  end

  (*** debug printing **)
  module Debug_printers = struct
    open Format

    let base ppf b =
      fprintf ppf "%s"
        (match b with
        | Void -> "Void"
        | Scannable -> "Scannable"
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
        | Vec512 -> "Vec512")

    let rec t ppf = function
      | Var v -> fprintf ppf "Var %a" var v
      | Base b -> base ppf b
      | Product ts ->
        fprintf ppf "Product [ %a ]"
          (pp_print_list ~pp_sep:(fun ppf () -> pp_print_text ppf "; ") t)
          ts

    and opt_t ppf = function
      | Some s -> fprintf ppf "Some %a" t s
      | None -> fprintf ppf "None"

    and var ppf v =
      fprintf ppf "{@[@ contents = %a;@ uid = %d@ @]}" opt_t v.contents v.uid
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

  let rec t_iter ~f = function
    | Var v -> f v
    | Base _ -> ()
    | Product ts -> List.iter (fun t -> t_iter ~f t) ts

  let update_level u v =
    let new_level = min v.level u.level in
    if v.level <> new_level
    then (
      log_change (v, Clevel v.level);
      v.level <- new_level);
    if u.level <> new_level
    then (
      log_change (u, Clevel u.level);
      u.level <- new_level)

  let set : var -> t option -> unit =
   fun v t_op ->
    log_change (v, Ccontents v.contents);
    v.contents <- t_op;
    Option.iter (t_iter ~f:(fun u -> update_level u v)) t_op

  module Static = struct
    (* Statically allocated values of various consts and sorts to save
       allocations in in the core hot path functions. [T] is also included in
       the outer module to provide the core sorts. *)

    module T = struct
      let void = Base Void

      let scannable = Base Scannable

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

      let rec of_const : Const.t -> t = function
        | Base b -> of_base b
        | Product cs -> Product (List.map of_const cs)
    end

    module T_option = struct
      let scannable = Some T.scannable

      let void = Some T.void

      let untagged_immediate = Some T.untagged_immediate

      let float64 = Some T.float64

      let float32 = Some T.float32

      let word = Some T.word

      let bits8 = Some T.bits8

      let bits16 = Some T.bits16

      let bits32 = Some T.bits32

      let bits64 = Some T.bits64

      let vec128 = Some T.vec128

      let vec256 = Some T.vec256

      let vec512 = Some T.vec512

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

      let rec of_const : Const.t -> t option = function
        | Base b -> of_base b
        | Product cs ->
          Option.map
            (fun x -> Product x)
            (Misc.Stdlib.List.map_option of_const cs)
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
    end
  end

  let of_var v = Var v

  let last_var_uid = ref 0

  let new_var ~level =
    incr last_var_uid;
    Var { contents = None; uid = !last_var_uid; level }

  let rec get : t -> t = function
    | Base _ as t -> t
    | Product ts as t ->
      let ts' = List.map get ts in
      if List.for_all2 ( == ) ts ts' then t else Product ts'
    | Var r as t -> (
      match r.contents with
      | None -> t
      | Some s ->
        let result = get s in
        if result != s then set r (Some result);
        (* path compression *)
        result)

  let rec default_to_scannable_and_get : t -> Const.t = function
    | Base b -> Static.Const.of_base b
    | Product ts -> Product (List.map default_to_scannable_and_get ts)
    | Var r -> (
      match r.contents with
      | None ->
        set r Static.T_option.scannable;
        Static.Const.scannable
      | Some s ->
        let result = default_to_scannable_and_get s in
        set r (Static.T_option.of_const result);
        (* path compression *)
        result)

  (* CR layouts v12: Default to void instead. *)
  let default_for_transl_and_get s = default_to_scannable_and_get s

  let is_scannable_or_var s =
    match get s with Base Scannable | Var _ -> true | _ -> false

  (***********************)
  (* equality *)

  type equate_result =
    | Unequal
    | Equal_mutated_first
    | Equal_mutated_second
    | Equal_mutated_both
    | Equal_no_mutation

  let swap_equate_result = function
    | Equal_mutated_first -> Equal_mutated_second
    | Equal_mutated_second -> Equal_mutated_first
    | (Unequal | Equal_no_mutation | Equal_mutated_both) as r -> r

  let[@inline] sorts_of_product s =
    (* In the equate functions, it's useful to pass around lists of sorts inside
       the product constructor they came from to avoid re-allocating it if we
       end up wanting to store it in a variable. We could probably eliminate the
       use of this by collapsing a bunch of the functions below into each other,
       but that would be much less readable. *)
    match s with
    | Product sorts -> sorts
    | Var _ | Base _ -> Misc.fatal_error "Jkind_types.sorts_of_product"

  let rec equate_sort_sort s1 s2 =
    match s1 with
    | Base b1 -> swap_equate_result (equate_sort_base s2 b1)
    | Var v1 -> equate_var_sort v1 s2
    | Product _ -> swap_equate_result (equate_sort_product s2 s1)

  and equate_sort_base s1 b2 =
    match s1 with
    | Base b1 -> if equal_base b1 b2 then Equal_no_mutation else Unequal
    | Var v1 -> equate_var_base v1 b2
    | Product _ -> Unequal

  and equate_var_base v1 b2 =
    match v1.contents with
    | Some s1 -> equate_sort_base s1 b2
    | None ->
      set v1 (Static.T_option.of_base b2);
      Equal_mutated_first

  and equate_var_sort v1 s2 =
    match s2 with
    | Base b2 -> equate_var_base v1 b2
    | Var v2 -> equate_var_var v1 v2
    | Product _ -> equate_var_product v1 s2

  and equate_var_var v1 v2 =
    if v1 == v2
    then Equal_no_mutation
    else
      match v1.contents, v2.contents with
      | Some s1, _ -> swap_equate_result (equate_var_sort v2 s1)
      | _, Some s2 -> equate_var_sort v1 s2
      | None, None ->
        set v1 (Some (of_var v2));
        Equal_mutated_first

  and equate_var_product v1 s2 =
    match v1.contents with
    | Some s1 -> equate_sort_product s1 s2
    | None ->
      set v1 (Some s2);
      Equal_mutated_first

  and equate_sort_product s1 s2 =
    match s1 with
    | Base _ -> Unequal
    | Product sorts1 ->
      let sorts2 = sorts_of_product s2 in
      equate_sorts sorts1 sorts2
    | Var v1 -> equate_var_product v1 s2

  and equate_sorts sorts1 sorts2 =
    let rec go sorts1 sorts2 acc =
      match sorts1, sorts2 with
      | [], [] -> acc
      | sort1 :: sorts1, sort2 :: sorts2 -> (
        match equate_sort_sort sort1 sort2, acc with
        | Unequal, _ -> Unequal
        | _, Unequal -> assert false
        | Equal_no_mutation, acc | acc, Equal_no_mutation ->
          go sorts1 sorts2 acc
        | Equal_mutated_both, _ | _, Equal_mutated_both ->
          go sorts1 sorts2 Equal_mutated_both
        | Equal_mutated_first, Equal_mutated_first ->
          go sorts1 sorts2 Equal_mutated_first
        | Equal_mutated_second, Equal_mutated_second ->
          go sorts1 sorts2 Equal_mutated_second
        | Equal_mutated_first, Equal_mutated_second
        | Equal_mutated_second, Equal_mutated_first ->
          go sorts1 sorts2 Equal_mutated_both)
      | _, _ -> assert false
    in
    if List.compare_lengths sorts1 sorts2 = 0
    then go sorts1 sorts2 Equal_no_mutation
    else Unequal

  let equate_tracking_mutation = equate_sort_sort

  (* Don't expose whether or not mutation happened; we just need that for
     [Jkind] *)
  let equate s1 s2 =
    match equate_tracking_mutation s1 s2 with
    | Unequal -> false
    | Equal_mutated_first | Equal_mutated_second | Equal_no_mutation
    | Equal_mutated_both ->
      true

  let is_void_defaulting t =
    (* CR layouts v5: this should probably default to void now *)
    match default_to_scannable_and_get t with
    | Base Void -> true
    | Base
        ( Scannable | Untagged_immediate | Float64 | Float32 | Word | Bits8
        | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 ) ->
      false
    | Product _ -> false

  let decompose_into_product ~level t n =
    let ts = List.init n (fun _ -> new_var ~level) in
    if equate t (Product ts) then Some ts else None

  (*** pretty printing ***)

  let format ppf t =
    let rec pp_element ~nested ppf t =
      match get t with
      | Base b -> Format.fprintf ppf "%s" (to_string_base b)
      | Var v -> Format.fprintf ppf "%s" (Var.name v)
      | Product ts ->
        let pp_sep ppf () = Format.fprintf ppf " & " in
        Misc.pp_nested_list ~nested ~pp_element ~pp_sep ppf ts
    in
    pp_element ~nested:false ppf t

  include Static.T

  module Flat = struct
    type t =
      | Var of Var.id
      | Base of base
  end
end

module Scannable_axes = struct
  open Jkind_axis

  type t =
    { nullability : Jkind_axis.Nullability.t;
      separability : Jkind_axis.Separability.t
    }

  let max = { nullability = Nullability.max; separability = Separability.max }

  let value_axes = { nullability = Non_null; separability = Separable }

  let equal { nullability = n1; separability = s1 }
      { nullability = n2; separability = s2 } =
    Nullability.equal n1 n2 && Separability.equal s1 s2
end

module Layout = struct
  open Jkind_axis

  type 'sort t =
    | Sort of 'sort * Scannable_axes.t
    | Product of 'sort t list
    | Any of Scannable_axes.t

  module Const = struct
    type t =
      | Any of Scannable_axes.t
      | Base of Sort.base * Scannable_axes.t
      | Product of t list

    let max = Any Scannable_axes.max

    let rec equal c1 c2 =
      match c1, c2 with
      | Base (Scannable, sa1), Base (Scannable, sa2) ->
        Scannable_axes.equal sa1 sa2
      | Base (b1, _), Base (b2, _) -> Sort.equal_base b1 b2
      | Any sa1, Any sa2 -> Scannable_axes.equal sa1 sa2
      | Product cs1, Product cs2 -> List.equal equal cs1 cs2
      | (Base _ | Any _ | Product _), _ -> false

    let rec get_sort : t -> Sort.Const.t option = function
      | Any _ -> None
      | Base (b, _) -> Some (Base b)
      | Product ts ->
        Option.map
          (fun x -> Sort.Const.Product x)
          (Misc.Stdlib.List.map_option get_sort ts)

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
    end

    let of_sort s sa =
      let rec of_sort (s : Sort.t) sa =
        match s with
        | Var _ -> None
        | Base b -> Some (Static.of_base b sa)
        | Product sorts ->
          Option.map
            (fun x -> Product x)
            (* [Sort.get] is deep, so no need to repeat it here *)
            (* In all cases where sort products are turned into layout products,
               [Scannable_axes.max] is used. The sort product doesn't store
               enough information to make any other choice. *)
            (Misc.Stdlib.List.map_option
               (fun s -> of_sort s Scannable_axes.max)
               sorts)
      in
      of_sort (Sort.get s) sa

    let of_flat_sort (s : Sort.Flat.t) sa =
      match s with Var _ -> None | Base b -> Some (Static.of_base b sa)
  end

  let rec of_const (const : Const.t) : _ t =
    match const with
    | Any sa -> Any sa
    | Base (b, sa) -> Sort (Sort.of_base b, sa)
    | Product cs -> Product (List.map of_const cs)

  let product = function
    | [] -> Misc.fatal_error "Layout.product: empty product"
    | [lay] -> lay
    | lays -> Product lays

  let rec get_const of_sort : _ t -> Const.t option = function
    | Any sa -> Some (Any sa)
    | Sort (s, sa) -> of_sort s sa
    | Product layouts ->
      Option.map
        (fun x -> Const.Product x)
        (Misc.Stdlib.List.map_option (get_const of_sort) layouts)

  let get_flat_const t = get_const Const.of_flat_sort t

  let get_const t = get_const Const.of_sort t

  let of_new_sort_var ~level sa =
    let sort = Sort.new_var ~level in
    Sort (sort, sa), sort
end
