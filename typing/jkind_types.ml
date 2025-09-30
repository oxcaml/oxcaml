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

module Scannable_axes = struct
  type pointerness = Non_pointer | (* Non_float | Separable | *) Any_pointerness
  type t = { pointerness : pointerness }

  let to_string_pointerness = function
    | Non_pointer -> "non_pointer"
    | Any_pointerness -> "any_pointerness"

  let equal_pointerness p1 p2 =
    match p1, p2 with
    | Non_pointer, Non_pointer
    | Any_pointerness, Any_pointerness ->
      true
    | (Non_pointer | Any_pointerness), _ -> false

  let equal
        { pointerness = pointerness1 }
        { pointerness = pointerness2 }
    =
    equal_pointerness pointerness1 pointerness2

  let to_string
        { pointerness }
    =
    to_string_pointerness pointerness

  let meet_pointerness p1 p2 =
    match p1, p2 with
    | Any_pointerness, Any_pointerness -> Any_pointerness
    | Non_pointer, Non_pointer
    | (Non_pointer, Any_pointerness)
    | (Any_pointerness, Non_pointer) -> Non_pointer

  let meet
        { pointerness = pointerness1 }
        { pointerness = pointerness2 }
    =
    { pointerness = meet_pointerness pointerness1 pointerness2 }

  let sub_pointerness p1 p2 : Misc.Le_result.t =
    match p1, p2 with
    | Any_pointerness, Any_pointerness
    | Non_pointer, Non_pointer -> Equal
    | Non_pointer, Any_pointerness -> Less
    | Any_pointerness, Non_pointer -> Not_le

  let sub
        { pointerness = pointerness1 }
        { pointerness = pointerness2 }
    =
    sub_pointerness pointerness1 pointerness2

  let max = { pointerness = Any_pointerness }
end

module Sort = struct
  type base =
    | Void
    | Scannable of Scannable_axes.t
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
    (* scannable axes is an upper bound, if the var gets filled in with
       scannable *)
    | Var of var
    | Base of base
    | Product of t list

  and var =
    { mutable contents : var_contents;
      uid : int (* For debugging / printing only *)
    }

  and var_contents =
    | Filled of t
    | Empty of Scannable_axes.t

  let equal_base b1 b2 =
    match b1, b2 with
    | Void, Void
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
    | Scannable s1, Scannable s2 ->
      Scannable_axes.equal s1 s2
    | ( ( Void | Scannable _ | Untagged_immediate | Float64 | Float32 | Word | Bits8
        | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 ),
        _ ) ->
      false

  let to_string_base = function
    | Scannable s -> "scannable " ^ Scannable_axes.to_string s
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
          ( Scannable _ | Untagged_immediate | Float64 | Float32 | Bits8 | Bits16
          | Bits32 | Bits64 | Word | Vec128 | Vec256 | Vec512 ) ->
        false
      | Product ts -> List.for_all all_void ts

    (* CR rtjoa: audit uses *)
    let value = Base (Scannable { pointerness = Any_pointerness })

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
              (* CR rtjoa: better debug printer *)
              | Scannable _ -> "Scannable"
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

    let for_function = value

    let for_predef_value = value

    let for_block_element = value

    let for_probe_body = value

    let for_poly_variant = value

    let for_boxed_record = value

    let for_object = value

    let for_lazy_body = value

    let for_tuple_element = value

    let for_variant_arg = value

    let for_instance_var = value

    let for_class_arg = value

    let for_method = value

    let for_initializer = value

    let for_module = value

    let for_tuple = value

    let for_array_get_result = value

    let for_array_comprehension_element = value

    let for_list_element = value

    let for_idx = bits64

    let for_loop_index = value

    let for_constructor = value

    let for_module_field = value

    let for_boxed_variant = value

    let for_exception = value
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
        (* CR rtjoa: better debug printer *)
        | Scannable _ -> "Scannable"
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

    and var_contents ppf = function
      | Filled s -> fprintf ppf "Filled %a" t s
      | Empty sa -> fprintf ppf "Empty %s" (Scannable_axes.to_string sa)

    and var ppf v =
      fprintf ppf "{@[@ contents = %a;@ uid = %d@ @]}" var_contents v.contents v.uid
  end

  (* To record changes to sorts, for use with `Types.{snapshot, backtrack}` *)
  type change = var * var_contents

  let change_log : (change -> unit) ref = ref (fun _ -> ())

  let set_change_log cl = change_log := cl

  let log_change change = !change_log change

  let undo_change (v, t_op) = v.contents <- t_op

  let set : var -> var_contents -> unit =
   fun v t_op ->
    log_change (v, v.contents);
    v.contents <- t_op

  module Static = struct
    (* Statically allocated values of various consts and sorts to save
       allocations in in the core hot path functions. [T] is also included in
       the outer module to provide the core sorts. *)

    module T = struct
      let void = Base Void

      let scannable_any_pointerness =
        Base (Scannable { pointerness = Any_pointerness })

      let scannable_non_pointer =
        Base (Scannable { pointerness = Non_pointer })

      let value =
        Base (Scannable { pointerness = Any_pointerness })

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
        | Scannable { pointerness = Any_pointerness } ->
          scannable_any_pointerness
        | Scannable { pointerness = Non_pointer } ->
          scannable_non_pointer
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
      let scannable_any_pointerness = Some T.scannable_any_pointerness

      let scannable_non_pointer = Some T.scannable_non_pointer

      let value = Some T.value

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
        | Scannable { pointerness = Any_pointerness } ->
          scannable_any_pointerness
        | Scannable { pointerness = Non_pointer } -> scannable_non_pointer
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

      let scannable_any_pointerness = Base (Scannable { pointerness = Any_pointerness })

      let scannable_non_pointer = Base (Scannable { pointerness = Non_pointer })

      let value = Base (Scannable { pointerness = Any_pointerness })

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
        | Scannable { pointerness = Any_pointerness } -> scannable_any_pointerness
        | Scannable { pointerness = Non_pointer } -> scannable_non_pointer
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

  let new_var s =
    incr last_var_uid;
    Var { contents = Empty s; uid = !last_var_uid }

  (* let apply_sa (t : t) (sa : Scannable_axes.t) =
   *   match t with
   *   | Var (r, sa') -> Var (r, Scannable_axes.meet sa sa')
   *   | Product _ -> t
   *   | Base sa' ->
   *     (* assert (Scannable_axes.less sa' sa) *)
   *     Base sa' *)

  let rec get : t -> t = function
    | Base _ as t -> t
    | Product ts as t ->
      let ts' = List.map get ts in
      if List.for_all2 ( == ) ts ts' then t else Product ts'
    | Var r as t -> (
      match r.contents with
      | Empty _ -> t
      | Filled s ->
        let result = get s in
        if result != s then set r result;
        (* path compression *)
        result)

  let rec default_to_value_and_get : t -> Const.t = function
    | Base b -> Static.Const.of_base b
    | Product ts -> Product (List.map default_to_value_and_get ts)
    | Var (r, sa) -> (
      match r.contents with
      | None ->
        set r Static.T_option.value;
        Static.Const.value
      | Some s ->
        let result = default_to_value_and_get s in
        set r (Static.T_option.of_const result);
        (* path compression *)
        result)

  (* CR layouts v12: Default to void instead. *)
  let default_for_transl_and_get s = default_to_value_and_get s

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
    match default_to_value_and_get t with
    | Base Void -> true
    | Base
        ( Scannable _ | Untagged_immediate | Float64 | Float32 | Word | Bits8 | Bits16
        | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 ) ->
      false
    | Product _ -> false

  let var_of_scannable_axes s =
    let v = new_var () in
    set v (Some (Var (new_var (), s)));
    v

  let decompose_into_product t n =
    let ts = List.init n (fun _ -> new_var ()) in
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
end

module Layout = struct
  type 'sort t =
    | Sort of 'sort
    | Product of 'sort t list
    | Any of Scannable_axes.t

  module Const = struct
    type t =
      | Any of Scannable_axes.t
      | Base of Sort.base
      | Product of t list
  end
end
