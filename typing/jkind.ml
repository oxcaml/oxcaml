(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Chris Casinghino, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Mode
open Jkind_types
open Types
open Jkind_axis
module Jkind0 = Btype.Jkind0

[@@@warning "+9"]

let print_type_expr : (Format.formatter -> type_expr -> unit) ref =
  ref (fun _ _ -> assert false)

let set_print_type_expr p = print_type_expr := p

let raw_type_expr : (Format.formatter -> type_expr -> unit) ref =
  ref (fun _ _ -> assert false)

let set_raw_type_expr p = raw_type_expr := p

module Nonempty_list = Misc.Nonempty_list

(* A *sort* is the information the middle/back ends need to be able to
   compile a manipulation (storing, passing, etc) of a runtime value. *)
module Sort = Jkind_types.Sort

type sort = Sort.t

module Sub_failure_reason = Jkind0.Violation.Sub_failure_reason

module Sub_result = struct
  type t =
    | Equal
    | Less
    | Not_le of Sub_failure_reason.t Nonempty_list.t

  let[@inline] of_le_result ~failure_reason (le_result : Misc.Le_result.t) =
    match le_result with
    | Less -> Less
    | Equal -> Equal
    | Not_le -> Not_le (failure_reason ())

  let[@inline] combine sr1 sr2 =
    match sr1, sr2 with
    | Equal, Equal -> Equal
    | Equal, Less | Less, Equal | Less, Less -> Less
    | Not_le reasons1, Not_le reasons2 ->
      Not_le Nonempty_list.(reasons1 @ reasons2)
    | Not_le reasons, _ | _, Not_le reasons -> Not_le reasons

  let require_le = function
    | Less | Equal -> Ok ()
    | Not_le reason -> Error reason

  let is_le t = require_le t |> Result.is_ok
end

module Scannable_axes = struct
  include Jkind_types.Scannable_axes

  let le sa1 sa2 = Misc.Le_result.is_le (less_or_equal sa1 sa2)

  let meet { nullability = n1; separability = s1 }
      { nullability = n2; separability = s2 } =
    { nullability = Nullability.meet n1 n2;
      separability = Separability.meet s1 s2
    }

  let to_string_list_diff
      ~base:{ nullability = n_against; separability = s_against }
      { nullability; separability } =
    let diff = [] in
    let diff =
      if Nullability.equal n_against nullability
      then diff
      else Nullability.to_string nullability :: diff
    in
    let diff =
      if Separability.equal s_against separability
      then diff
      else Separability.to_string separability :: diff
    in
    diff

  let to_string_list = to_string_list_diff ~base:max

  let debug_print ppf { nullability; separability } =
    Format.fprintf ppf "@[{ nullability = %a;@ separability = %a }@]"
      Nullability.print nullability Separability.print separability
end

(* A *layout* of a type describes the way values of that type are stored at
   runtime, including details like width, register convention, calling
   convention, etc. A layout may be *representable* or *unrepresentable*.  The
   middle/back ends are unable to cope with values of types with an
   unrepresentable layout. The only unrepresentable layout is `any`, which is
   the top of the layout lattice. *)
module Layout = struct
  include Jkind_types.Layout

  type nonrec 'sort t = 'sort t =
    | Sort of 'sort * Scannable_axes.t
    | Product of 'sort t list
    | Any of Scannable_axes.t

  module Const = struct
    include Jkind_types.Layout.Const

    (* if so, scannable axis annotations should not trigger a warning *)
    let is_scannable_or_any = function
      | Any _ | Base (Scannable, _) -> true
      | Base
          ( ( Void | Untagged_immediate | Float64 | Float32 | Word | Bits8
            | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 ),
            _ ) ->
        false
      | Product _ -> false

    let rec of_sort_const (s : Sort.Const.t) sa =
      match s with
      | Base b -> Static.of_base b sa
      | Product consts ->
        Product (List.map (fun s -> of_sort_const s sa) consts)

    let rec equal_up_to_scannable_axes c1 c2 =
      match c1, c2 with
      | Base (b1, _), Base (b2, _) -> Sort.equal_base b1 b2
      | Any _, Any _ -> true
      | Product cs1, Product cs2 ->
        List.equal equal_up_to_scannable_axes cs1 cs2
      | (Base _ | Any _ | Product _), _ -> false

    let set_root_nullability t nullability =
      match t with
      | Any sa -> Any { sa with nullability }
      | Base (b, sa) -> Static.of_base b { sa with nullability }
      | Product _ -> t

    let set_root_separability t separability =
      match t with
      | Any sa -> Any { sa with separability }
      | Base (b, sa) -> Static.of_base b { sa with separability }
      | Product _ -> t

    (* Returns [None] if the root has no meaningful scannable axes. *)
    let get_root_scannable_axes t =
      match t with
      | Any sa -> Some sa
      | Base (_, sa) -> if is_scannable_or_any t then Some sa else None
      | Product _ -> None

    let to_string t =
      let rec to_string nested (t : t) =
        match t with
        | Any sa -> String.concat " " ("any" :: Scannable_axes.to_string_list sa)
        | Base (Scannable, sa) ->
          String.concat " "
            ("value" :: Scannable_axes.(to_string_list_diff ~base:value_axes) sa)
        | Base (b, _) -> Sort.to_string_base b
        | Product ts ->
          String.concat ""
            [ (if nested then "(" else "");
              String.concat " & " (List.map (to_string true) ts);
              (if nested then ")" else "") ]
      in
      to_string false t

    let rec has_component ~component t =
      equal component t
      ||
      match t with
      | Base _ | Any _ -> false (* nothing left to descend into *)
      | Product ts -> List.exists (has_component ~component) ts

    module Debug_printers = struct
      open Format

      let t ppf t = fprintf ppf "%s" (to_string t)
    end
  end

  module Debug_printers = struct
    open Format

    let rec t format_sort ppf = function
      | Any sa -> fprintf ppf "Any %a" Scannable_axes.debug_print sa
      | Sort (s, sa) ->
        fprintf ppf "Sort (%a, %a)" format_sort s Scannable_axes.debug_print sa
      | Product ts ->
        fprintf ppf "Product [ %a ]"
          (pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
             (t format_sort))
          ts
  end

  let rec get : Sort.t t -> Sort.Flat.t t =
    let rec flatten_sort (s : Sort.t) sa : Sort.Flat.t t =
      match s with
      | Var v -> Sort (Var (Sort.Var.get_id v), sa)
      | Base b ->
        Sort (Base b, sa)
        (* No need to call [Sort.get] here, because one [get] is deep. *)
      | Product sorts ->
        Product (List.map (fun s -> flatten_sort s Scannable_axes.max) sorts)
    in
    function
    | Any sa -> Any sa
    | Sort (s, sa) -> flatten_sort (Sort.get s) sa
    | Product ts -> Product (List.map get ts)

  let sort_equal_result ~allow_mutation result =
    match (result : Sort.equate_result) with
    | (Equal_mutated_first | Equal_mutated_second | Equal_mutated_both)
      when not allow_mutation ->
      Misc.fatal_errorf "Jkind.equal: Performed unexpected mutation"
    | Unequal -> false
    | Equal_no_mutation | Equal_mutated_first | Equal_mutated_second
    | Equal_mutated_both ->
      true

  let rec equate_or_equal ~allow_mutation ~level t1 t2 =
    match t1, t2 with
    | Sort (s1, sa1), Sort (s2, sa2) ->
      sort_equal_result ~allow_mutation (Sort.equate_tracking_mutation s1 s2)
      &&
      if
        Sort.is_scannable_or_var s1
        (* CR layouts-scannable: if [s1] and [s2] are both unfilled sort
            variables, and [sa1 <> sa2], then they should _not_ be equal,
            even though they would be in the case that the sort variables
            aren't filled with [value].
            It is possible the unifying that (now shared) sort variable with
            [value] would lead to better error reporting. For now, it just
            (conservatively) concludes that they are not equal. *)
      then Scannable_axes.equal sa1 sa2
      else true
    | Product ts, Sort (sort, _) | Sort (sort, _), Product ts -> (
      match Sort.decompose_into_product ~level sort (List.length ts) with
      | None -> false
      | Some sorts ->
        let sorts = List.map (fun x -> Sort (x, Scannable_axes.max)) sorts in
        List.equal (equate_or_equal ~allow_mutation ~level) ts sorts)
    | Product ts1, Product ts2 ->
      List.equal (equate_or_equal ~allow_mutation ~level) ts1 ts2
    | Any sa1, Any sa2 -> Scannable_axes.equal sa1 sa2
    | (Any _ | Sort _ | Product _), _ -> false

  let is_scannable_or_var : _ t -> bool = function
    | Any _ -> false
    | Sort (b, _) -> Sort.is_scannable_or_var b
    | Product _ -> false

  let get_root_scannable_axes : _ t -> Scannable_axes.t option = function
    | Any sa -> Some sa
    | Sort (b, sa) -> if Sort.is_scannable_or_var b then Some sa else None
    | Product _ -> None

  let set_root_nullability t nullability =
    match t with
    | Any sa -> Any { sa with nullability }
    | Sort (b, sa) ->
      if Sort.is_scannable_or_var b
      then Sort (b, { sa with nullability })
      else t
    | Product _ -> t

  let set_root_separability t separability =
    match t with
    | Any sa -> Any { sa with separability }
    | Sort (b, sa) ->
      if Sort.is_scannable_or_var b
      then Sort (b, { sa with separability })
      else t
    | Product _ -> t

  (* only meets at the root, meaning products are left unchanged. *)
  let meet_root_scannable_axes t sa =
    match t with
    | Any sa' -> Any (Scannable_axes.meet sa sa')
    | Sort (s, sa') -> Sort (s, Scannable_axes.meet sa sa')
    | Product _ -> t

  let sub ~level t1 t2 =
    let rec sub t1 t2 : Misc.Le_result.t =
      match t1, t2 with
      | Any sa1, Any sa2 -> Scannable_axes.less_or_equal sa1 sa2
      | Sort (sort, sa1), Any sa2 ->
        (* CR layouts-scannable: If [sort] has not been filled and
           [sa1] </= [sa2], we conservatively say that it is [Not_le].
           They can still become equal, though, in the case where [sort] is
           filled in with anything other than [scannable]. Unifying [sort] with
           [scannable] could potentially yield better error messages. Another
           option could be to add to the [Layout_disagreement] type. *)
        if Sort.is_scannable_or_var sort && not (Scannable_axes.le sa1 sa2)
        then Not_le
        else Less
      | Product _, Any _ -> Less
      | Any _, _ -> Not_le
      | Sort (s1, sa1), Sort (s2, sa2) ->
        if Sort.equate s1 s2
        then
          if Sort.is_scannable_or_var s1
          then Scannable_axes.less_or_equal sa1 sa2
          else Equal
        else Not_le
      | Product ts1, Product ts2 ->
        if List.compare_lengths ts1 ts2 = 0
        then Misc.Le_result.combine_list (List.map2 sub ts1 ts2)
        else Not_le
      | Product ts1, Sort (s2, _) -> (
        match Sort.decompose_into_product ~level s2 (List.length ts1) with
        | None -> Not_le
        | Some ss2 ->
          Misc.Le_result.combine_list
            (List.map2
               (fun t1 s2 -> sub t1 (Sort (s2, Scannable_axes.max)))
               ts1 ss2))
      | Sort (s1, _), Product ts2 -> (
        match Sort.decompose_into_product ~level s1 (List.length ts2) with
        | None -> Not_le
        | Some ss1 ->
          Misc.Le_result.combine_list
            (List.map2
               (fun s1 t2 -> sub (Sort (s1, Scannable_axes.max)) t2)
               ss1 ts2))
    in
    Sub_result.of_le_result (sub t1 t2) ~failure_reason:(fun () ->
        [Layout_disagreement])

  let rec intersection ~level t1 t2 =
    (* pre-condition to [products]: [ts1] and [ts2] have the same length *)
    let products ts1 ts2 =
      let components = List.map2 (intersection ~level) ts1 ts2 in
      Option.map
        (fun x -> Product x)
        (Misc.Stdlib.List.some_if_all_elements_are_some components)
    in
    match t1, t2 with
    | _, Any sa2 -> Some (meet_root_scannable_axes t1 sa2)
    | Any sa1, _ -> Some (meet_root_scannable_axes t2 sa1)
    | Sort (s1, sa1), Sort (s2, sa2) ->
      if Sort.equate s1 s2
      then Some (Sort (s1, Scannable_axes.meet sa1 sa2))
      else None
    | Product ts1, Product ts2 ->
      if List.compare_lengths ts1 ts2 = 0 then products ts1 ts2 else None
    | Product ts, Sort (sort, _) | Sort (sort, _), Product ts -> (
      match Sort.decompose_into_product ~level sort (List.length ts) with
      | None -> None
      | Some sorts ->
        products ts (List.map (fun x -> Sort (x, Scannable_axes.max)) sorts))

  let rec default_to_scannable_and_get : _ Layout.t -> Const.t = function
    | Any sa -> Any sa
    | Sort (s, sa) ->
      Const.of_sort_const (Sort.default_to_scannable_and_get s) sa
    | Product p -> Product (List.map default_to_scannable_and_get p)

  let format ppf layout =
    let open Format in
    let pp_string_list ppf lst =
      pp_print_list ~pp_sep:(fun f () -> fprintf f " ") pp_print_string ppf lst
    in
    let rec pp_element ~nested ppf : _ Layout.t -> unit = function
      | Any sa -> pp_string_list ppf ("any" :: Scannable_axes.to_string_list sa)
      | Sort (s, sa) -> (
        match Sort.get s with
        | Base Scannable ->
          let value_axes_diff =
            Scannable_axes.(to_string_list_diff ~base:value_axes sa)
          in
          pp_string_list ppf ("value" :: value_axes_diff)
        | Var _ ->
          let sort_var_str = Format.asprintf "%a" Sort.format s in
          pp_string_list ppf (sort_var_str :: Scannable_axes.to_string_list sa)
        (* definitely never scannable *)
        | Base _ | Product _ -> fprintf ppf "%a" Sort.format s)
      | Product ts ->
        let pp_sep ppf () = Format.fprintf ppf "@ & " in
        Misc.pp_nested_list ~nested ~pp_element ~pp_sep ppf ts
    in
    pp_element ~nested:false ppf layout
end

module Externality = Externality
module Nullability = Nullability

module History = struct
  include Jkind_intf.History

  let is_imported t =
    match t.history with Creation Imported -> true | _ -> false

  (* CR layouts: Anything that returns false here could probably just be removed,
     but let's keep the info around at least during development. *)
  let is_informative t =
    match t.history with Creation Imported -> false | _ -> true

  let update_reason t reason = { t with history = Creation reason }

  let with_warning t = { t with has_warned = true }

  let has_warned t = t.has_warned
end

(******************************)
(*** user errors ***)

module Error = struct
  type t =
    | Insufficient_level :
        { jkind : Parsetree.jkind_annotation;
          required_layouts_level : Language_extension.maturity
        }
        -> t
    | Unknown_jkind of Parsetree.jkind_annotation
    | Unknown_kind_modifier of string
    | Multiple_jkinds of
        { from_annotation : Parsetree.jkind_annotation;
          from_attribute : Builtin_attributes.jkind_attribute Location.loc
        }
    | Unimplemented_syntax
    | With_on_right

  exception User_error of Location.t * t
end

let raise ~loc err = raise (Error.User_error (loc, err))

(******************************)
module Mod_bounds = struct
  include Jkind0.Mod_bounds

  let meet t1 t2 =
    let crossing = Crossing.meet (crossing t1) (crossing t2) in
    let externality = Externality.meet (externality t1) (externality t2) in
    create crossing ~externality

  let less_or_equal t1 t2 =
    let[@inline] modal_less_or_equal ax : Sub_result.t =
      let a = t1 |> crossing |> (Crossing.proj [@inlined hint]) ax in
      let b = t2 |> crossing |> (Crossing.proj [@inlined hint]) ax in
      match
        ( (Crossing.Per_axis.le [@inlined hint]) ax a b,
          (Crossing.Per_axis.le [@inlined hint]) ax b a )
      with
      | true, true -> Equal
      | true, false -> Less
      | false, _ -> Not_le [Axis_disagreement (Pack (Modal ax))]
    in
    let[@inline] axis_less_or_equal ~le ~axis a b : Sub_result.t =
      match le a b, le b a with
      | true, true -> Equal
      | true, false -> Less
      | false, _ -> Not_le [Axis_disagreement axis]
    in
    Sub_result.combine (modal_less_or_equal (Comonadic Areality))
    @@ Sub_result.combine (modal_less_or_equal (Monadic Uniqueness))
    @@ Sub_result.combine (modal_less_or_equal (Comonadic Linearity))
    @@ Sub_result.combine (modal_less_or_equal (Monadic Contention))
    @@ Sub_result.combine (modal_less_or_equal (Comonadic Portability))
    @@ Sub_result.combine (modal_less_or_equal (Comonadic Forkable))
    @@ Sub_result.combine (modal_less_or_equal (Comonadic Yielding))
    @@ Sub_result.combine (modal_less_or_equal (Comonadic Statefulness))
    @@ Sub_result.combine (modal_less_or_equal (Monadic Visibility))
    @@ Sub_result.combine (modal_less_or_equal (Monadic Staticity))
    @@ axis_less_or_equal ~le:Externality.le ~axis:(Pack (Nonmodal Externality))
         (externality t1) (externality t2)

  let[@inline] get (type a) ~(axis : a Axis.t) t : a =
    match axis with
    | Modal ax -> t |> crossing |> (Crossing.proj [@inlined hint]) ax
    | Nonmodal Externality -> externality t

  (** Get all axes that are set to max *)
  let get_max_axes t =
    let[@inline] add_if b ax axis_set =
      if b then Axis_set.add axis_set ax else axis_set
    in
    let[@inline] add_crossing_if ax axis_set =
      if
        Crossing.Per_axis.(
          (le [@inlined hint]) ax ((max [@inlined hint]) ax)
            ((Crossing.proj [@inlined hint]) ax (crossing t)))
      then Axis_set.add axis_set (Modal ax)
      else axis_set
    in
    Axis_set.empty
    |> add_crossing_if (Comonadic Areality)
    |> add_crossing_if (Comonadic Linearity)
    |> add_crossing_if (Monadic Uniqueness)
    |> add_crossing_if (Comonadic Portability)
    |> add_crossing_if (Monadic Contention)
    |> add_crossing_if (Comonadic Forkable)
    |> add_crossing_if (Comonadic Yielding)
    |> add_crossing_if (Comonadic Statefulness)
    |> add_crossing_if (Monadic Visibility)
    |> add_crossing_if (Monadic Staticity)
    |> add_if
         (Externality.le Externality.max (externality t))
         (Nonmodal Externality)

  let to_mode_crossing t = crossing t
end

module With_bounds = struct
  include Jkind0.With_bounds

  module Type_info = struct
    include With_bounds_type_info

    let print ppf { relevant_axes } =
      let open Format in
      fprintf ppf "@[{ relevant_axes = %a }@]" Axis_set.print relevant_axes

    let axes_ignored_by_modalities ~mod_bounds
        ~type_info:{ relevant_axes = explicit_relevant_axes } =
      (* Axes that are max are implicitly relevant. ie, including or excluding an
         axis from the set of relevant axes is semantically equivalent if the mod-
         bound on that axis is max.

         Note that this mostly matters because we mark axes as /not/ explicitly relevant
         on types when the axis is max, for performance reasons - but we don't want to
         print constant modalities for those axes!
      *)
      let implicit_relevant_axes = Mod_bounds.get_max_axes mod_bounds in
      let relevant_axes =
        Axis_set.union explicit_relevant_axes implicit_relevant_axes
      in
      Axis_set.complement relevant_axes
  end

  let to_best_eff_map = function
    | No_with_bounds -> With_bounds_types.empty
    | With_bounds bounds -> bounds

  let to_list : type d. d with_bounds -> _ = function
    | No_with_bounds -> []
    | With_bounds tys -> tys |> With_bounds_types.to_seq |> List.of_seq

  open Allowance

  let map (type l r) f : (l * r) t -> (l * r) t = function
    | No_with_bounds -> No_with_bounds
    | With_bounds tys -> With_bounds (With_bounds_types.map f tys)

  let debug_print_types ppf tys =
    let open Format in
    pp_print_seq
      ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
      (fun ppf (ty, ti) ->
        fprintf ppf "@[(%a, %a)@]" !raw_type_expr ty Type_info.print ti)
      ppf
      (With_bounds_types.to_seq tys)

  let debug_print (type l r) ppf : (l * r) t -> _ =
    let open Format in
    function
    | No_with_bounds -> fprintf ppf "No_with_bounds"
    | With_bounds tys ->
      fprintf ppf "With_bounds @[[%a]@]" debug_print_types tys

  let join_bounds =
    With_bounds_types.merge (fun _ ti1 ti2 ->
        match ti1, ti2 with
        | None, None -> None
        | Some ti, None -> Some ti
        | None, Some ti -> Some ti
        | Some ti1, Some ti2 -> Some (Type_info.join ti1 ti2))

  (* You might think that we can only do joins on the left. But that's not true!
     We can join constants. The important thing is that the allowances of both
     arguments are the same and that they match the result: this will mean that
     if we have any with_bounds in either argument, the result is an l-With_bounds, as
     required. This might change once we have arrow kinds, but we'll deal with
     that when we get there. *)
  let join (type l r) (bag1 : (l * r) t) (bag2 : (l * r) t) : (l * r) t =
    match bag1, bag2 with
    | No_with_bounds, No_with_bounds -> No_with_bounds
    | No_with_bounds, b -> b
    | b, No_with_bounds -> b
    | With_bounds tys1, With_bounds tys2 -> With_bounds (join_bounds tys1 tys2)

  let meet (type l1 l2) (bag1 : (l1 * allowed) t) (bag2 : (l2 * allowed) t) :
      (l1 * allowed) t =
    match bag1, bag2 with No_with_bounds, No_with_bounds -> No_with_bounds

  let add type_expr type_info bounds =
    match bounds with
    | No_with_bounds ->
      With_bounds (With_bounds_types.singleton type_expr type_info)
    | With_bounds bounds -> With_bounds (add_bound type_expr type_info bounds)

  let format (type l r) ppf (t : (l * r) t) =
    match t with
    | No_with_bounds -> ()
    | With_bounds wbs ->
      let type_exprs =
        wbs |> With_bounds_types.to_seq
        |> Seq.map (fun (ty, _) -> Format.asprintf "%a" !print_type_expr ty)
        |> List.of_seq
        (* HACK: we pre-format the types as strings so we so we can sort them
           lexicographically, because otherwise the order of printed [with]s is
           nondeterministic. This is sad, but we'd need deterministic sorting of types to
           work around it.

           CR aspsmith: remove this (and the same HACK in Oprint) if we ever add
           deterministic, semantic type comparison *)
        |> List.sort String.compare
      in
      Format.(
        fprintf ppf "%a"
          (pp_print_list (fun ppf -> fprintf ppf "with@ %s"))
          type_exprs)
end

(******************************)
(* context *)

type jkind_context =
  { jkind_of_type : Types.type_expr -> Types.jkind_l option;
    is_abstract : Path.t -> bool
  }

module Layout_and_axes = struct
  include Jkind0.Layout_and_axes

  let equal eq_layout
      { layout = lay1;
        mod_bounds = mod_bounds1;
        with_bounds = (No_with_bounds : (allowed * allowed) with_bounds)
      }
      { layout = lay2;
        mod_bounds = mod_bounds2;
        with_bounds = (No_with_bounds : (allowed * allowed) with_bounds)
      } =
    eq_layout lay1 lay2 && Mod_bounds.equal mod_bounds1 mod_bounds2

  let debug_print format_layout ppf { layout; mod_bounds; with_bounds } =
    Format.fprintf ppf "{ layout = %a;@ mod_bounds = %a;@ with_bounds = %a }"
      format_layout layout Mod_bounds.debug_print mod_bounds
      With_bounds.debug_print with_bounds

  type 'r normalize_mode =
    | Require_best : disallowed normalize_mode
    | Ignore_best : 'r normalize_mode

  module Fuel_status = struct
    type t =
      | Ran_out_of_fuel
      | Sufficient_fuel
  end

  (* Normalize the jkind. If mode is [Require_best], only jkinds that have quality [Best]
     will be used. If mode is [Ignore_best], then jkinds that have quality [Not_best] will
     also be used. Since [Ignore_best] can use [Not_best] jkinds, the result is guaranteed
     to have no with-bounds.

     At each step during normalization, before expanding a type, [map_type_info]
     is used to map the type-info for the type being expanded. The type can be
     prevented from being expanded by mapping the relevant axes to an empty
     set. [map_type_info] is used by sub_jkind_l to remove irrelevant axes.

     The [skip_axes] argument says which axes we can skip normalizing along. The behavior
     of this function for these axes is undefined; do *not* look at the results for these
     axes.
  *)
  let normalize : type layout l r1 r2.
      context:_ ->
      mode:r2 normalize_mode ->
      skip_axes:_ ->
      previously_ran_out_of_fuel:bool ->
      ?map_type_info:
        (type_expr -> With_bounds_type_info.t -> With_bounds_type_info.t) ->
      (layout, l * r1) layout_and_axes ->
      (layout, l * r2) layout_and_axes * Fuel_status.t =
   fun ~context ~mode ~skip_axes ~previously_ran_out_of_fuel ?map_type_info t ->
    (* handle a few common cases first, before doing anything else *)
    (* DEBUGGING
       Format.printf "@[normalize: %a@;  relevant_axes: %a@]@;"
         With_bounds.debug_print t.with_bounds Jkind_axis.Axis_set.print
         relevant_axes;
    *)
    match t with
    | { with_bounds = No_with_bounds; _ } as t -> t, Sufficient_fuel
    | { with_bounds = With_bounds tys; _ } as t
      when Axis_set.equal skip_axes Axis_set.all
           || With_bounds_types.is_empty tys ->
      { t with with_bounds = No_with_bounds }, Sufficient_fuel
    | _
      when Mod_bounds.is_max_within_set t.mod_bounds
             (Axis_set.complement skip_axes) ->
      { t with with_bounds = No_with_bounds }, Sufficient_fuel
    | _ ->
      (* Sadly, it seems hard (impossible?) to be sure to expand all types
         here without using a fuel parameter to stop infinite regress. Here
         is a nasty case:

         {[
           type zero
           type 'n succ

           type 'n loopy = Mk of 'n succ loopy list [@@unboxed]
         ]}

         First off: this type *is* inhabited, because of the [list] intervening
         type (which can be empty). It's also inhabited by various circular
         structures.

         But what's the jkind of ['n loopy]? It must be the jkind of
         ['n succ loopy list], which is [immutable_data with 'n succ loopy].
         In order to see if we shouldn't mode-cross, we have to expand the
         ['n succ loopy] in the jkind, but expanding that just yields the need
         to expand ['n succ succ loopy], and around we go.

         It seems hard to avoid this problem. And so we use fuel. Yet we want
         both a small amount of fuel (a type like [type t = K of (t * t) list]
         gets big very quickly) and a lot of fuel (we can imagine using a unit
         of fuel for each level of a deeply nested record structure). The
         compromise is to track fuel per type head, where a type head is either
         the path to a type constructor (like [t] or [loopy]) or a tuple.
         (We need to include tuples because of the possibility of recursive
         types and the fact that tuples track their element types in their
         jkind's with_bounds.)

         The initial fuel per type head is 10, as it seems hard to imagine that
         we're going to make meaningful progress if we've seen the same type
         head 10 times in one line of recursive descent. (This "one line of
         recursive descent" bit is why we recur separately down one type before
         iterating down the list.)
      *)
      let module Loop_control = struct
        (** Represents the cache for a specific type constructor *)
        type seen_constr =
          { fuel : int;
            seen_args : type_expr list;
                (** The arguments the type constructor was most recently seen
                    with. *)
            relevant_axes_when_seen : Axis_set.t
                (** The axes that were relevant when the type constructor was
                    most recently seen. *)
          }

        type seen_row_var =
          { relevant_axes_when_seen : Axis_set.t
                (** The axes that were relevant when the row var was seen. *)
          }
        [@@unboxed]

        type t =
          { tuple_fuel : int;
            seen_constrs : seen_constr Path.Map.t;
            seen_row_vars : seen_row_var Numbers.Int.Map.t;
            fuel_status : Fuel_status.t
          }

        type result =
          | Stop of t  (** give up, returning [max] *)
          | Skip  (** skip reducing this type, but otherwise continue *)
          | Continue of
              { ctl : t;
                skippable_axes : Axis_set.t
              }  (** continue, with a new [t] *)

        let initial_fuel_per_ty =
          (* Optimization: If we ran out of fuel while normalizing this jkind
             before, chances are we will again. So we reduce fuel to reduce work
             in this case. (We still use a little bit of fuel because a
             substitution may have enabled us to now terminate significantly
             faster.) In practice, this doesn't seem to make us reject any
             programs we care about. *)
          match previously_ran_out_of_fuel with
          | false -> 10
          | true -> 1

        let starting =
          { tuple_fuel = initial_fuel_per_ty;
            seen_constrs = Path.Map.empty;
            seen_row_vars = Numbers.Int.Map.empty;
            fuel_status = Sufficient_fuel
          }

        (* Note [Abstract types in normalization]
            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            Abstract types need to be handled differently from concrete types in jkind
            normalization. In particular, abstract types with recursion through with-
            bounds are problematic, as the following example found by Benjamin Peters
            shows:

             module type X = sig
               type t : value mod contended with t
             end
             module Xm : X = struct
               type t = int ref
             end
             type q : value mod contended = Xm.t  (* should be rejected *)

           If with-bounds in abstract types were treated the same as with-bounds in
           concrete recursive types, then `q : value mod contended` would be accepted,
           because the compiler reasons as follows:

             We want to normalize the jkind `value mod contended with t`.
             We compute the jkind of t, which is again `value mod contended with t`.
             The jkind normalization algorithm detects the infinite recursion, and cuts
             it off the second time, because we've already visited `t`.

           Cutting off the recursion makes the jkind normalization return the
           kind `value mod contended` for this abstract type. This is incorrect.

           Note that this cutoff mechanism *is* sound for concrete type definitions!
           Why is it sound for concrete but not abstract types?
           - For concrete types, we want to compute the *best* jkind that satisfies its
             recursion equation.
           - For abstract types, we must compute the *worst* jkind that satisfies its
             recursion equation, because an implementer of the signature is allowed to
             choose *any* type that satisfies the recursion equation.

           We accomplish this in the algorithm below by disabling the recursion cut-off
           for abstract types. This causes the algorithm to recurse until fuel runs out
           and then return the worst jkind conservatively.

           Another option would have been to change what happens when we detect infinite
           recursion: for concrete types, return best jkind, for abstract, return worst.
           However, because we think that users should not and will not typically write
           recursive kinds for abstract types, we choose to simply not detect infinite
           recursion for abstract types by not marking them as visited at all.
           This should be more efficient in the common case, because we avoid storing
           anything in the cache for abstract types.
           It should be slightly slower in the uncommon case when a user wrote a recursive
           jkind for an abstract type, because now instead of immediately detecting and
           cutting off the recursion, it continues until it runs out of fuel.
        *)

        let rec check ~relevant_axes
            ({ tuple_fuel; seen_constrs; seen_row_vars; fuel_status = _ } as t)
            ty =
          match Types.get_desc ty with
          | Tpoly (ty, _) -> check ~relevant_axes t ty
          | Ttuple _ ->
            if tuple_fuel > 0
            then
              Continue
                { ctl = { t with tuple_fuel = tuple_fuel - 1 };
                  skippable_axes = Axis_set.empty
                }
            else Stop { t with fuel_status = Ran_out_of_fuel }
          | Tconstr (p, args, _) -> (
            match Path.Map.find_opt p seen_constrs with
            | None ->
              Continue
                { ctl =
                    { t with
                      seen_constrs =
                        Path.Map.add p
                          { fuel = initial_fuel_per_ty;
                            seen_args = args;
                            relevant_axes_when_seen = relevant_axes
                          }
                          seen_constrs
                    };
                  skippable_axes = Axis_set.empty
                }
            | Some { fuel; seen_args; relevant_axes_when_seen } ->
              let args_equal =
                List.for_all2
                  (fun ty1 ty2 ->
                    TransientTypeOps.equal (Transient_expr.repr ty1)
                      (Transient_expr.repr ty2))
                  seen_args args
              in
              let skippable_axes =
                if args_equal && not (context.is_abstract p)
                then relevant_axes_when_seen
                else Axis_set.empty
              in
              if Axis_set.is_subset relevant_axes skippable_axes
              then Skip
              else if fuel > 0
              then
                Continue
                  { ctl =
                      { t with
                        seen_constrs =
                          Path.Map.add p
                            { fuel = fuel - 1;
                              seen_args = args;
                              relevant_axes_when_seen =
                                (* Even if we can't skip the type, if the args match
                                   the most recently seen args, we can merge the
                                   relevant axes with the ones seen previously since
                                   we have now seen the types under all of these
                                   axes. *)
                                (if args_equal
                                 then
                                   Axis_set.union relevant_axes
                                     relevant_axes_when_seen
                                 else relevant_axes)
                            }
                            seen_constrs
                      };
                    skippable_axes
                  }
              else Stop { t with fuel_status = Ran_out_of_fuel })
          | Tvariant _ ->
            let row_var_id = get_id (Btype.proxy ty) in
            let { relevant_axes_when_seen } =
              Numbers.Int.Map.find_opt row_var_id seen_row_vars
              |> Option.value
                   ~default:{ relevant_axes_when_seen = Axis_set.empty }
            in
            (* For our purposes, row variables are like constructors with no arguments,
               so if we saw one already, we don't need to expand it again. *)
            if Axis_set.is_subset relevant_axes relevant_axes_when_seen
            then Skip
            else
              Continue
                { ctl =
                    { t with
                      seen_row_vars =
                        Numbers.Int.Map.add row_var_id
                          { relevant_axes_when_seen =
                              Axis_set.union relevant_axes_when_seen
                                relevant_axes
                          }
                          seen_row_vars
                    };
                  skippable_axes = relevant_axes_when_seen
                }
          | Tvar _ | Tarrow _ | Tunboxed_tuple _ | Tobject _ | Tfield _ | Tnil
          | Tunivar _ | Tpackage _ | Tquote _ | Tsplice _ | Tof_kind _ ->
            (* these cases either cannot be infinitely recursive or their jkinds
               do not have with_bounds *)
            (* CR layouts v2.8: Some of these might get with-bounds someday. We
               should double-check before we're done that they haven't. *)
            Continue { ctl = t; skippable_axes = Axis_set.empty }
          | Tlink _ | Tsubst _ ->
            Misc.fatal_error "Tlink or Tsubst in normalize"
      end in
      let rec loop (ctl : Loop_control.t) bounds_so_far relevant_axes :
          (type_expr * With_bounds_type_info.t) list ->
          Mod_bounds.t * (l * r2) with_bounds * Loop_control.t = function
        (* early cutoff *)
        | [] -> bounds_so_far, No_with_bounds, ctl
        | _ when Mod_bounds.is_max_within_set bounds_so_far relevant_axes ->
          (* CR layouts v2.8: we can do better by early-terminating on a per-axis
             basis *)
          ( bounds_so_far,
            No_with_bounds,
            { ctl with fuel_status = Sufficient_fuel } )
        | (ty, ti) :: bs -> (
          (* Map the type's info before expanding the type *)
          let ti =
            match map_type_info with
            | None -> ti
            | Some map_type_info -> map_type_info ty ti
          in
          (* We don't care about axes that are already max because they can't get
             any better or worse. By ignoring them, we may be able to terminate
             early.

             We also don't care about axes that aren't in [relevant_axes]. *)
          let relevant_axes_for_ty =
            Axis_set.intersection
              (Axis_set.diff ti.relevant_axes
                 (Mod_bounds.get_max_axes bounds_so_far))
              relevant_axes
          in
          match Axis_set.is_empty relevant_axes_for_ty with
          | true ->
            (* If [ty] is not relevant to any axes, then we can safely drop it and
               thereby avoid doing the work of expanding it. *)
            loop ctl bounds_so_far relevant_axes bs
          | false -> (
            let join_bounds b1 b2 ~relevant_axes =
              let value_for_axis (type a) ~(axis : a Axis.t) : a =
                if Axis_set.mem relevant_axes axis
                then
                  (Per_axis.join [@inlined hint]) axis (Mod_bounds.get ~axis b1)
                    (Mod_bounds.get ~axis b2)
                else Mod_bounds.get ~axis b1
              in
              let monadic =
                Mod_bounds.Crossing.Monadic.create
                  ~uniqueness:
                    (value_for_axis ~axis:(Modal (Monadic Uniqueness)))
                  ~contention:
                    (value_for_axis ~axis:(Modal (Monadic Contention)))
                  ~visibility:
                    (value_for_axis ~axis:(Modal (Monadic Visibility)))
                  ~staticity:(value_for_axis ~axis:(Modal (Monadic Staticity)))
              in
              let comonadic =
                Mod_bounds.Crossing.Comonadic.create
                  ~regionality:
                    (value_for_axis ~axis:(Modal (Comonadic Areality)))
                  ~linearity:
                    (value_for_axis ~axis:(Modal (Comonadic Linearity)))
                  ~portability:
                    (value_for_axis ~axis:(Modal (Comonadic Portability)))
                  ~forkable:(value_for_axis ~axis:(Modal (Comonadic Forkable)))
                  ~yielding:(value_for_axis ~axis:(Modal (Comonadic Yielding)))
                  ~statefulness:
                    (value_for_axis ~axis:(Modal (Comonadic Statefulness)))
              in
              let crossing : Mod_bounds.Crossing.t = { monadic; comonadic } in
              Mod_bounds.create crossing
                ~externality:(value_for_axis ~axis:(Nonmodal Externality))
            in
            let found_jkind_for_ty ctl b_upper_bounds b_with_bounds quality
                skippable_axes :
                Mod_bounds.t * (l * r2) with_bounds * Loop_control.t =
              let relevant_axes_for_ty =
                Axis_set.diff relevant_axes_for_ty skippable_axes
              in
              match quality, mode with
              | Best, _ | Not_best, Ignore_best -> (
                (* The relevant axes are the intersection of the relevant axes within our
                   branch of the with-bounds tree, and the relevant axes on this
                   particular with-bound *)
                let bounds_so_far =
                  join_bounds bounds_so_far b_upper_bounds
                    ~relevant_axes:relevant_axes_for_ty
                in
                (* Descend into the with-bounds of each of our with-bounds types'
                    with-bounds *)
                let bounds_so_far, nested_with_bounds, ctl =
                  loop ctl bounds_so_far relevant_axes_for_ty
                    (With_bounds.to_list b_with_bounds)
                in
                match ctl.fuel_status, mode with
                | Ran_out_of_fuel, Ignore_best | Sufficient_fuel, _ ->
                  (* CR layouts v2.8: we use the same [ctl] here, to avoid big
                     quadratic stack growth for very widely recursive types. This is
                     sad, since it prevents us from mode crossing a record with 20
                     lists with different payloads, but less sad than a stack
                     overflow of the compiler during type declaration checking.

                     Ideally, this whole problem goes away once we rethink fuel.
                  *)
                  let bounds, bs', ctl =
                    loop ctl bounds_so_far relevant_axes bs
                  in
                  bounds, With_bounds.join nested_with_bounds bs', ctl
                | Ran_out_of_fuel, Require_best ->
                  (* See Note [Ran out of fuel when requiring best]. *)
                  Mod_bounds.max, No_with_bounds, ctl)
              | Not_best, Require_best ->
                (* CR layouts v2.8: The type annotation on the next line is
                   necessary only because [loop] is
                   local. Bizarre. Investigate. *)
                let bounds_so_far, (bs' : (l * r2) With_bounds.t), ctl =
                  loop ctl bounds_so_far relevant_axes bs
                in
                ( bounds_so_far,
                  With_bounds.add ty
                    { relevant_axes = relevant_axes_for_ty }
                    bs',
                  ctl )
            in
            match
              Loop_control.check ~relevant_axes:relevant_axes_for_ty ctl ty
            with
            | Stop ctl -> (
              match mode with
              | Ignore_best ->
                (* out of fuel, so assume [ty] has the worst possible bounds. *)
                found_jkind_for_ty ctl Mod_bounds.max No_with_bounds Not_best
                  Axis_set.empty [@nontail]
              | Require_best ->
                (* See Note [Ran out of fuel when requiring best]. *)
                Mod_bounds.max, No_with_bounds, ctl)
            | Skip -> loop ctl bounds_so_far relevant_axes bs (* skip [b] *)
            | Continue { ctl; skippable_axes } -> (
              match context.jkind_of_type ty with
              | Some b_jkind ->
                found_jkind_for_ty ctl b_jkind.jkind.mod_bounds
                  b_jkind.jkind.with_bounds b_jkind.quality skippable_axes
                [@nontail]
              | None ->
                (* kind of b is not principally known, so we treat it as having
                   the max bound (only along the axes we care about for this
                   type!) *)
                found_jkind_for_ty ctl Mod_bounds.max No_with_bounds Not_best
                  skippable_axes [@nontail])))
      in
      let mod_bounds = Mod_bounds.set_max_in_set t.mod_bounds skip_axes in
      let mod_bounds, with_bounds, ctl =
        loop Loop_control.starting mod_bounds
          (Axis_set.complement skip_axes)
          (With_bounds.to_list t.with_bounds)
      in
      let normalized_t : (layout, l * r2) layout_and_axes =
        match mode, ctl.fuel_status with
        | Require_best, Sufficient_fuel | Ignore_best, _ ->
          { t with mod_bounds; with_bounds }
        | Require_best, Ran_out_of_fuel ->
          (* Note [Ran out of fuel when requiring best]
             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             If we run out of fuel when in Require_best mode, and thus are
             unable to expand some type ['a u] ('a here is standing in for an
             arbitrary set of arguments), here's some reasonable strategies to
             handle this:

             1. Continue expanding remaining with-bounds and then add
                ['a u] to the with-bounds when done.
             2. Restart normalization from the beginning (or at least go back to
                the first occurrence of [u]) and blacklist [u]. That is, we
                never try to expand [u] - every time we see it, we simply add it
                to the output jkind's with-bounds.
             3. Just return the original jkind that we were given.

             Option 1 can result in a very large number of with-bounds in the
             output jkind because we may try to expand [u] a number of times
             that is exponential on the amount of fuel. As a result,
             normalization using this strategy can result in more complex
             jkinds, which can hurt performace. Thus, option 1 is undesirable.

             Option 2 seems to be a good solution to the problem with option 1,
             but it seems inefficient. But it seems possible that in practice
             this backtracking wouldn't be expensive because maybe it's rare to
             need to backtrack more than once.

             Option 3 is easy to implement and avoids the issue with option 1,
             so we choose this.

             Implementation note: Inside [loop], whenever we detect that we ran
             out of fuel (when in require-best mode), we bail out of the loop
             since there's no point in continuing. Instead, we return arbitrary
             mod- and with-bounds, because down here we detect the case and
             simply return [t].
          *)
          t |> disallow_right
      in
      normalized_t, ctl.fuel_status
end

(*********************************)

include Jkind0.Jkind

(***********************)
(*** constant jkinds ***)

module Context_with_transl = struct
  type 'd t =
    | Right_jkind :
        ('l * allowed) History.annotation_context
        -> ('l * allowed) t
    | Left_jkind :
        (Parsetree.core_type -> Types.type_expr)
        * (allowed * disallowed) History.annotation_context
        -> (allowed * disallowed) t

  let get_context : type l r. (l * r) t -> (l * r) History.annotation_context =
    function
    | Right_jkind ctx -> ctx
    | Left_jkind (_, ctx) -> ctx
end

(* CR layouts v2.8: This should sometimes be for type schemes, not types
   (which print weak variables like ['_a] correctly), but this works better
   for the common case. When we re-do printing, fix. Internal ticket 4435. *)
let outcometrees_of_types = ref (fun _ -> assert false)

let set_outcometrees_of_types p = outcometrees_of_types := p

let outcometree_of_modalities = ref (fun _ _ -> assert false)

let set_outcometree_of_modalities p = outcometree_of_modalities := p

module Const = struct
  include Jkind0.Const

  module To_out_jkind_const : sig
    (** Convert a [t] into a [Outcometree.out_jkind_const]. If [expanded] is
        [false], the jkind is written in terms of the built-in jkind that
        requires the least amount of modes after the mod. For example,
        [value mod global many unique portable uncontended external_ non_null]
        could be written in terms of [value] (as it appears above), or in terms
        of [immediate] (which would just be [immediate]). Since the latter
        requires less modes to be printed, it is chosen. *)
    val convert : expanded:bool -> 'd t -> Outcometree.out_jkind_const
  end = struct
    type printable_jkind =
      { base : string;
        scannable_axes : string list;
        modal_bounds : string list;
        printable_with_bounds :
          (Outcometree.out_type * Outcometree.out_modality list) list
      }

    (** [diff base actual] returns the axes on which [actual] is strictly
        stronger than [base], represented as a mod-bounds where unchanged axes
        are set to [max]. Returns [None] if [actual] isn't stronger than [base].
    *)
    let diff base actual =
      match Mod_bounds.less_or_equal actual base with
      | Not_le _ -> None
      | Equal -> Some Mod_bounds.max
      | Less ->
        let crossing_base = Mod_bounds.crossing base in
        let crossing_actual = Mod_bounds.crossing actual in
        let crossing_diff =
          List.fold_left
            (fun acc value_ax ->
              let (Crossing.Axis.P ax) =
                value_ax |> Modality.Axis.of_value |> Crossing.Axis.of_modality
              in
              let base_value = Crossing.proj ax crossing_base in
              let actual_value = Crossing.proj ax crossing_actual in
              (* [le] here implies equality. *)
              if Crossing.Per_axis.le ax base_value actual_value
              then acc
              else Crossing.set ax actual_value acc)
            Crossing.max Value.Axis.all
        in
        let externality =
          if
            Externality.equal
              (Mod_bounds.externality base)
              (Mod_bounds.externality actual)
          then Externality.max
          else Mod_bounds.externality actual
        in
        Some (Mod_bounds.create crossing_diff ~externality)

    let get_modal_bounds ~(base : Mod_bounds.t) (actual : Mod_bounds.t) =
      match diff base actual with
      | None -> None
      | Some diff ->
        let modes =
          Typemode.untransl_mod_bounds diff
          |> List.map (fun { Location.txt = Parsetree.Mode s; _ } -> s)
        in
        Some modes

    let get_scannable_axes_diff ~base actual =
      let base_sa = Layout.Const.get_root_scannable_axes base in
      let actual_sa = Layout.Const.get_root_scannable_axes actual in
      match base_sa, actual_sa with
      | None, _ | _, None -> []
      | Some base_sa, Some actual_sa ->
        Scannable_axes.to_string_list_diff ~base:base_sa actual_sa

    let modality_to_ignore_axes axes_to_ignore =
      (* The modality is constant along axes to ignore and id along others *)
      List.fold_left
        (fun acc (Axis.Pack axis) ->
          match axis with
          | Modal axis -> (
            match axis, Crossing.Per_axis.min axis with
            | Monadic ax, Modality t -> Modality.Const.set (Monadic ax) t acc
            | Comonadic ax, Modality t ->
              Modality.Const.set (Comonadic ax) t acc)
          | Nonmodal _ ->
            (* TODO: don't know how to print *)
            acc)
        Modality.Const.id
        (Axis_set.to_list axes_to_ignore)

    (** Write [actual] in terms of [base] *)
    let convert_with_base ~(base : Builtin.t) (actual : _ t) =
      let matching_layouts =
        Layout.Const.equal_up_to_scannable_axes base.jkind.layout actual.layout
      in
      let scannable_axes =
        if Layout.Const.is_scannable_or_any actual.layout
        then get_scannable_axes_diff ~base:base.jkind.layout actual.layout
        else []
      in
      let modal_bounds =
        get_modal_bounds ~base:base.jkind.mod_bounds actual.mod_bounds
      in
      let printable_with_bounds =
        (* This match statement is a bit of a hack. One usage of this function
           is to print jkind annotations on type variables while printing a
           type/signature. But outcometrees_of_types resets the printing
           environment, which shouldn't be done mid-printing. Fortunately, only
           r-kinds appear in a jkind annotation on a variable, meaning the list
           is always empty in this problematic case. Thus, if we don't call
           outcometrees_of_types when the list is empty, we'll be okay. We
           should fix this by being more deliberate about reseting the printing
           environment and preparing types for printing.
           Internal ticket 6133. *)
        match With_bounds.to_list actual.with_bounds with
        | [] -> []
        | with_bounds ->
          let otys = !outcometrees_of_types (List.map fst with_bounds) in
          List.map2
            (fun (_, type_info) out_type ->
              let axes_ignored_by_modalities =
                With_bounds.Type_info.axes_ignored_by_modalities
                  ~mod_bounds:actual.mod_bounds ~type_info
              in
              ( out_type,
                !outcometree_of_modalities Types.Immutable
                  (modality_to_ignore_axes axes_ignored_by_modalities) ))
            with_bounds otys
      in
      match matching_layouts, modal_bounds with
      | true, Some modal_bounds ->
        Some
          { base = base.name;
            scannable_axes;
            modal_bounds;
            printable_with_bounds
          }
      | false, _ | _, None -> None

    (** Select the out_jkind_const with the least number of modal bounds to
        print *)
    let rec select_simplest = function
      | a :: b :: tl ->
        let simpler =
          if
            List.length a.modal_bounds + List.length a.scannable_axes
            < List.length b.modal_bounds + List.length b.scannable_axes
          then a
          else b
        in
        select_simplest (simpler :: tl)
      | [out] -> Some out
      | [] -> None

    let convert ~expanded jkind =
      (* For each primitive jkind, we try to print the jkind in terms of it
         (this is possible if the primitive is a subjkind of it). We then choose
         the "simplest". The "simplest" is taken to mean the one with the least
         number of modes that need to follow the [mod]. *)
      let simplest =
        match expanded with
        | false ->
          Builtin.all
          |> List.filter_map (fun base -> convert_with_base ~base jkind)
          |> select_simplest
        | true -> None
      in
      let { base; scannable_axes; modal_bounds; printable_with_bounds } =
        match simplest with
        | Some simplest -> simplest
        | None ->
          (* CR layouts v2.8: sometimes there is no valid way to build a jkind
             from a built-in abbreviation. For now, we just pretend that the
             layout name is a valid jkind abbreviation whose modal bounds are
             all max, even though this is a lie. Internal ticket 3284. *)
          let out_jkind_verbose =
            convert_with_base
              ~base:
                { jkind =
                    (* Previously, adding a non-modal axis annotation (like
                       [mod separable]) would take the _meet_ with a built-in
                       abbreviation's point on that axis. These annotations (now
                       called scannable axes, not non-modal) now _override_ the
                       built-in abbreviation's point on the axis. Thus, we can
                       avoid setting to max and decreasing. If the overriding
                       behavior changes, this function should too. *)
                    { layout = jkind.layout;
                      mod_bounds = Mod_bounds.max;
                      with_bounds = No_with_bounds
                    };
                  name = Layout.Const.to_string jkind.layout
                }
              jkind
          in
          (* convert_with_base is guaranteed to succeed since the layout
             matches and the modal bounds are all max *)
          Option.get out_jkind_verbose
      in
      let base = Outcometree.Ojkind_const_abbreviation (base, scannable_axes) in
      (* Add on [mod] bounds, if there are any *)
      let base =
        if modal_bounds = []
        then base
        else Outcometree.Ojkind_const_mod (Some base, modal_bounds)
      in
      (* Finally, add on the [with]-types and their modalities *)
      List.fold_left
        (fun jkind (ty, modalities) ->
          Outcometree.Ojkind_const_with (jkind, ty, modalities))
        base printable_with_bounds
  end

  let to_out_jkind_const jkind =
    To_out_jkind_const.convert ~expanded:false jkind

  let format ~expanded ppf jkind =
    To_out_jkind_const.convert ~expanded jkind |> !Oprint.out_jkind_const ppf

  (*******************************)
  (* converting user annotations *)

  let set_nullability ~abbrev (nul : Nullability.t Location.loc option) t =
    match nul with
    | None -> t
    | Some { txt = new_nullability; loc } ->
      (match Layout.Const.get_root_scannable_axes t.layout with
      | None -> ()
      | Some { nullability; separability = _ } ->
        if new_nullability = nullability
        then
          Location.prerr_warning loc (Warnings.Redundant_kind_modifier abbrev));
      let new_layout =
        Layout.Const.set_root_nullability t.layout new_nullability
      in
      { t with layout = new_layout }

  let set_separability ~abbrev (sep : Separability.t Location.loc option) t =
    match sep with
    | None -> t
    | Some { txt = new_separability; loc } ->
      (match Layout.Const.get_root_scannable_axes t.layout with
      | None -> ()
      | Some { nullability = _; separability } ->
        if new_separability = separability
        then
          Location.prerr_warning loc (Warnings.Redundant_kind_modifier abbrev));
      let new_layout =
        Layout.Const.set_root_separability t.layout new_separability
      in
      { t with layout = new_layout }

  let meet_nullability (nul : Nullability.t Location.loc option) t =
    match nul with
    | None -> t
    | Some { txt = new_nullability; loc = _ } -> (
      match Layout.Const.get_root_scannable_axes t.layout with
      | None -> t
      | Some { nullability; separability = _ } ->
        let result_nullability = Nullability.meet nullability new_nullability in
        let new_layout =
          Layout.Const.set_root_nullability t.layout result_nullability
        in
        { t with layout = new_layout })

  let meet_separability (sep : Separability.t Location.loc option) t =
    match sep with
    | None -> t
    | Some { txt = new_separability; loc = _ } -> (
      match Layout.Const.get_root_scannable_axes t.layout with
      | None -> t
      | Some { nullability = _; separability } ->
        let result_separability =
          Separability.meet separability new_separability
        in
        let new_layout =
          Layout.Const.set_root_separability t.layout result_separability
        in
        { t with layout = new_layout })

  let jkind_of_product_annotations (type l r) (jkinds : (l * r) t list) =
    let folder (type l r) (layouts_acc, mod_bounds_acc, with_bounds_acc)
        ({ layout; mod_bounds; with_bounds } : (l * r) t) =
      ( layout :: layouts_acc,
        Mod_bounds.join mod_bounds mod_bounds_acc,
        With_bounds.join with_bounds with_bounds_acc )
    in
    let layouts, mod_bounds, with_bounds =
      List.fold_left folder ([], Mod_bounds.min, No_with_bounds) jkinds
    in
    { layout = Layout.Const.Product (List.rev layouts);
      mod_bounds;
      with_bounds
    }

  let transl_scannable_axes sa_annots =
    let set_or_warn ~loc ~to_ ~to_string cur_axis =
      match cur_axis with
      | Some overridden_by ->
        Location.prerr_warning loc
          (Warnings.Overridden_kind_modifier
             (to_string (Location.get_txt overridden_by)));
        cur_axis
      | None -> Some (Location.mkloc to_ loc)
    in
    (* This will compute and report errors from right-to-left, which enables
       better error messages while traversing the list only once. It comes at
       the cost of warnings being reported in a slightly weirder order. *)
    List.fold_right
      (fun ({ txt; loc } : string Location.loc) (nullability, separability) ->
        match txt with
        | "non_pointer" ->
          ( nullability,
            Separability.(
              set_or_warn ~loc ~to_:Non_pointer ~to_string separability) )
        | "non_pointer64" ->
          ( nullability,
            Separability.(
              set_or_warn ~loc ~to_:Non_pointer64 ~to_string separability) )
        | "non_float" ->
          ( nullability,
            Separability.(
              set_or_warn ~loc ~to_:Non_float ~to_string separability) )
        | "separable" ->
          ( nullability,
            Separability.(
              set_or_warn ~loc ~to_:Separable ~to_string separability) )
        | "maybe_separable" ->
          ( nullability,
            Separability.(
              set_or_warn ~loc ~to_:Maybe_separable ~to_string separability) )
        | "non_null" ->
          ( Nullability.(set_or_warn ~loc ~to_:Non_null ~to_string nullability),
            separability )
        | "maybe_null" ->
          ( Nullability.(set_or_warn ~loc ~to_:Maybe_null ~to_string nullability),
            separability )
        | _ -> raise ~loc (Unknown_kind_modifier txt))
      sa_annots (None, None)

  let rec of_user_written_annotation_unchecked_level : type l r.
      (l * r) Context_with_transl.t -> Parsetree.jkind_annotation -> (l * r) t =
   fun context jkind ->
    match jkind.pjkind_desc with
    | Pjk_abbreviation (name, sa_annot) ->
      (* CR layouts v2.8: move this to predef. Internal ticket 3339. *)
      let jkind_without_sa =
        (match name.txt with
          | "any" -> Builtin.any.jkind
          | "scannable" -> Builtin.scannable.jkind
          | "value_or_null" -> Builtin.value_or_null.jkind
          | "value" -> Builtin.value.jkind
          | "void" -> Builtin.void.jkind
          | "immediate64" -> Builtin.immediate64.jkind
          | "immediate64_or_null" -> Builtin.immediate64_or_null.jkind
          | "immediate" -> Builtin.immediate.jkind
          | "immediate_or_null" -> Builtin.immediate_or_null.jkind
          | "float64" -> Builtin.float64.jkind
          | "float32" -> Builtin.float32.jkind
          | "word" -> Builtin.word.jkind
          | "untagged_immediate" -> Builtin.untagged_immediate.jkind
          | "bits8" -> Builtin.bits8.jkind
          | "bits16" -> Builtin.bits16.jkind
          | "bits32" -> Builtin.bits32.jkind
          | "bits64" -> Builtin.bits64.jkind
          | "vec128" -> Builtin.vec128.jkind
          | "vec256" -> Builtin.vec256.jkind
          | "vec512" -> Builtin.vec512.jkind
          | "immutable_data" -> Builtin.immutable_data.jkind
          | "sync_data" -> Builtin.sync_data.jkind
          | "mutable_data" -> Builtin.mutable_data.jkind
          | _ -> raise ~loc:jkind.pjkind_loc (Unknown_jkind jkind))
        |> allow_left |> allow_right
      in
      let nullability, separability = transl_scannable_axes sa_annot in
      if
        sa_annot <> []
        && not (Layout.Const.is_scannable_or_any jkind_without_sa.layout)
      then
        Location.prerr_warning jkind.pjkind_loc
          (Warnings.Ignored_kind_modifier
             (name.txt, List.map Location.get_txt sa_annot));
      jkind_without_sa
      |> set_nullability ~abbrev:name.txt nullability
      |> set_separability ~abbrev:name.txt separability
    | Pjk_mod (base, modifiers) ->
      let base = of_user_written_annotation_unchecked_level context base in
      (* for each mode, lower the corresponding modal bound to be that mode *)
      let mod_bounds, (nullability, separability) =
        Typemode.transl_mod_bounds modifiers
      in
      let mod_bounds = Mod_bounds.meet base.mod_bounds mod_bounds in
      { layout = base.layout; mod_bounds; with_bounds = No_with_bounds }
      |> meet_nullability nullability
      |> meet_separability separability
    | Pjk_product ts ->
      let jkinds =
        List.map (of_user_written_annotation_unchecked_level context) ts
      in
      jkind_of_product_annotations jkinds
    | Pjk_with (base, type_, modalities) -> (
      let base = of_user_written_annotation_unchecked_level context base in
      match context with
      | Right_jkind _ -> raise ~loc:type_.ptyp_loc With_on_right
      | Left_jkind (transl_type, _) ->
        let type_ = transl_type type_ in
        let modality =
          Typemode.transl_modalities ~maturity:Stable Immutable modalities
        in
        { layout = base.layout;
          mod_bounds = base.mod_bounds;
          with_bounds =
            With_bounds.add_modality ~modality ~type_expr:type_ base.with_bounds
        })
    | Pjk_default | Pjk_kind_of _ ->
      raise ~loc:jkind.pjkind_loc Unimplemented_syntax

  (* The [annotation_context] parameter can be used to allow annotations / kinds
     in different contexts to be enabled with different extension settings.
     At some points in time, we will not care about the context, and so this
     parameter might effectively be unused.
  *)
  (* CR layouts: When everything is stable, remove this function. *)
  let get_required_layouts_level (_context : 'd Context_with_transl.t)
      (jkind : 'd t) =
    let rec scan_layout (l : Layout.Const.t) : Language_extension.maturity =
      match l with
      | Base
          ( ( Scannable | Float64 | Float32 | Word | Bits8 | Bits16 | Bits32
            | Bits64 | Vec128 | Vec256 | Vec512 | Untagged_immediate ),
            _ )
      | Any _ ->
        Stable
      | Product layouts ->
        List.fold_left
          (fun m l -> Language_extension.Maturity.max m (scan_layout l))
          Language_extension.Stable layouts
      | Base (Void, _) -> Stable
    in
    scan_layout jkind.layout

  let of_user_written_annotation ~context (annot : Parsetree.jkind_annotation) =
    let const = of_user_written_annotation_unchecked_level context annot in
    let required_layouts_level = get_required_layouts_level context const in
    if not (Language_extension.is_at_least Layouts required_layouts_level)
    then
      raise ~loc:annot.pjkind_loc
        (Insufficient_level { jkind = annot; required_layouts_level });
    const
end

module Desc = struct
  type 'd t = (Sort.Flat.t Layout.t, 'd) layout_and_axes

  let get_const t = Layout_and_axes.map_option Layout.get_flat_const t

  (* CR layouts v2.8: This will probably need to be overhauled with
     [with]-types. See also [Printtyp.out_jkind_of_desc], which uses the same
     algorithm. Internal ticket 5096. *)
  let format_maybe_expanded ~expanded ppf t =
    let open Format in
    let rec format_desc ~nested ppf (desc : _ t) =
      match desc.layout with
      | Sort (Var n, sa) ->
        let sort_var_str = asprintf "'s%d" (Sort.Var.get_print_number n) in
        (pp_print_list ~pp_sep:(fun f () -> fprintf f " ") pp_print_string)
          ppf
          (sort_var_str :: Scannable_axes.to_string_list sa)
      (* Analyze a product before calling [get_const]: the machinery in
         [Const.format] works better for atomic layouts, not products. *)
      | Product lays ->
        let pp_sep ppf () = fprintf ppf "@ & " in
        Misc.pp_nested_list ~nested ~pp_element:format_desc ~pp_sep ppf
          (List.map (fun layout -> { desc with layout }) lays)
      | _ -> (
        match get_const desc with
        | Some c -> Const.format ~expanded ppf c
        | None -> assert false (* handled above *))
    in
    format_desc ppf ~nested:false t

  let format ppf t = format_maybe_expanded ~expanded:false ppf t
end

module Jkind_desc = struct
  let unsafely_set_bounds t ~from =
    { t with mod_bounds = from.mod_bounds; with_bounds = from.with_bounds }

  let equate_or_equal ~allow_mutation ~level t1 t2 =
    Layout_and_axes.equal (Layout.equate_or_equal ~allow_mutation ~level) t1 t2

  let sub (type l r) ~type_equal:_ ~context ~level
      ~sub_previously_ran_out_of_fuel (sub : (allowed * r) jkind_desc)
      ({ layout = lay2; mod_bounds = bounds2; with_bounds = No_with_bounds } :
        (l * allowed) jkind_desc) =
    let axes_max_on_right =
      (* Optimization: if the upper_bound is max on the right, then that axis is
         irrelevant - the left will always satisfy the right along that axis. *)
      Mod_bounds.get_max_axes bounds2
    in
    let ( ({ layout = lay1; mod_bounds = bounds1; with_bounds = No_with_bounds } :
            (_ * allowed) jkind_desc),
          _ ) =
      Layout_and_axes.normalize ~skip_axes:axes_max_on_right
        ~previously_ran_out_of_fuel:sub_previously_ran_out_of_fuel
        ~mode:Ignore_best ~context sub
    in
    let layout = Layout.sub ~level lay1 lay2 in
    let bounds = Mod_bounds.less_or_equal bounds1 bounds2 in
    Sub_result.combine layout bounds

  let intersection ~level
      { layout = lay1; mod_bounds = mod_bounds1; with_bounds = with_bounds1 }
      { layout = lay2; mod_bounds = mod_bounds2; with_bounds = with_bounds2 } =
    match Layout.intersection ~level lay1 lay2 with
    | None -> None
    | Some layout ->
      Some
        { layout;
          mod_bounds = Mod_bounds.meet mod_bounds1 mod_bounds2;
          with_bounds = With_bounds.meet with_bounds1 with_bounds2
        }

  let of_new_sort_var ~level sa =
    let layout, sort = Layout.of_new_sort_var ~level sa in
    { layout; mod_bounds = Mod_bounds.max; with_bounds = No_with_bounds }, sort

  let get t = Layout_and_axes.map Layout.get t

  module Debug_printers = struct
    let t ppf t =
      Layout_and_axes.debug_print
        (Layout.Debug_printers.t Sort.Debug_printers.t)
        ppf t
  end
end

(******************************)
(* constants *)

let is_best t = match t.quality with Best -> true | Not_best -> false

let unsafely_set_bounds (type l r) ~(from : (l * r) jkind) t =
  { t with jkind = Jkind_desc.unsafely_set_bounds t.jkind ~from:from.jkind }

(******************************)
(* construction *)

let of_new_sort_var ~why ~level =
  let jkind, sort = Jkind_desc.of_new_sort_var ~level Scannable_axes.max in
  fresh_jkind jkind ~annotation:None ~why:(Concrete_creation why), sort

let of_new_sort ~why ~level = fst (of_new_sort_var ~why ~level)

let of_new_legacy_sort_var ~why ~level =
  let jkind, sort =
    Jkind_desc.of_new_sort_var ~level Scannable_axes.value_axes
  in
  fresh_jkind jkind ~annotation:None ~why:(Concrete_legacy_creation why), sort

let of_new_non_float_sort_var ~why ~level =
  let jkind, sort =
    Jkind_desc.of_new_sort_var ~level
      { nullability = Maybe_null; separability = Non_float }
  in
  fresh_jkind jkind ~annotation:None ~why:(Concrete_creation why), sort

let of_new_legacy_sort ~why ~level = fst (of_new_legacy_sort_var ~why ~level)

let of_annotated_const ~context ~annotation ~const ~const_loc =
  let context = Context_with_transl.get_context context in
  of_const ~annotation
    ~why:(Annotated (context, const_loc))
    const ~quality:Not_best ~ran_out_of_fuel_during_normalize:false

let of_annotation_lr ~context (annot : Parsetree.jkind_annotation) =
  let const = Const.of_user_written_annotation ~context annot in
  of_annotated_const ~annotation:(Some annot) ~const ~const_loc:annot.pjkind_loc
    ~context

let of_annotation ~context annot =
  of_annotation_lr ~context:(Right_jkind context) annot

let of_annotation_option_default ~default ~context = function
  | None -> default
  | Some annot -> of_annotation ~context annot

let of_attribute ~context
    (attribute : Builtin_attributes.jkind_attribute Location.loc) =
  let ({ jkind = const; name } : Const.Builtin.t) =
    Const.Builtin.of_attribute attribute.txt
  in
  of_annotated_const ~context ~annotation:(mk_annot name) ~const
    ~const_loc:attribute.loc

let of_type_decl ~context ~transl_type (decl : Parsetree.type_declaration) =
  let context = Context_with_transl.Left_jkind (transl_type, context) in
  let jkind_of_annotation =
    decl.ptype_jkind_annotation
    |> Option.map (fun annot -> of_annotation_lr ~context annot, annot)
  in
  let jkind_of_attribute =
    Builtin_attributes.jkind decl.ptype_attributes
    |> Option.map (fun attr ->
        (of_attribute ~context attr |> disallow_right, None), attr)
  in
  match jkind_of_annotation, jkind_of_attribute with
  | None, None -> None
  | Some (jkind, annot), None -> Some (jkind, Some annot)
  | None, Some (jkind_with_annot, _) -> Some jkind_with_annot
  | Some (_, from_annotation), Some (_, from_attribute) ->
    raise ~loc:decl.ptype_loc
      (Multiple_jkinds { from_annotation; from_attribute })

let of_type_decl_default ~context ~transl_type ~default
    (decl : Parsetree.type_declaration) =
  match of_type_decl ~context ~transl_type decl with
  | Some (t, _) -> t
  | None -> default

let for_unboxed_record lbls layouts =
  let open Types in
  let tys_modalities =
    List.map (fun lbl -> lbl.ld_type, lbl.ld_modalities) lbls
  in
  Builtin.product ~why:Unboxed_record tys_modalities layouts

let for_abbreviation ~type_jkind_purely ~modality ty =
  (* CR layouts v2.8: This should really use layout_of. Internal ticket 2912. *)
  let jkind = type_jkind_purely ty in
  let with_bounds_types =
    let relevant_axes = Mod_bounds.relevant_axes_of_modality ~modality in
    With_bounds_types.singleton ty { relevant_axes }
  in
  fresh_jkind_poly
    { layout = jkind.jkind.layout;
      mod_bounds = Mod_bounds.min;
      with_bounds = With_bounds with_bounds_types
    }
    ~annotation:None ~why:Abbreviation

let for_boxed_tuple elts =
  List.fold_right
    (fun (_, type_expr) ->
      add_with_bounds ~modality:Mode.Modality.Const.id ~type_expr)
    elts
    (Builtin.immutable_data ~why:Tuple |> mark_best)

let for_open_boxed_row =
  let mod_bounds =
    Mod_bounds.create Crossing.max ~externality:Externality.max
  in
  fresh_jkind
    { layout =
        Sort
          (Base Scannable, { nullability = Non_null; separability = Non_float });
      mod_bounds;
      with_bounds = No_with_bounds
    }
    ~annotation:None ~why:(Value_creation Polymorphic_variant)

let limit_for_mode_crossing_rows = 100

let for_boxed_row row =
  if Btype.tvariant_not_immediate row
  then
    if not (Btype.static_row row)
    then
      (* CR layouts v2.8: We can probably do a fair bit better here in most cases. But if we ever allow open types to mode-cross, we have to get rid of the 100-row limit below. Internal ticket 5097 and 5098. *)
      for_open_boxed_row
    else
      (* Here we count how many rows are in the polymorphic variant and default
         to value if there are more than 100. This is to avoid regressions in
         compilation time, which was observed in some files with large
         polymorphic variants.

         We choose to make two different calls to [Btype.fold_row] to avoid
         doing allocations in the case where there's a large number of variants,
         as those allocations were enough to slow down the problematic files
         with large polymorphic variants. Presumably the second loop is fast
         anyways due to caching. *)
      (* CR layouts v2.8: Remove this [limit_for_mode_crossing_rows]
         restriction. See internal ticket 5435. *)
      let bounds_count = Btype.fold_row (fun acc _ -> acc + 1) 0 row in
      if bounds_count <= limit_for_mode_crossing_rows
      then
        let base = Builtin.immutable_data ~why:Polymorphic_variant in
        Btype.fold_row
          (fun jkind type_expr ->
            add_with_bounds ~modality:Mode.Modality.Const.id ~type_expr jkind)
          base row
        |> mark_best
      else Builtin.value ~why:Polymorphic_variant_too_big
  else Builtin.immediate ~why:Immediate_polymorphic_variant

let for_arrow =
  fresh_jkind
    { layout =
        Sort
          (Base Scannable, { nullability = Non_null; separability = Non_float });
      mod_bounds = Mod_bounds.for_arrow;
      with_bounds = No_with_bounds
    }
    ~annotation:None ~why:(Value_creation Arrow)
  |> mark_best

let for_object =
  (* The crossing of objects are based on the fact that they are
     produced/defined/allocated at legacy, which applies to only the
     comonadic axes. *)
  let comonadic =
    Crossing.Comonadic.always_constructed_at Value.Comonadic.Const.legacy
  in
  let monadic =
    Crossing.Monadic.create
      ~uniqueness:(Crossing.Per_axis.min (Crossing.Axis.Monadic Uniqueness))
        (* Since [global] implies [aliased] in presence of borrowing,
           objects also cross uniqueness. *)
      ~contention:(Crossing.Per_axis.max (Crossing.Axis.Monadic Contention))
      ~visibility:(Crossing.Per_axis.max (Crossing.Axis.Monadic Visibility))
      ~staticity:(Crossing.Per_axis.max (Crossing.Axis.Monadic Staticity))
  in
  fresh_jkind
    { layout =
        Sort
          (Base Scannable, { nullability = Non_null; separability = Non_float });
      mod_bounds =
        Mod_bounds.create { comonadic; monadic } ~externality:Externality.max;
      with_bounds = No_with_bounds
    }
    ~annotation:None ~why:(Value_creation Object)

let for_array_element_sort ~level =
  let jkind_desc, sort =
    Jkind_desc.of_new_sort_var ~level
      { nullability = Maybe_null; separability = Separable }
  in
  let jkind = { for_array_argument.jkind with layout = jkind_desc.layout } in
  ( fresh_jkind jkind ~annotation:None ~why:(Concrete_creation Array_element),
    sort )

(******************************)
(* elimination and defaulting *)

type normalize_mode =
  | Require_best
  | Ignore_best

let[@inline] normalize ~mode ~context t =
  let mode : _ Layout_and_axes.normalize_mode =
    match mode with Require_best -> Require_best | Ignore_best -> Ignore_best
  in
  let jkind, fuel_result =
    Layout_and_axes.normalize ~context ~skip_axes:Axis_set.empty
      ~previously_ran_out_of_fuel:t.ran_out_of_fuel_during_normalize ~mode
      t.jkind
  in
  { t with
    jkind;
    quality =
      (match t.quality, fuel_result with
      | Not_best, _ | _, Ran_out_of_fuel -> Not_best
      | Best, Sufficient_fuel -> Best);
    ran_out_of_fuel_during_normalize =
      (match fuel_result with
      | Ran_out_of_fuel -> true
      | _ -> t.ran_out_of_fuel_during_normalize)
  }

let get_layout_defaulting_to_scannable { jkind = { layout; _ }; _ } =
  Layout.default_to_scannable_and_get layout

let default_to_scannable t = ignore (get_layout_defaulting_to_scannable t)

let get t = Jkind_desc.get t.jkind

(* CR layouts: this function is suspect; it seems likely to reisenberg
   that refactoring could get rid of it *)
let sort_of_jkind (t : jkind_l) : sort =
  let rec sort_of_layout (t : _ Layout.t) =
    match t with
    | Any _ -> Misc.fatal_error "Jkind.sort_of_jkind"
    | Sort (s, _) -> s
    | Product ls -> Sort.Product (List.map sort_of_layout ls)
  in
  sort_of_layout t.jkind.layout

let get_layout jk : Layout.Const.t option = Layout.get_const jk.jkind.layout

let extract_layout jk = jk.jkind.layout

let get_root_scannable_axes jk = Layout.get_root_scannable_axes jk.jkind.layout

let get_mode_crossing (type l r) ~context (jk : (l * r) jkind) =
  let ( ({ layout = _; mod_bounds; with_bounds = No_with_bounds } :
          (_ * allowed) jkind_desc),
        _ ) =
    Layout_and_axes.normalize ~mode:Ignore_best
      ~skip_axes:Axis_set.all_nonmodal_axes
      ~previously_ran_out_of_fuel:jk.ran_out_of_fuel_during_normalize ~context
      jk.jkind
  in
  Mod_bounds.crossing mod_bounds

let to_unsafe_mode_crossing jkind =
  { unsafe_mod_bounds = Mod_bounds.to_mode_crossing jkind.jkind.mod_bounds;
    unsafe_with_bounds = jkind.jkind.with_bounds
  }

let all_except_externality =
  Axis_set.singleton (Nonmodal Externality) |> Axis_set.complement

let get_externality_upper_bound ~context jk =
  let ( ({ layout = _; mod_bounds; with_bounds = No_with_bounds } :
          (_ * allowed) jkind_desc),
        _ ) =
    Layout_and_axes.normalize ~mode:Ignore_best
      ~skip_axes:all_except_externality
      ~previously_ran_out_of_fuel:jk.ran_out_of_fuel_during_normalize ~context
      jk.jkind
  in
  Mod_bounds.get mod_bounds ~axis:(Nonmodal Externality)

let set_externality_upper_bound jk externality_upper_bound =
  { jk with
    jkind =
      { jk.jkind with
        mod_bounds =
          Mod_bounds.set_externality externality_upper_bound jk.jkind.mod_bounds
      }
  }

let get_nullability jk =
  get_root_scannable_axes jk
  |> Option.map (fun ({ nullability; _ } : Scannable_axes.t) -> nullability)

let set_root_nullability jk nullability =
  { jk with
    jkind =
      { jk.jkind with
        layout = Layout.set_root_nullability jk.jkind.layout nullability
      }
  }

let set_root_separability jk separability =
  { jk with
    jkind =
      { jk.jkind with
        layout = Layout.set_root_separability jk.jkind.layout separability
      }
  }

let set_layout jk layout = { jk with jkind = { jk.jkind with layout } }

let apply_modality_l modality jk =
  let relevant_axes = Mod_bounds.relevant_axes_of_modality ~modality in
  let mod_bounds =
    Mod_bounds.set_min_in_set jk.jkind.mod_bounds
      (Axis_set.complement relevant_axes)
  in
  let with_bounds =
    With_bounds.map
      (fun ti ->
        { relevant_axes = Axis_set.intersection ti.relevant_axes relevant_axes })
      jk.jkind.with_bounds
  in
  { jk with jkind = { jk.jkind with mod_bounds; with_bounds } }
  |> disallow_right

let apply_modality_r modality jk =
  let relevant_axes = Mod_bounds.relevant_axes_of_modality ~modality in
  let mod_bounds =
    Mod_bounds.set_max_in_set jk.jkind.mod_bounds
      (Axis_set.complement relevant_axes)
  in
  { jk with jkind = { jk.jkind with mod_bounds } } |> disallow_left

let apply_or_null_l jkind =
  match get_root_scannable_axes jkind with
  | Some { nullability = Non_null; separability } ->
    let jkind = set_root_nullability jkind Maybe_null in
    let jkind =
      match separability with
      | Maybe_separable -> jkind
      | Separable -> set_root_separability jkind Maybe_separable
      | Non_float | Non_pointer64 | Non_pointer -> jkind
    in
    Ok jkind
  | Some { nullability = Maybe_null; separability = _ } | None -> Error ()

let apply_or_null_r jkind =
  match get_root_scannable_axes jkind with
  | Some { nullability = Maybe_null; separability } ->
    let jkind = set_root_nullability jkind Non_null in
    let jkind =
      match separability with
      | Maybe_separable -> jkind
      | Separable -> set_root_separability jkind Non_float
      | Non_float | Non_pointer64 | Non_pointer -> jkind
    in
    Ok jkind
  | Some { nullability = Non_null; separability = _ } -> Error ()
  | None ->
    Misc.fatal_error "or_null applied to a type without a scannable layout"

let get_annotation jk = jk.annotation

let decompose_product ({ jkind; _ } as jk) =
  let mk_jkind layout = { jk with jkind = { jkind with layout } } in
  let deal_with_sort : Sort.t -> _ = function
    | Var _ -> None (* we've called [get] and there's *still* a variable *)
    | Base _ -> None
    | Product sorts ->
      Some
        (List.map
           (fun sort -> mk_jkind (Sort (sort, Scannable_axes.max)))
           sorts)
  in
  match jkind.layout with
  | Any _ -> None
  | Product layouts ->
    (* CR layouts v7.1: The histories here are wrong (we are giving each
       component the history of the whole product).  They don't show up in
       errors, so it's fine for now, but we'll probably need to fix this as
       part of improving errors around products. A couple options: re-work the
       relevant bits of [Ctype.type_jkind_sub] to just work on layouts, or
       introduce product histories. *)
    Some (List.map mk_jkind layouts)
  | Sort (s, _) -> deal_with_sort (Sort.get s)

(*********************************)
(* pretty printing *)

(* CR layouts v2.8: This is the spot where we could print the annotation in
   the jkind, if there is one. But actually the output seems better without
   doing so, because it teaches the user that e.g. [value mod local] is better
   off spelled [value]. Possibly remove [jkind.annotation], but only after
   we have a proper printing story. Internal ticket 5096. *)
let format_maybe_expanded ~expanded ppf jkind =
  Desc.format_maybe_expanded ~expanded ppf (Jkind_desc.get jkind.jkind)

let format ppf jkind = format_maybe_expanded ~expanded:false ppf jkind

let format_expanded ppf jkind = format_maybe_expanded ~expanded:true ppf jkind

let printtyp_path = ref (fun _ _ -> assert false)

let set_printtyp_path f = printtyp_path := f

module Report_missing_cmi : sig
  (* used both in format_history and in Violation.report_general *)
  val report_missing_cmi : Format.formatter -> Path.t option -> unit
end = struct
  open Format

  (* CR layouts: Remove this horrible (but useful) heuristic once we have
     transitive dependencies in jenga. *)
  let missing_cmi_hint ppf type_path =
    let root_module_name p = p |> Path.head |> Ident.name in
    let delete_trailing_double_underscore s =
      if Misc.Stdlib.String.ends_with ~suffix:"__" s
      then String.sub s 0 (String.length s - 2)
      else s
    in
    (* A heuristic for guessing at a plausible library name for an identifier
       with a missing .cmi file; definitely less likely to be right outside of
       Jane Street. *)
    let guess_library_name : Path.t -> string option = function
      | Pdot _ as p ->
        Some
          (match root_module_name p with
          | "Location" | "Longident" -> "ocamlcommon"
          | mn ->
            mn |> String.lowercase_ascii |> delete_trailing_double_underscore)
      | Pident _ | Papply _ | Pextra_ty _ -> None
    in
    Option.iter
      (fprintf ppf "@,Hint: Adding \"%s\" to your dependencies might help.")
      (guess_library_name type_path)

  let report_missing_cmi ppf = function
    | Some p ->
      fprintf ppf "@,@[No .cmi file found containing %a.%a@]" !printtyp_path p
        missing_cmi_hint p
    | None -> ()
end

include Report_missing_cmi

(* CR layouts: should this be configurable? In the meantime, you
   may want to change these to experiment / debug. *)

(* should we print histories at all? *)
let display_histories = true

(* should we print histories in a way users can understand?
   The alternative is to print out all the data, which may be useful
   during debugging. *)
let flattened_histories = true

(* This module is just to keep all the helper functions more locally
   scoped. *)
module Format_history = struct
  (* CR layouts: all the output in this section is subject to change;
     actually look closely at error messages once this is activated *)

  open Format

  let format_with_notify_js ppf str =
    fprintf ppf
      "@[%s.@ Please notify the Jane Street compilers group if you see this \
       output@]"
      str

  let format_position ~arity position =
    let to_ordinal num = Int.to_string num ^ Misc.ordinal_suffix num in
    match arity with 1 -> "" | _ -> to_ordinal position ^ " "

  let format_concrete_creation_reason ppf :
      History.concrete_creation_reason -> unit = function
    | Match -> fprintf ppf "a value of this type is matched against a pattern"
    | Constructor_declaration _ ->
      fprintf ppf "it's the type of a constructor field"
    | Label_declaration lbl ->
      fprintf ppf "it is the type of record field %s" (Ident.name lbl)
    | Record_projection ->
      fprintf ppf "it's the record type used in a projection"
    | Record_assignment ->
      fprintf ppf "it's the record type used in an assignment"
    | Record_functional_update ->
      fprintf ppf "it's the record type used in a functional update"
    | Let_binding -> fprintf ppf "it's the type of a variable bound by a `let`"
    | Function_argument ->
      fprintf ppf "we must know concretely how to pass a function argument"
    | Function_result ->
      fprintf ppf "we must know concretely how to return a function result"
    | Structure_item_expression ->
      fprintf ppf "it's the type of an expression in a structure"
    | External_argument ->
      fprintf ppf "it's the type of an argument in an external declaration"
    | External_result ->
      fprintf ppf "it's the type of the result of an external declaration"
    | Statement -> fprintf ppf "it's the type of a statement"
    | Optional_arg_default ->
      fprintf ppf "it's the type of an optional argument default"
    | Unboxed_tuple_element ->
      fprintf ppf "it's the type of unboxed tuple element"
    | Layout_poly_in_external ->
      fprintf ppf
        "it's the layout polymorphic type in an external declaration@ \
         ([@@layout_poly] forces all variables of layout 'any' to be@ \
         representable at call sites)"
    | Peek_or_poke ->
      fprintf ppf "it's the type being used for a peek or poke primitive"
    | Old_style_unboxed_type -> fprintf ppf "it's an [@@@@unboxed] type"
    | Array_element -> fprintf ppf "it's the type of an array element"
    | Idx_element ->
      fprintf ppf
        "it's the element type (the second type parameter) for a@ block index \
         (idx or mut_idx)"
    | Structure_item ->
      fprintf ppf "it's the type of something stored in a module"
    | Signature_item -> fprintf ppf "it's the type of something in a signature"

  let format_concrete_legacy_creation_reason ppf :
      History.concrete_legacy_creation_reason -> unit = function
    | Unannotated_type_parameter path ->
      fprintf ppf "it instantiates an unannotated type parameter of %a"
        !printtyp_path path
    | Wildcard -> fprintf ppf "it's a _ in the type"
    | Unification_var -> fprintf ppf "it's a fresh unification variable"

  let rec format_annotation_context : type l r.
      _ -> (l * r) History.annotation_context -> unit =
   fun ppf -> function
    | Type_declaration p ->
      fprintf ppf "the declaration of the type %a" !printtyp_path p
    | Type_parameter (path, var) ->
      let var_string = match var with None -> "_" | Some v -> "'" ^ v in
      fprintf ppf "@[%s@ in the declaration of the type@ %a@]" var_string
        !printtyp_path path
    | Newtype_declaration name ->
      fprintf ppf "the abstract type declaration for %s" name
    | Constructor_type_parameter (cstr, name) ->
      fprintf ppf "@[%s@ in the declaration of constructor@ %a@]" name
        !printtyp_path cstr
    | Existential_unpack name -> fprintf ppf "the existential variable %s" name
    | Univar name -> fprintf ppf "the universal variable %s" name
    | Type_variable name -> fprintf ppf "the type variable %s" name
    | Implicit_jkind name ->
      fprintf ppf "the implicit kind of type variables named %s" name
    | Type_wildcard loc ->
      fprintf ppf "the wildcard _ at %a" Location.print_loc_in_lowercase loc
    | Type_of_kind loc ->
      fprintf ppf "the type at %a" Location.print_loc_in_lowercase loc
    | With_error_message (_message, context) ->
      (* message gets printed in [format_flattened_history] so we ignore it here *)
      format_annotation_context ppf context

  let format_any_creation_reason ppf ~layout_or_kind :
      History.any_creation_reason -> _ = function
    | Missing_cmi p ->
      fprintf ppf "the .cmi file for %a is missing" !printtyp_path p
    | Initial_typedecl_env ->
      format_with_notify_js ppf
        "a dummy kind of any is used to check mutually recursive datatypes"
    | Wildcard -> format_with_notify_js ppf "there's a _ in the type"
    | Unification_var ->
      format_with_notify_js ppf "it's a fresh unification variable"
    | Dummy_jkind ->
      format_with_notify_js ppf
        "it's assigned a dummy kind that should have been overwritten"
    (* CR layouts: Improve output or remove this constructor ^^ *)
    | Type_expression_call ->
      format_with_notify_js ppf
        "there's a call to [type_expression] via the ocaml API"
    | Inside_of_Tarrow -> fprintf ppf "argument or result of a function type"
    | Array_type_argument ->
      fprintf ppf "it's the type argument to the array type"
    | Type_argument { parent_path; position; arity } ->
      fprintf ppf "the %stype argument of %a has %s any"
        (format_position ~arity position)
        !printtyp_path parent_path layout_or_kind

  let format_immediate_creation_reason ppf :
      History.immediate_creation_reason -> _ = function
    | Empty_record ->
      fprintf ppf "it's a record type containing all void elements"
    | Enumeration ->
      fprintf ppf
        "it's an enumeration variant type (all constructors are constant)"
    | Primitive id ->
      fprintf ppf "it is the primitive immediate type %s" (Ident.name id)
    | Immediate_polymorphic_variant ->
      fprintf ppf
        "it's an enumeration variant type (all constructors are constant)"

  let format_immediate_or_null_creation_reason ppf :
      History.immediate_or_null_creation_reason -> _ = function
    | Primitive id ->
      fprintf ppf "it is the primitive immediate_or_null type %s"
        (Ident.name id)

  let format_scannable_creation_reason ppf :
      History.scannable_creation_reason -> _ = function
    | Dummy_jkind ->
      format_with_notify_js ppf
        "it's assigned a dummy kind that should have been overwritten"

  let format_value_or_null_creation_reason ppf ~layout_or_kind :
      History.value_or_null_creation_reason -> _ = function
    | Primitive id ->
      fprintf ppf "it is the primitive value_or_null type %s" (Ident.name id)
    | Tuple_element -> fprintf ppf "it's the type of a tuple element"
    | Separability_check ->
      fprintf ppf "the check that a type is definitely not `float`"
    | Polymorphic_variant_field ->
      fprintf ppf "it's the type of the field of a polymorphic variant"
    | V1_safety_check ->
      fprintf ppf "it has to be value for the V1 safety check"
    | Probe -> format_with_notify_js ppf "it's a probe"
    | Captured_in_object ->
      fprintf ppf "it's the type of a variable captured in an object"
    | Let_rec_variable v ->
      fprintf ppf "it's the type of the recursive variable %s" (Ident.name v)
    | Type_argument { parent_path; position; arity } ->
      fprintf ppf "the %stype argument of %a has %s value_or_null"
        (format_position ~arity position)
        !printtyp_path parent_path layout_or_kind
    | Recmod_fun_arg ->
      fprintf ppf
        "it's the type of the first argument to a function in a recursive \
         module"
    | Array_comprehension_element ->
      fprintf ppf "it's the element type of array comprehension"
    | Array_comprehension_iterator_element ->
      fprintf ppf
        "it's the element type of an array that is iterated over in a \
         comprehension"

  let format_value_creation_reason ppf ~layout_or_kind :
      History.value_creation_reason -> _ = function
    | Class_let_binding ->
      fprintf ppf "it's the type of a let-bound variable in a class expression"
    | Object -> fprintf ppf "it's the type of an object"
    | Instance_variable -> fprintf ppf "it's the type of an instance variable"
    | Object_field -> fprintf ppf "it's the type of an object field"
    | Class_field -> fprintf ppf "it's the type of a class field"
    | Boxed_record -> fprintf ppf "it's a boxed record type"
    | Boxed_variant -> fprintf ppf "it's a boxed variant type"
    | Extensible_variant -> fprintf ppf "it's an extensible variant type"
    | Primitive id ->
      fprintf ppf "it is the primitive value type %s" (Ident.name id)
    | Type_argument { parent_path; position; arity } ->
      fprintf ppf "the %stype argument of %a has %s value"
        (format_position ~arity position)
        !printtyp_path parent_path layout_or_kind
    | Tuple -> fprintf ppf "it's a tuple type"
    | Row_variable -> format_with_notify_js ppf "it's a row variable"
    | Polymorphic_variant -> fprintf ppf "it's a polymorphic variant type"
    | Polymorphic_variant_too_big ->
      fprintf ppf
        "it's a polymorphic variant type that has more than %d entries"
        limit_for_mode_crossing_rows
    | Arrow -> fprintf ppf "it's a function type"
    | Tfield ->
      format_with_notify_js ppf
        "it's an internal Tfield type (you shouldn't see this)"
    | Tnil ->
      format_with_notify_js ppf
        "it's an internal Tnil type (you shouldn't see this)"
    | First_class_module -> fprintf ppf "it's a first-class module type"
    | Univar ->
      fprintf ppf "it is or unifies with an unannotated universal variable"
    | Default_type_jkind ->
      fprintf ppf "an abstract type has the value %s by default" layout_or_kind
    | Existential_type_variable ->
      fprintf ppf "it's an unannotated existential type variable"
    | Idx_base ->
      fprintf ppf
        "it's the base type (the first type parameter) for a@ block index (idx \
         or mut_idx)"
    | List_comprehension_iterator_element ->
      fprintf ppf
        "it's the element type of a list that is iterated over in a \
         comprehension"
    | Lazy_expression -> fprintf ppf "it's the type of a lazy expression"
    | Class_type_argument ->
      fprintf ppf "it's a type argument to a class constructor"
    | Class_term_argument ->
      fprintf ppf
        "it's the type of a term-level argument to a class constructor"
    | Debug_printer_argument ->
      format_with_notify_js ppf
        "it's the type of an argument to a debugger printer function"
    | Quotation_result -> fprintf ppf "it's the result type of a quotation"
    | Antiquotation_result -> fprintf ppf "it's the result type of splicing"
    | Tquote -> fprintf ppf "it's a staged type"
    | Tsplice -> fprintf ppf "it's a splice of a staged type"
    | Unknown s ->
      fprintf ppf
        "unknown @[(please alert the Jane Street@;\
         compilers team with this message: %s)@]"
        s
    | Array_type_kind ->
      fprintf ppf
        "it's the element type for an array operation with an opaque@ array \
         type"

  let format_product_creation_reason ppf : History.product_creation_reason -> _
      = function
    | Unboxed_tuple -> fprintf ppf "it is an unboxed tuple"
    | Unboxed_record -> fprintf ppf "it is an unboxed record"

  let format_creation_reason ppf ~layout_or_kind :
      History.creation_reason -> unit = function
    | Annotated (ctx, _) ->
      fprintf ppf "of the annotation on %a" format_annotation_context ctx
    | Missing_cmi p ->
      fprintf ppf "the .cmi file for %a is missing" !printtyp_path p
    | Any_creation any -> format_any_creation_reason ppf any ~layout_or_kind
    | Immediate_creation immediate ->
      format_immediate_creation_reason ppf immediate
    | Immediate_or_null_creation immediate ->
      format_immediate_or_null_creation_reason ppf immediate
    | Scannable_creation scannable ->
      format_scannable_creation_reason ppf scannable
    | Void_creation _ -> .
    | Value_or_null_creation value ->
      format_value_or_null_creation_reason ppf value ~layout_or_kind
    | Value_creation value ->
      format_value_creation_reason ppf ~layout_or_kind value
    | Product_creation product -> format_product_creation_reason ppf product
    | Concrete_creation concrete -> format_concrete_creation_reason ppf concrete
    | Concrete_legacy_creation concrete ->
      format_concrete_legacy_creation_reason ppf concrete
    | Primitive id -> fprintf ppf "it is the primitive type %s" (Ident.name id)
    | Unboxed_primitive id ->
      fprintf ppf "it is the unboxed version of the primitive type %s"
        (Ident.name id)
    | Imported ->
      fprintf ppf "of %s requirements from an imported definition"
        layout_or_kind
    | Imported_type_argument { parent_path; position; arity } ->
      fprintf ppf "the %stype argument of %a has this %s"
        (format_position ~arity position)
        !printtyp_path parent_path layout_or_kind
    | Generalized (id, loc) ->
      let format_id ppf = function
        | Some id -> fprintf ppf " of %s" (Ident.name id)
        | None -> ()
      in
      fprintf ppf "of the definition%a at %a" format_id id
        Location.print_loc_in_lowercase loc
    | Abbreviation -> fprintf ppf "it is the expansion of a type abbreviation"

  let format_interact_reason ppf : History.interact_reason -> _ = function
    | Gadt_equation name ->
      fprintf ppf "a GADT match refining the type %a" !printtyp_path name
    | Tyvar_refinement_intersection -> fprintf ppf "updating a type variable"
    | Subjkind -> fprintf ppf "subkind check"

  (* CR layouts: An older implementation of format_flattened_history existed
      which displays more information not limited to one layout and one creation_reason
      around commit 66a832d70bf61d9af3b0ec6f781dcf0a188b324d in main.

      Consider revisiting that if the current implementation becomes insufficient. *)

  let format_flattened_history ~intro ~layout_or_kind ppf t =
    let jkind_desc = Jkind_desc.get t.jkind in
    fprintf ppf "@[<v 2>%t" intro;
    (match t.history with
    | Creation reason ->
      if History.is_informative t
      then (
        fprintf ppf "@ because %a"
          (format_creation_reason ~layout_or_kind)
          reason;
        match reason, Desc.get_const jkind_desc with
        | Concrete_legacy_creation _, Some _ ->
          fprintf ppf ",@ chosen to have %s %a" layout_or_kind format t
        | _ -> ())
    | Interact _ ->
      Misc.fatal_error "Non-flat history in format_flattened_history");
    fprintf ppf ".";
    (match t.history with
    | Creation (Annotated (With_error_message (message, _), _)) ->
      fprintf ppf "@ @[%s@]" message
    | _ -> ());
    fprintf ppf "@]"

  (* this isn't really formatted for user consumption *)
  let format_history_tree ~intro ~layout_or_kind ppf t =
    let rec in_order ppf = function
      | Interact { reason; history1; history2; jkind1 = _; jkind2 = _ } ->
        fprintf ppf "@[<v 2>  %a@]@;%a@ @[<v 2>  %a@]" in_order history1
          format_interact_reason reason in_order history2
      | Creation c -> format_creation_reason ppf ~layout_or_kind c
    in
    fprintf ppf "@;%t has this %s history:@;@[<v 2>  %a@]" intro layout_or_kind
      in_order t.history

  let format_history ~intro ~layout_or_kind ppf t =
    if display_histories
    then
      if flattened_histories
      then format_flattened_history ~intro ~layout_or_kind ppf t
      else format_history_tree ~intro ~layout_or_kind ppf t
end

let format_history ~intro ppf t =
  Format_history.format_history ~intro ~layout_or_kind:"kind" ppf t

(******************************)
(* errors *)

module Violation = struct
  open Format
  include Jkind0.Violation

  let of_ ~context ?missing_cmi violation =
    (* Normalize for better printing *)
    let violation =
      match violation with
      | Not_a_subjkind (jkind1, jkind2, reasons) ->
        let jkind1 =
          normalize ~mode:Require_best ~context (disallow_right jkind1)
        in
        let jkind2 =
          normalize ~mode:Require_best ~context (disallow_right jkind2)
        in
        Not_a_subjkind (jkind1, jkind2, reasons)
      | No_intersection (jkind1, jkind2) ->
        let jkind1 =
          normalize ~mode:Require_best ~context (disallow_right jkind1)
        in
        (* jkind2 can't have with-bounds, by its type *)
        No_intersection (jkind1, jkind2)
    in
    { violation; missing_cmi }

  let is_missing_cmi viol = Option.is_some viol.missing_cmi

  type locale =
    | Mode
    | Layout

  let report_reason ppf violation =
    (* Print out per-axis information about why the error occurred. This only
       happens when modalities are printed because the errors are simple enough
       when there are no modalities that it makes the error unnecessarily noisy.
    *)
    match violation with
    | Not_a_subjkind (sub, super, reasons) -> (
      let disagreeing_axes =
        (* Collect all the axes that disagree into a set. If none disagree,
           then it is [None] *)
        List.fold_left
          (fun disagreeing_axes_so_far reason ->
            match (reason : Sub_failure_reason.t), disagreeing_axes_so_far with
            | Axis_disagreement (Pack axis), Some disagreeing_axes_so_far ->
              Some (Axis_set.add disagreeing_axes_so_far axis)
            | Axis_disagreement (Pack axis), None ->
              Some (Axis_set.singleton axis)
            | (Layout_disagreement | Constrain_ran_out_of_fuel), _ ->
              disagreeing_axes_so_far)
          None reasons
      in
      let has_modalities =
        let jkind_has_modalities jkind =
          List.exists
            (fun (_, type_info) ->
              let axes_ignored_by_modalities =
                With_bounds.Type_info.axes_ignored_by_modalities
                  ~mod_bounds:jkind.jkind.mod_bounds ~type_info
              in
              not (Axis_set.is_empty axes_ignored_by_modalities))
            (With_bounds.to_list jkind.jkind.with_bounds)
        in
        jkind_has_modalities sub || jkind_has_modalities super
      in
      match disagreeing_axes, has_modalities with
      | None, _ | _, false -> ()
      | Some disagreeing_axes, true ->
        (* CR: @\n is discouraged by the documentation, but @;@; seems to emit
           one newline and then one space rather than two newlines *)
        fprintf ppf "@\n@\nThe first mode-crosses less than the second along:";
        Axis_set.to_list disagreeing_axes
        |> List.iter (fun (Pack axis : Axis.packed) ->
            let pp_bound ppf jkind =
              let mod_bound = Mod_bounds.get ~axis jkind.mod_bounds in
              let with_bounds =
                match Per_axis.(le axis (max axis) mod_bound) with
                | true ->
                  (* If the mod_bound is max, then no with-bounds are
                        relevant *)
                  []
                | false ->
                  With_bounds.to_list jkind.with_bounds
                  |> List.filter_map
                       (fun
                         (ty, ({ relevant_axes } : With_bounds_type_info.t)) ->
                         match Axis_set.mem relevant_axes axis with
                         | true -> Some (!outcometrees_of_types [ty])
                         | false -> None)
                  |> List.flatten
              in
              let ojkind =
                List.fold_left
                  (fun acc with_bound ->
                    Outcometree.Ojkind_const_with (acc, with_bound, []))
                  (Outcometree.Ojkind_const_mod
                     ( None,
                       [Format.asprintf "%a" (Per_axis.print axis) mod_bound] ))
                  with_bounds
              in
              !Oprint.out_jkind_const ppf ojkind
            in
            fprintf ppf "@;  @[<hov 2>%s:@ %a ≰@ %a@]" (Axis.name axis) pp_bound
              sub.jkind pp_bound super.jkind))
    | No_intersection _ -> ()

  (* CR layouts-scannable: For now, this is special-cased to print out notes iff
     the layout error message prints containing [value non_pointer(64)] since
     [immediate(64)] is such a common jkind abbreviation. There is probably room
     to print out better notes (maybe by looking at the full jkinds?) *)
  let report_layout_notes ppf violation mismatch_type ~print_as_value_layout =
    match mismatch_type with
    | Mode -> ()
    | Layout ->
      let immediate_layout = Const.Builtin.immediate.jkind.layout in
      let immediate64_layout = Const.Builtin.immediate64.jkind.layout in
      let check_has_component component jkind =
        match Layout.get_const jkind.jkind.layout with
        | None -> false
        | Some const -> Layout.Const.has_component ~component const
      in
      let check_both_jkinds jk1 jk2 =
        let should_check_jk2 = not print_as_value_layout in
        ( check_has_component immediate_layout jk1
          || (should_check_jk2 && check_has_component immediate_layout jk2),
          check_has_component immediate64_layout jk1
          || (should_check_jk2 && check_has_component immediate64_layout jk2) )
      in
      let should_note_immediate, should_note_immediate64 =
        match violation with
        (* If we are printing the jkind on the right as a value layout, then
           we should not look at it to determine whether to emit a note *)
        (* Can't use an or-pattern since the jkinds have different allowances *)
        | Not_a_subjkind (jkind1, jkind2, _) -> check_both_jkinds jkind1 jkind2
        | No_intersection (jkind1, jkind2) -> check_both_jkinds jkind1 jkind2
      in
      if should_note_immediate
      then
        fprintf ppf "@;@[Note: The layout of immediate is value non_pointer.@]";
      if should_note_immediate64
      then
        fprintf ppf
          "@;@[Note: The layout of immediate64 is value non_pointer64.@]"

  let report_fuel ppf violation =
    let report_fuel_for_type which =
      fprintf ppf
        "@;\
         @[Note: I gave up trying to find the simplest kind for the %s,@,\
         as it is very large or deeply recursive.@]"
        which
    in
    let first_ran_out, second_ran_out =
      match violation with
      | Not_a_subjkind (k1, k2, _) ->
        k1.ran_out_of_fuel_during_normalize, k2.ran_out_of_fuel_during_normalize
      | No_intersection (k1, k2) ->
        k1.ran_out_of_fuel_during_normalize, k2.ran_out_of_fuel_during_normalize
    in
    if first_ran_out then report_fuel_for_type "first";
    if second_ran_out then report_fuel_for_type "second"

  (* CR layouts-scannable: When there is a layout violation and both are
     value (scannable) layouts, a more useful error should be reported.
     Specifically, the first mismatched axis should be reported as a reason,
     like "because it is not non_pointer" for a value vs immediate error. *)
  (* CR layouts-scannable: Also, better error messages should be reported for
     products! For instance, an error message blaming an arity difference, or
     an error message that drills down into two products to find the first
     conflicing component. Note reporting should be adjusted appropriately. *)
  let report_general ~level preamble pp_former former ppf t =
    (* Sometimes, when reporting a layout conflict, the scannable axes of
       the expected layout should not be shown since the information is
       unnecessary. For now, this condition is when the layout on the left is
       known to not be scannable, but the layout on the right is scannable
       (so printing as "a value layout" is sensible). *)
    let mismatch_type, print_as_value_layout =
      match t.violation with
      | Not_a_subjkind (k1, k2, _) ->
        let l1, l2 = k1.jkind.layout, k2.jkind.layout in
        if Sub_result.is_le (Layout.sub ~level l1 l2)
        then Mode, false
        else
          ( Layout,
            (not (Layout.is_scannable_or_var l1))
            && Layout.is_scannable_or_var l2 )
      | No_intersection (k1, k2) ->
        ( Layout,
          (not (Layout.is_scannable_or_var k1.jkind.layout))
          && Layout.is_scannable_or_var k2.jkind.layout )
    in
    let layout_or_kind =
      match mismatch_type with Mode -> "kind" | Layout -> "layout"
    in
    let rec has_sort_var : Sort.Flat.t Layout.t -> bool = function
      | Sort (Var _, _) -> true
      | Product layouts -> List.exists has_sort_var layouts
      | Sort (Base _, _) | Any _ -> false
    in
    let indent = pp_print_custom_break ~fits:("", 0, "") ~breaks:("", 2, "") in
    let format_layout_or_kind ppf jkind =
      match mismatch_type with
      | Mode -> fprintf ppf "%t%a" indent format jkind
      | Layout -> fprintf ppf "%t%a" indent Layout.format jkind.jkind.layout
    in
    let subjkind_format verb k2 =
      if has_sort_var (get k2).layout
      then dprintf "%s representable" verb
      else if print_as_value_layout
      then
        (* avoid printing "a sublayout of a value layout" *)
        dprintf "%s@ a value layout" verb
      else
        dprintf "%s a sub%s of@ %a" verb layout_or_kind format_layout_or_kind k2
    in
    let Pack_jkind k1, Pack_jkind k2, fmt_k1, fmt_k2, missing_cmi_option =
      match t with
      | { violation = Not_a_subjkind (k1, k2, _); missing_cmi } -> (
        let missing_cmi =
          match missing_cmi with
          | None -> (
            match k1.history with
            | Creation (Missing_cmi p) -> Some p
            | Creation (Any_creation (Missing_cmi p)) -> Some p
            | _ -> None)
          | Some _ -> missing_cmi
        in
        match missing_cmi with
        | None ->
          ( Pack_jkind k1,
            Pack_jkind k2,
            dprintf "%s@ %a" layout_or_kind format_layout_or_kind k1,
            subjkind_format "is not" k2,
            None )
        | Some p ->
          ( Pack_jkind k1,
            Pack_jkind k2,
            dprintf "an unknown %s" layout_or_kind,
            subjkind_format "might not be" k2,
            Some p ))
      | { violation = No_intersection (k1, k2); missing_cmi } ->
        assert (Option.is_none missing_cmi);
        let fmt_k2 =
          if print_as_value_layout
          then dprintf "does not overlap with@ %a" format_layout_or_kind k2
          else dprintf "is not@ a value layout"
        in
        ( Pack_jkind k1,
          Pack_jkind k2,
          dprintf "%s@ %a" layout_or_kind format_layout_or_kind k1,
          fmt_k2,
          None )
    in
    if display_histories
    then
      let connective =
        if has_sort_var (get k2).layout
        then dprintf "be representable"
        else if print_as_value_layout
        then dprintf "be@ a value layout"
        else
          match t.violation with
          | Not_a_subjkind _ ->
            dprintf "be a sub%s of@ %a" layout_or_kind format_layout_or_kind k2
          | No_intersection _ ->
            dprintf "overlap with@ %a" format_layout_or_kind k2
      in
      fprintf ppf "@[<v>%a@;%a@]"
        (Format_history.format_history
           ~intro:
             (dprintf "@[<hov 2>The %s of %a is@ %a@]" layout_or_kind pp_former
                former format_layout_or_kind k1)
           ~layout_or_kind)
        k1
        (Format_history.format_history
           ~intro:
             (dprintf "@[<hov 2>But the %s of %a must %t@]" layout_or_kind
                pp_former former connective)
           ~layout_or_kind)
        k2
    else
      fprintf ppf "@[<hov 2>%s%a has %t,@ which %t.@]" preamble pp_former former
        fmt_k1 fmt_k2;
    report_missing_cmi ppf missing_cmi_option;
    report_reason ppf t.violation;
    (* otherwise, we get notes for layout abbreviations that get omitted. *)
    report_layout_notes ppf t.violation mismatch_type ~print_as_value_layout;
    report_fuel ppf t.violation

  let pp_t ppf x = fprintf ppf "%t" x

  let report_with_offender ~offender ~level =
    report_general ~level "" pp_t offender

  let () = Env.report_jkind_violation_with_offender := report_with_offender

  let report_with_offender_sort ~offender ~level =
    report_general ~level "A representable layout was expected, but " pp_t
      offender

  let report_with_name ~name ~level =
    report_general ~level "" pp_print_string name
end

(******************************)
(* relations *)

let equate_or_equal ~allow_mutation
    { jkind = jkind1;
      annotation = _;
      history = _;
      has_warned = _;
      ran_out_of_fuel_during_normalize = _;
      quality = _
    }
    { jkind = jkind2;
      annotation = _;
      history = _;
      has_warned = _;
      ran_out_of_fuel_during_normalize = _;
      quality = _
    } =
  Jkind_desc.equate_or_equal ~allow_mutation jkind1 jkind2

(* CR layouts: Switch this back to ~allow_mutation:false. Internal ticket 5099. *)
let equal ~level t1 t2 = equate_or_equal ~allow_mutation:true ~level t1 t2

let equate ~level t1 t2 = equate_or_equal ~allow_mutation:true ~level t1 t2

(* Not all jkind history reasons are created equal. Some are more helpful than
   others.  This function encodes that information.

    The reason with higher score should get preserved when combined with one of
    lower score. *)
let score_reason = function
  (* error_message annotated by the user should always take priority *)
  | Creation (Annotated (With_error_message _, _)) -> 1
  (* Concrete creation is quite vague, prefer more specific reasons *)
  | Creation (Concrete_creation _ | Concrete_legacy_creation _) -> -1
  | _ -> 0

let combine_histories ~type_equal ~context ~level reason (Pack_jkind k1)
    (Pack_jkind k2) =
  if flattened_histories
  then
    let choose_higher_scored_history history_a history_b =
      if score_reason history_a >= score_reason history_b
      then history_a
      else history_b
    in
    let choose_subjkind_history k_a history_a roofdn_a k_b history_b =
      match
        Jkind_desc.sub ~level ~type_equal
          ~sub_previously_ran_out_of_fuel:roofdn_a ~context k_a k_b
      with
      | Less -> history_a
      | Not_le _ ->
        (* CR layouts: this will be wrong if we ever have a non-trivial meet in
           the kind lattice -- which is now! So this is actually wrong. *)
        history_b
      | Equal -> choose_higher_scored_history history_a history_b
    in
    match Layout_and_axes.(try_allow_l k1.jkind, try_allow_r k2.jkind) with
    | Some k1_l, Some k2_r ->
      choose_subjkind_history k1_l k1.history
        k1.ran_out_of_fuel_during_normalize k2_r k2.history
    | _ -> (
      match Layout_and_axes.(try_allow_r k1.jkind, try_allow_l k2.jkind) with
      | Some k1_r, Some k2_l ->
        choose_subjkind_history k2_l k2.history
          k2.ran_out_of_fuel_during_normalize k1_r k1.history
      | _ -> choose_higher_scored_history k1.history k2.history)
  else
    Interact
      { reason;
        jkind1 = Pack_jkind_desc k1.jkind;
        history1 = k1.history;
        jkind2 = Pack_jkind_desc k2.jkind;
        history2 = k2.history
      }

let has_intersection ~level t1 t2 =
  (* Need to check only the layouts: all the axes have bottom elements. *)
  Option.is_some (Layout.intersection ~level t1.jkind.layout t2.jkind.layout)

let intersection_or_error ~type_equal ~context ~reason ~level t1 t2 =
  match Jkind_desc.intersection ~level t1.jkind t2.jkind with
  | None -> Error (Violation.of_ ~context (No_intersection (t1, t2)))
  | Some jkind ->
    Ok
      { jkind;
        annotation = None;
        history =
          combine_histories ~type_equal ~context ~level reason (Pack_jkind t1)
            (Pack_jkind t2);
        has_warned = t1.has_warned || t2.has_warned;
        ran_out_of_fuel_during_normalize =
          t1.ran_out_of_fuel_during_normalize
          || t2.ran_out_of_fuel_during_normalize;
        quality =
          Not_best (* As required by the fact that this is a [jkind_r] *)
      }

let round_up (type l r) ~context (t : (allowed * r) jkind) : (l * allowed) jkind
    =
  let normalized = normalize ~mode:Ignore_best ~context (t |> disallow_right) in
  { t with
    jkind = { normalized.jkind with with_bounds = No_with_bounds };
    quality = Not_best (* As required by the fact that this is a [jkind_r] *)
  }

(* this is hammered on; it must be fast! *)
let check_sub ~context sub super = Jkind_desc.sub ~context sub.jkind super.jkind

let sub_with_reason ~type_equal ~context ~level sub super =
  Sub_result.require_le
    (check_sub ~type_equal
       ~sub_previously_ran_out_of_fuel:sub.ran_out_of_fuel_during_normalize
       ~context ~level sub super)

let sub ~type_equal ~context ~level sub super =
  Result.is_ok (sub_with_reason ~type_equal ~context ~level sub super)

type sub_or_intersect =
  | Sub
  | Disjoint of Violation.Sub_failure_reason.t Nonempty_list.t
  | Has_intersection of Violation.Sub_failure_reason.t Nonempty_list.t

let sub_or_intersect ~type_equal ~context ~level t1 t2 =
  match sub_with_reason ~type_equal ~context ~level t1 t2 with
  | Ok () -> Sub
  | Error reason ->
    if has_intersection ~level t1 t2
    then Has_intersection reason
    else Disjoint reason

let sub_or_error ~type_equal ~context ~level t1 t2 =
  match sub_or_intersect ~type_equal ~context ~level t1 t2 with
  | Sub -> Ok ()
  | Disjoint reason | Has_intersection reason ->
    Error
      (Violation.of_ ~context
         (Not_a_subjkind (t1, t2, Nonempty_list.to_list reason)))

let sub_jkind_l ~type_equal ~context ~level ?(allow_any_crossing = false) sub
    super =
  (* This function implements the "SUB" judgement from kind-inference.md. *)
  let open Misc.Stdlib.Monad.Result.Syntax in
  let require_le sub_result =
    Sub_result.require_le sub_result
    |> Result.map_error (fun reasons ->
        (* When we report an error, we want to show the best-normalized
              version of sub, but the original super. When this check fails, it
              is usually the case that the super was written by the user and the
              sub was inferred. Thus, we should display the user-written jkind,
              but simplify the inferred one, since the inferred one is probably
              overly complex. *)
        (* CR layouts v2.8: It would be useful report to the user why this
              violation occurred, specifically which axes the violation is
              along. Internal ticket 5100. *)
        let best_sub = normalize ~mode:Require_best ~context sub in
        Violation.of_ ~context
          (Not_a_subjkind (best_sub, super, Nonempty_list.to_list reasons)))
  in
  let* () =
    (* Validate layouts *)
    require_le (Layout.sub ~level sub.jkind.layout super.jkind.layout)
  in
  match allow_any_crossing with
  | true -> Ok ()
  | false ->
    let best_super =
      (* MB_EXPAND_R *)
      normalize ~mode:Require_best ~context super
    in
    let right_bounds =
      With_bounds.to_best_eff_map best_super.jkind.with_bounds
    in
    let axes_max_on_right =
      (* If the upper_bound is max on the right, then that axis is irrelevant -
         the left will always satisfy the right along that axis. This is an
         optimization, not necessary for correctness *)
      Mod_bounds.get_max_axes best_super.jkind.mod_bounds
    in
    let right_bounds_seq = right_bounds |> With_bounds_types.to_seq in
    let ( ({ layout = _;
             mod_bounds = sub_upper_bounds;
             with_bounds = No_with_bounds
           } :
            (_ * allowed) jkind_desc),
          _ ) =
      (* MB_EXPAND_L *)
      (* Here we progressively expand types on the left.

         Every time we see a type [ty] on the left, we first look to see if [ty]
         occurs on the right. If it does, then we can skip* [ty]. There is an *
         on skip because we can actually only skip on a per-axis basis - if [ty]
         is relevant only along the portability axis on the right, then [ty] is
         no longer relevant to portability on the left, but it is still relevant
         to all other axes. So really, we subtract the axes that are relevant to
         the right from the axes that are relevant to the left.  We can also
         skip [ty] on any axes that are max on the right since anything is <=
         max. Hence, we can also subtract [axes_max_on_right].

         After finding which axes [ty] is relevant along, we lookup [ty]'s jkind
         and join it with the [mod_bounds] along the relevant axes. *)
      (* [Jkind_desc.map_normalize] handles the stepping, jkind lookups, and
         joining.  [map_type_info] handles looking for [ty] on the right and
         removing irrelevant axes. *)
      Layout_and_axes.normalize sub.jkind ~skip_axes:axes_max_on_right
        ~previously_ran_out_of_fuel:sub.ran_out_of_fuel_during_normalize
        ~context ~mode:Ignore_best
        ~map_type_info:(fun ty { relevant_axes = left_relevant_axes } ->
          let right_relevant_axes =
            (* Look for [ty] on the right. There may be multiple occurrences of
               it on the right; if so, we union together the relevant axes. *)
            right_bounds_seq
            (* CR layouts v2.8: maybe it's worth memoizing using a best-effort
               type map? Internal ticket 5086. *)
            |> Seq.fold_left
                 (fun acc (ty2, ti) ->
                   match type_equal ty ty2 with
                   | true ->
                     Axis_set.union acc ti.With_bounds_type_info.relevant_axes
                   | false -> acc)
                 Axis_set.empty
          in
          (* MB_WITH : drop types from the left that appear on the right *)
          { relevant_axes = Axis_set.diff left_relevant_axes right_relevant_axes
          })
    in
    let* () =
      (* MB_MODE : verify that the remaining upper_bounds from sub are <=
         super's bounds *)
      let super_lower_bounds = best_super.jkind.mod_bounds in
      require_le (Mod_bounds.less_or_equal sub_upper_bounds super_lower_bounds)
    in
    Ok ()

let is_void_defaulting = function
  | { jkind = { layout = Sort (s, _); _ }; _ } -> Sort.is_void_defaulting s
  | _ -> false

let is_max (t : (_ * allowed) jkind) =
  match t with
  (* This doesn't do any mutation because mutating a sort variable can't make it
     any, and modal upper bounds are constant. *)
  | { jkind = { layout = Any sa; mod_bounds; with_bounds = No_with_bounds }; _ }
    ->
    Scannable_axes.(equal sa max) && Mod_bounds.is_max mod_bounds
  | { jkind = { layout = _; mod_bounds = _; with_bounds = No_with_bounds }; _ }
    ->
    false

let mod_bounds_are_max
    ({ jkind = { layout = _; mod_bounds; with_bounds = No_with_bounds }; _ } :
      (_ * allowed) jkind) =
  Mod_bounds.is_max mod_bounds

let has_layout_any jkind =
  match jkind.jkind.layout with Any _ -> true | _ -> false

let is_value_for_printing ~ignore_null { jkind; _ } =
  match Desc.get_const (Jkind_desc.get jkind) with
  | None -> false
  | Some const ->
    let value = Const.Builtin.value.jkind in
    let values = [value] in
    let values =
      if ignore_null
      then Const.Builtin.value_or_null.jkind :: values
      else values
    in
    List.exists (fun v -> Const.shallow_no_with_bounds_and_equal const v) values

(*********************************)
(* debugging *)

module Debug_printers = struct
  open Format

  let concrete_creation_reason ppf : History.concrete_creation_reason -> unit =
    function
    | Match -> fprintf ppf "Match"
    | Constructor_declaration idx ->
      fprintf ppf "Constructor_declaration %d" idx
    | Label_declaration lbl ->
      fprintf ppf "Label_declaration %a" Ident.print lbl
    | Record_projection -> fprintf ppf "Record_projection"
    | Record_assignment -> fprintf ppf "Record_assignment"
    | Record_functional_update -> fprintf ppf "Record_functional_update"
    | Let_binding -> fprintf ppf "Let_binding"
    | Function_argument -> fprintf ppf "Function_argument"
    | Function_result -> fprintf ppf "Function_result"
    | Structure_item_expression -> fprintf ppf "Structure_item_expression"
    | External_argument -> fprintf ppf "External_argument"
    | External_result -> fprintf ppf "External_result"
    | Statement -> fprintf ppf "Statement"
    | Optional_arg_default -> fprintf ppf "Optional_arg_default"
    | Layout_poly_in_external -> fprintf ppf "Layout_poly_in_external"
    | Unboxed_tuple_element -> fprintf ppf "Unboxed_tuple_element"
    | Peek_or_poke -> fprintf ppf "Peek_or_poke"
    | Old_style_unboxed_type -> fprintf ppf "Old_style_unboxed_type"
    | Array_element -> fprintf ppf "Array_element"
    | Idx_element -> fprintf ppf "Idx_element"
    | Structure_item -> fprintf ppf "Structure_item"
    | Signature_item -> fprintf ppf "Signature_item"

  let concrete_legacy_creation_reason ppf :
      History.concrete_legacy_creation_reason -> unit = function
    | Unannotated_type_parameter path ->
      fprintf ppf "Unannotated_type_parameter %a" !printtyp_path path
    | Wildcard -> fprintf ppf "Wildcard"
    | Unification_var -> fprintf ppf "Unification_var"

  let rec annotation_context : type l r.
      _ -> (l * r) History.annotation_context -> unit =
   fun ppf -> function
    | Type_declaration p -> fprintf ppf "Type_declaration %a" Path.print p
    | Type_parameter (p, var) ->
      fprintf ppf "Type_parameter (%a, %a)" Path.print p
        (Misc.Stdlib.Option.print Misc.Stdlib.String.print)
        var
    | Newtype_declaration name -> fprintf ppf "Newtype_declaration %s" name
    | Constructor_type_parameter (cstr, name) ->
      fprintf ppf "Constructor_type_parameter (%a, %S)" Path.print cstr name
    | Existential_unpack name -> fprintf ppf "Existential_unpack %s" name
    | Univar name -> fprintf ppf "Univar %S" name
    | Type_variable name -> fprintf ppf "Type_variable %S" name
    | Implicit_jkind name -> fprintf ppf "Implicit_jkind %S" name
    | Type_wildcard loc ->
      fprintf ppf "Type_wildcard (%a)" Location.print_loc loc
    | Type_of_kind loc -> fprintf ppf "Type_of_kind (%a)" Location.print_loc loc
    | With_error_message (message, context) ->
      fprintf ppf "With_error_message (%s, %a)" message annotation_context
        context

  let any_creation_reason ppf : History.any_creation_reason -> unit = function
    | Missing_cmi p -> fprintf ppf "Missing_cmi %a" Path.print p
    | Initial_typedecl_env -> fprintf ppf "Initial_typedecl_env"
    | Dummy_jkind -> fprintf ppf "Dummy_jkind"
    | Wildcard -> fprintf ppf "Wildcard"
    | Unification_var -> fprintf ppf "Unification_var"
    | Type_expression_call -> fprintf ppf "Type_expression_call"
    | Inside_of_Tarrow -> fprintf ppf "Inside_of_Tarrow"
    | Array_type_argument -> fprintf ppf "Array_type_argument"
    | Type_argument { parent_path; position; arity } ->
      fprintf ppf "Type_argument (pos %d, arity %d) of %a" position arity
        !printtyp_path parent_path

  let immediate_creation_reason ppf : History.immediate_creation_reason -> _ =
    function
    | Empty_record -> fprintf ppf "Empty_record"
    | Enumeration -> fprintf ppf "Enumeration"
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
    | Immediate_polymorphic_variant ->
      fprintf ppf "Immediate_polymorphic_variant"

  let immediate_or_null_creation_reason ppf :
      History.immediate_or_null_creation_reason -> _ = function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)

  let scannable_creation_reason ppf : History.scannable_creation_reason -> _ =
    function
    | Dummy_jkind -> fprintf ppf "Dummy_jkind"

  let value_or_null_creation_reason ppf :
      History.value_or_null_creation_reason -> _ = function
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
    | Tuple_element -> fprintf ppf "Tuple_element"
    | Separability_check -> fprintf ppf "Separability_check"
    | Polymorphic_variant_field -> fprintf ppf "Polymorphic_variant_field"
    | V1_safety_check -> fprintf ppf "V1_safety_check"
    | Probe -> fprintf ppf "Probe"
    | Captured_in_object -> fprintf ppf "Captured_in_object"
    | Let_rec_variable v -> fprintf ppf "Let_rec_variable %a" Ident.print v
    | Type_argument { parent_path; position; arity } ->
      fprintf ppf "Type_argument (pos %d, arity %d) of %a" position arity
        !printtyp_path parent_path
    | Recmod_fun_arg -> fprintf ppf "Recmod_fun_arg"
    | Array_comprehension_element -> fprintf ppf "Array_comprehension_element"
    | Array_comprehension_iterator_element ->
      fprintf ppf "Array_comprehension_iterator_element"

  let value_creation_reason ppf : History.value_creation_reason -> _ = function
    | Class_let_binding -> fprintf ppf "Class_let_binding"
    | Object -> fprintf ppf "Object"
    | Instance_variable -> fprintf ppf "Instance_variable"
    | Object_field -> fprintf ppf "Object_field"
    | Class_field -> fprintf ppf "Class_field"
    | Boxed_record -> fprintf ppf "Boxed_record"
    | Boxed_variant -> fprintf ppf "Boxed_variant"
    | Extensible_variant -> fprintf ppf "Extensible_variant"
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.unique_name id)
    | Type_argument { parent_path; position; arity } ->
      fprintf ppf "Type_argument (pos %d, arity %d) of %a" position arity
        !printtyp_path parent_path
    | Tuple -> fprintf ppf "Tuple"
    | Row_variable -> fprintf ppf "Row_variable"
    | Polymorphic_variant -> fprintf ppf "Polymorphic_variant"
    | Polymorphic_variant_too_big -> fprintf ppf "Polymorphic_variant_too_big"
    | Arrow -> fprintf ppf "Arrow"
    | Tfield -> fprintf ppf "Tfield"
    | Tnil -> fprintf ppf "Tnil"
    | First_class_module -> fprintf ppf "First_class_module"
    | Univar -> fprintf ppf "Univar"
    | Default_type_jkind -> fprintf ppf "Default_type_jkind"
    | Existential_type_variable -> fprintf ppf "Existential_type_variable"
    | Idx_base -> fprintf ppf "Idx_base"
    | List_comprehension_iterator_element ->
      fprintf ppf "List_comprehension_iterator_element"
    | Lazy_expression -> fprintf ppf "Lazy_expression"
    | Class_type_argument -> fprintf ppf "Class_type_argument"
    | Class_term_argument -> fprintf ppf "Class_term_argument"
    | Debug_printer_argument -> fprintf ppf "Debug_printer_argument"
    | Quotation_result -> fprintf ppf "Quotation_result"
    | Antiquotation_result -> fprintf ppf "Antiquotation_result"
    | Tquote -> fprintf ppf "Tquote"
    | Tsplice -> fprintf ppf "Tsplice"
    | Unknown s -> fprintf ppf "Unknown %s" s
    | Array_type_kind -> fprintf ppf "Array_type_kind"

  let product_creation_reason ppf : History.product_creation_reason -> _ =
    function
    | Unboxed_tuple -> fprintf ppf "Unboxed_tuple"
    | Unboxed_record -> fprintf ppf "Unboxed_record"

  let creation_reason ppf : History.creation_reason -> unit = function
    | Annotated (ctx, loc) ->
      fprintf ppf "Annotated (%a,%a)" annotation_context ctx Location.print_loc
        loc
    | Missing_cmi p -> fprintf ppf "Missing_cmi %a" !printtyp_path p
    | Any_creation any -> fprintf ppf "Any_creation %a" any_creation_reason any
    | Immediate_creation immediate ->
      fprintf ppf "Immediate_creation %a" immediate_creation_reason immediate
    | Immediate_or_null_creation immediate ->
      fprintf ppf "Immediate_or_null_creation %a"
        immediate_or_null_creation_reason immediate
    | Scannable_creation scannable ->
      fprintf ppf "Scannable_creation %a" scannable_creation_reason scannable
    | Value_or_null_creation value ->
      fprintf ppf "Value_or_null_creation %a" value_or_null_creation_reason
        value
    | Value_creation value ->
      fprintf ppf "Value_creation %a" value_creation_reason value
    | Void_creation _ -> .
    | Product_creation product ->
      fprintf ppf "Product_creation %a" product_creation_reason product
    | Concrete_creation concrete ->
      fprintf ppf "Concrete_creation %a" concrete_creation_reason concrete
    | Concrete_legacy_creation concrete ->
      fprintf ppf "Concrete_legacy_creation %a" concrete_legacy_creation_reason
        concrete
    | Primitive id -> fprintf ppf "Primitive %s" (Ident.name id)
    | Unboxed_primitive id -> fprintf ppf "Unboxed_primitive %s" (Ident.name id)
    | Imported -> fprintf ppf "Imported"
    | Imported_type_argument { parent_path; position; arity } ->
      fprintf ppf "Imported_type_argument (pos %d, arity %d) of %a" position
        arity !printtyp_path parent_path
    | Generalized (id, loc) ->
      fprintf ppf "Generalized (%s, %a)"
        (match id with Some id -> Ident.unique_name id | None -> "")
        Location.print_loc loc
    | Abbreviation -> fprintf ppf "Abbreviation"

  let interact_reason ppf : History.interact_reason -> _ = function
    | Gadt_equation p -> fprintf ppf "Gadt_equation %a" Path.print p
    | Tyvar_refinement_intersection ->
      fprintf ppf "Tyvar_refinement_intersection"
    | Subjkind -> fprintf ppf "Subjkind"

  let rec history ppf =
    let jkind_desc = Jkind_desc.Debug_printers.t in
    function
    | Interact
        { reason;
          jkind1 = Pack_jkind_desc jkind1;
          history1;
          jkind2 = Pack_jkind_desc jkind2;
          history2
        } ->
      fprintf ppf
        "Interact {@[reason = %a;@ jkind1 = %a;@ history1 = %a;@ jkind2 = %a;@ \
         history2 = %a}@]"
        interact_reason reason jkind_desc jkind1 history history1 jkind_desc
        jkind2 history history2
    | Creation c -> fprintf ppf "Creation (%a)" creation_reason c

  let t (type l r) ppf
      ({ jkind;
         annotation = a;
         history = h;
         has_warned = _;
         ran_out_of_fuel_during_normalize = roofdn;
         quality = q
       } :
        (l * r) jkind) : unit =
    fprintf ppf
      "@[<v 2>{ jkind = %a@,\
       ; annotation = %a@,\
       ; history = %a@,\
       ; ran_out_of_fuel_during_normalize = %a@,\
       ; quality = %s@,\
      \ }@]"
      Jkind_desc.Debug_printers.t jkind
      (pp_print_option Pprintast.jkind_annotation)
      a history h pp_print_bool roofdn
      (match q with Best -> "Best" | Not_best -> "Not_best")

  module Const = struct
    let t ppf ({ layout; mod_bounds; with_bounds } : _ Const.t) =
      fprintf ppf
        "@[<v 2>{ layout = %a@,; mod_bounds = %a@,; with_bounds = %a@, }@]"
        Layout.Const.Debug_printers.t layout Mod_bounds.debug_print mod_bounds
        With_bounds.debug_print with_bounds
  end
end

(*** formatting user errors ***)
let report_error ~loc : Error.t -> _ = function
  | Unknown_jkind jkind ->
    Location.errorf ~loc
      (* CR layouts v2.9: use the context to produce a better error message.
         When RAE tried this, some types got printed like [t/2], but the
         [/2] shouldn't be there. Investigate and fix. *)
      "@[<v>Unknown layout %a@]"
      Pprintast.jkind_annotation jkind
  | Unknown_kind_modifier saxis ->
    Location.errorf ~loc "@[<v>Unknown kind modifier %s@]" saxis
  | Multiple_jkinds { from_annotation; from_attribute } ->
    Location.errorf ~loc
      "@[<v>A type declaration's layout can be given at most once.@;\
       This declaration has an layout annotation (%a) and a layout attribute \
       ([@@@@%s]).@]"
      Pprintast.jkind_annotation from_annotation
      (Builtin_attributes.jkind_attribute_to_string from_attribute.txt)
  | Insufficient_level { jkind; required_layouts_level } -> (
    let hint ppf =
      Format.fprintf ppf "You must enable -extension %s to use this feature."
        (Language_extension.to_command_line_string Layouts
           required_layouts_level)
    in
    match Language_extension.is_enabled Layouts with
    | false ->
      Location.errorf ~loc
        "@[<v>The appropriate layouts extension is not enabled.@;%t@]" hint
    | true ->
      Location.errorf ~loc
        (* CR layouts errors: use the context to produce a better error message.
           When RAE tried this, some types got printed like [t/2], but the
           [/2] shouldn't be there. Investigate and fix. *)
        "@[<v>Layout %a is more experimental than allowed by the enabled \
         layouts extension.@;\
         %t@]"
        Pprintast.jkind_annotation jkind hint)
  | Unimplemented_syntax ->
    Location.errorf ~loc "@[<v>Unimplemented kind syntax@]"
  | With_on_right ->
    Location.errorf ~loc "'with' syntax is not allowed on a right mode."

let () =
  Location.register_error_of_exn (function
    | Error.User_error (loc, err) -> Some (report_error ~loc err)
    | _ -> None)

(* See mli *)
type temp_cycle_check_subst = Subst.t

type temp_cycle_check_env = Env.t

module type temp_cycle_check_datarepr = module type of Datarepr

module type temp_cycle_check_predef = module type of Predef
