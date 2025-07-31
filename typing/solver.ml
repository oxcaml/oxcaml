(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Stephen Dolan, Jane Street, London                   *)
(*                    Zesen Qian, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Allowance
open Solver_intf

module Magic_equal (X : Equal) :
  Equal with type ('a, 'b, 'c) t = ('a, 'b, 'c) X.t = struct
  type ('a, 'b, 'd) t = ('a, 'b, 'd) X.t

  let equal :
      type a0 a1 b l0 l1 r0 r1.
      (a0, b, l0 * r0) t -> (a1, b, l1 * r1) t -> (a0, a1) Misc.eq option =
   fun x0 x1 ->
    if Obj.repr x0 = Obj.repr x1 then Some (Obj.magic Misc.Refl) else None
end
[@@inline]

module Solver_mono (H : Hint) (C : Lattices_mono) = struct
  type ('a, 'd) hint =
    | Apply : 'd H.morph * ('b, 'a, 'd) C.morph * ('b, 'd) hint -> ('a, 'd) hint
    | Const : H.const -> ('a, 'l * 'r) hint
    | Branch :
        'a * ('a, 'l * 'r) hint * 'a * ('a, 'l * 'r) hint
        -> ('a, 'l * 'r) hint
    | Nil : ('a, 'l * 'r) hint
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  type 'a error =
    { left : 'a;
      left_hint : ('a, left_only) hint;
      right : 'a;
      right_hint : ('a, right_only) hint
    }

  (** Provides a more general form of hints and morphism hints that are easier for
    the solver to construct, and allows for composition and deferred composition.

    It is used internally in the solver but will be converted to the normal kind variety
    of hints before returning it. *)
  module Comp_hint = struct
    module Morph_hint = struct
      (** A value of type [('a, 'b, 'd) morph_hint] is a hint for a morphism
      from ['a] to ['b] with allowance ['d] *)
      type ('a, 'b, 'd) t =
        | Base : 'd H.morph * ('a, 'b, 'd) C.morph -> ('a, 'b, 'd) t
        | None : 'a C.obj -> ('a, 'b, 'l * 'r) t
        | Compose :
            ('b, 'c, 'l * 'r) t * ('a, 'b, 'l * 'r) t
            -> ('a, 'c, 'l * 'r) t
        | Hole_compose : ('a, 'b, 'l * 'r) C.morph -> ('a, 'b, 'l * 'r) t
        constraint 'd = _ * _
      [@@ocaml.warning "-62"]

      let left_adjoint =
        let rec aux :
            type a b l.
            b C.obj ->
            (a, b, l * allowed) t ->
            a C.obj * (b, a, allowed * disallowed) t =
         fun b_obj -> function
          | Base (small_morph_hint, morph) ->
            ( C.src b_obj morph,
              Base (H.left_adjoint small_morph_hint, C.left_adjoint b_obj morph)
            )
          | None a_obj -> a_obj, None b_obj
          | Compose (f_morph_hint, g_morph_hint) ->
            let mid, f_morph_hint_adj = aux b_obj f_morph_hint in
            let src, g_morph_hint_adj = aux mid g_morph_hint in
            src, Compose (g_morph_hint_adj, f_morph_hint_adj)
          | Hole_compose morph ->
            C.src b_obj morph, Hole_compose (C.left_adjoint b_obj morph)
        in
        fun b_obj h -> snd (aux b_obj h)

      let right_adjoint =
        let rec aux :
            type a b r.
            b C.obj ->
            (a, b, allowed * r) t ->
            a C.obj * (b, a, disallowed * allowed) t =
         fun b_obj -> function
          | Base (small_morph_hint, morph) ->
            ( C.src b_obj morph,
              Base
                (H.right_adjoint small_morph_hint, C.right_adjoint b_obj morph)
            )
          | None a_obj -> a_obj, None b_obj
          | Compose (f_morph_hint, g_morph_hint) ->
            let mid, f_morph_hint_adj = aux b_obj f_morph_hint in
            let src, g_morph_hint_adj = aux mid g_morph_hint in
            src, Compose (g_morph_hint_adj, f_morph_hint_adj)
          | Hole_compose morph ->
            C.src b_obj morph, Hole_compose (C.right_adjoint b_obj morph)
        in
        fun b_obj h -> snd (aux b_obj h)

      module Allow_disallow = Magic_allow_disallow (struct
        type ('a, 'b, 'd) sided = ('a, 'b, 'd) t constraint 'd = 'l * 'r

        let rec allow_left :
            type l a b r. (a, b, allowed * r) t -> (a, b, l * r) t =
         fun h ->
          match h with
          | Base (morph_hint, morph) ->
            Base (H.Allow_disallow.allow_left morph_hint, C.allow_left morph)
          | None a_obj -> None a_obj
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (allow_left a_morph_hint, allow_left b_morph_hint)
          | Hole_compose morph -> Hole_compose (C.allow_left morph)

        let rec allow_right :
            type a b l r. (a, b, l * allowed) t -> (a, b, l * r) t =
         fun h ->
          match h with
          | Base (morph_hint, morph) ->
            Base (H.Allow_disallow.allow_right morph_hint, C.allow_right morph)
          | None a_obj -> None a_obj
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (allow_right a_morph_hint, allow_right b_morph_hint)
          | Hole_compose morph -> Hole_compose (C.allow_right morph)

        let rec disallow_left :
            type a b l r. (a, b, l * r) t -> (a, b, disallowed * r) t =
         fun h ->
          match h with
          | Base (morph_hint, morph) ->
            Base
              (H.Allow_disallow.disallow_left morph_hint, C.disallow_left morph)
          | None a_obj -> None a_obj
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (disallow_left a_morph_hint, disallow_left b_morph_hint)
          | Hole_compose morph -> Hole_compose (C.disallow_left morph)

        let rec disallow_right :
            type a b l r. (a, b, l * r) t -> (a, b, l * disallowed) t =
         fun h ->
          match h with
          | Base (morph_hint, morph) ->
            Base
              ( H.Allow_disallow.disallow_right morph_hint,
                C.disallow_right morph )
          | None a_obj -> None a_obj
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (disallow_right a_morph_hint, disallow_right b_morph_hint)
          | Hole_compose morph -> Hole_compose (C.disallow_right morph)
      end)
    end

    type const_hint =
      | Base of H.const
      | Skip
          (** The [Skip] const hint is for a constant that should never produce an error *)

    type ('a, 'd) t =
      | Apply : ('b, 'a, 'd) Morph_hint.t * ('b, 'd) t -> ('a, 'd) t
      | Const : const_hint -> ('a, 'l * 'r) t
      | Branch : 'a * ('a, 'l * 'r) t * 'a * ('a, 'l * 'r) t -> ('a, 'l * 'r) t
      | Nil
      constraint 'd = _ * _
    [@@ocaml.warning "-62"]

    module Allow_disallow = Magic_allow_disallow (struct
      type ('a, _, 'd) sided = ('a, 'd) t constraint 'd = 'l * 'r

      let rec allow_left : type a l r. (a, allowed * r) t -> (a, l * r) t =
        function
        | Apply (f_hint, h) ->
          Apply (Morph_hint.Allow_disallow.allow_left f_hint, allow_left h)
        | Const h -> Const h
        | Branch (a, a_hint, b, b_hint) ->
          Branch (a, allow_left a_hint, b, allow_left b_hint)
        | Nil -> Nil

      let rec allow_right : type a l r. (a, l * allowed) t -> (a, l * r) t =
        function
        | Apply (f_hint, h) ->
          Apply (Morph_hint.Allow_disallow.allow_right f_hint, allow_right h)
        | Const h -> Const h
        | Branch (a, a_hint, b, b_hint) ->
          Branch (a, allow_right a_hint, b, allow_right b_hint)
        | Nil -> Nil

      let rec disallow_left : type a l r. (a, l * r) t -> (a, disallowed * r) t
          = function
        | Apply (f_hint, h) ->
          Apply (Morph_hint.Allow_disallow.disallow_left f_hint, disallow_left h)
        | Const h -> Const h
        | Branch (a, a_hint, b, b_hint) ->
          Branch (a, disallow_left a_hint, b, disallow_left b_hint)
        | Nil -> Nil

      let rec disallow_right : type a l r. (a, l * r) t -> (a, l * disallowed) t
          = function
        | Apply (f_hint, h) ->
          Apply
            (Morph_hint.Allow_disallow.disallow_right f_hint, disallow_right h)
        | Const h -> Const h
        | Branch (a, a_hint, b, b_hint) ->
          Branch (a, disallow_right a_hint, b, disallow_right b_hint)
        | Nil -> Nil
    end)
  end

  (** An intermediate representation for converting from [Comp_hint.t] to [hint] *)
  module Holed_hint = struct
    module Morph_hint = struct
      type ('a, 'b, 'd) t =
        | Base : 'd H.morph * ('a, 'b, 'd) C.morph -> ('a, 'b, 'd) t
        | None : 'a C.obj -> ('a, 'b, 'l * 'r) t
        | Hole_compose : ('a, 'b, 'l * 'r) C.morph -> ('a, 'b, 'l * 'r) t
        constraint 'd = _ * _
      [@@ocaml.warning "-62"]
    end

    type ('a, 'd) t =
      | Apply : ('b, 'a, 'd) Morph_hint.t * ('b, 'd) t -> ('a, 'd) t
      | Const : Comp_hint.const_hint -> ('a, 'l * 'r) t
      | Branch : 'a * ('a, 'l * 'r) t * 'a * ('a, 'l * 'r) t -> ('a, 'l * 'r) t
      | Nil
      constraint 'd = _ * _
    [@@ocaml.warning "-62"]
  end

  (** This is for resolving holes (i.e. [Hole_compose] values) after the composition
    morph hints have already been flattened. This contains mentions of [assert false]
    for cases that shouldn't be allowed.  *)
  let hint_of_holed_hint =
    (* The auxiliary function will take an accumulator, [acc], which is the most-recent
        base hint, as well as the morphisms encountered since then. When considering a
        hole hint, we will add to this morphism, and otherwise the accumulator will not
        be used, and the auxiliary function returns that the value has not been used.

        The interesting cases are:

        - [Apply (morph_hint, subhint)], where we should consider possiblilities for what
          [morph_hint] is:
          - [None], we can simply return an empty hint, and we have not used the
            accumulator.
          - [Base (morph_hint, morph)], we don't have to use the accumulator, and we
            recurse into the subhint, using these new details, [morph_hint] and [morph],
            as the new accumulator.
          - [Hole_compose morph], this is the only case where we will use the accumulator.
            We add [morph] to the accumulator and recurse into the subhint.

       - [Branch (x, x_hint, y, y_hint)], we will recurse into both hints, passing the
         same accumulator to the recursive calls. We have a special case here for if
         neither of them used the accumulator, in which case the entire call can avoid
         using the accumulator. Otherwise, we manually apply the accumulator to one that
         didn't use it (if both used the accumulator, we don't need to do anything here),
         and return a new branch construct with the subhints with the accumulator applied
         to each. *)
    let rec aux :
        type a b l r.
        acc:((l * r) H.morph * (a, b, l * r) C.morph) option ->
        (* TODO - decide if need both of these objects or if either can always be derived *)
        b C.obj ->
        a C.obj ->
        (a, l * r) Holed_hint.t ->
        [ `Acc_has_been_used of (b, l * r) hint
        | `Acc_was_not_used of (a, l * r) hint ] =
     fun ~acc b_obj a_obj -> function
      | Const const_hint -> (
        match const_hint with
        | Base const_hint -> `Acc_was_not_used (Const const_hint)
        | Skip -> assert false)
      | Nil -> `Acc_was_not_used Nil
      | Apply (morph_hint, subhint) -> (
        let recurse_and_apply new_acc b_obj a_obj subhint =
          (* This helper function will recurse into the subhint with the given
             new accumulator and, if the recursive call doesn't use the accumulator, will apply it to the output *)
          match aux ~acc:(Some new_acc) b_obj a_obj subhint with
          | `Acc_has_been_used subhint' -> subhint'
          | `Acc_was_not_used subhint'_without_acc ->
            Apply (fst new_acc, snd new_acc, subhint'_without_acc)
        in
        match morph_hint with
        | None _ -> `Acc_was_not_used Nil
        | Base (morph_hint, morph) ->
          `Acc_was_not_used
            (recurse_and_apply (morph_hint, morph) a_obj (C.src a_obj morph)
               subhint)
        | Hole_compose morph ->
          let new_acc =
            match acc with
            | None ->
              (* This is the case where we've reached a [Hole_compose] as our first
                 morph hint, but this isn't allowed so we don't consider this case *)
              assert false
            | Some (acc_morph_hint, acc_morph) ->
              acc_morph_hint, C.compose b_obj acc_morph morph
          in
          `Acc_has_been_used
            (recurse_and_apply new_acc b_obj (C.src a_obj morph) subhint))
      | Branch (x, x_hint, y, y_hint) -> (
        let get_acc_and_prev_x_y () =
          (* This function assumes that one or both of the recursive calls returns
             that it used the accumulator, and so the accumulator must not be a [None] *)
          let acc_morph_hint, acc_morph =
            (* For the recursive call to claim to have used [acc], it must not be [None] *)
            Option.get acc
          in
          let x' = C.apply b_obj acc_morph x in
          let y' = C.apply b_obj acc_morph y in
          acc_morph_hint, acc_morph, x', y'
        in
        let apply_acc_hint acc_morph_hint acc_morph hint =
          Apply (acc_morph_hint, acc_morph, hint)
        in
        match aux ~acc b_obj a_obj x_hint, aux ~acc b_obj a_obj y_hint with
        | ( `Acc_was_not_used x_hint'_without_acc,
            `Acc_was_not_used y_hint'_without_acc ) ->
          `Acc_was_not_used
            (Branch (x, x_hint'_without_acc, y, y_hint'_without_acc))
        | `Acc_was_not_used x_hint'_without_acc, `Acc_has_been_used y_hint' ->
          let acc_morph_hint, acc_morph, x', y' = get_acc_and_prev_x_y () in
          let x_hint' =
            apply_acc_hint acc_morph_hint acc_morph x_hint'_without_acc
          in
          `Acc_has_been_used (Branch (x', x_hint', y', y_hint'))
        | `Acc_has_been_used x_hint', `Acc_was_not_used y_hint'_without_acc ->
          let acc_morph_hint, acc_morph, x', y' = get_acc_and_prev_x_y () in
          let y_hint' =
            apply_acc_hint acc_morph_hint acc_morph y_hint'_without_acc
          in
          `Acc_has_been_used (Branch (x', x_hint', y', y_hint'))
        | `Acc_has_been_used x_hint', `Acc_has_been_used y_hint' ->
          let _acc_morph_hint, _acc_morph, x', y' = get_acc_and_prev_x_y () in
          `Acc_has_been_used (Branch (x', x_hint', y', y_hint')))
    in
    fun a_obj h ->
      match aux ~acc:None a_obj a_obj h with
      | `Acc_has_been_used _ ->
        (* Since we didn't pass [acc] in, it is impossible for it to have used it *)
        assert false
      | `Acc_was_not_used h' -> h'

  (** This is for removing compositions. This function doesn't contain any [assert false]
  as it is just for a straightforward transformation *)
  let rec holed_hint_of_comp_hint :
      type a l r. (a, l * r) Comp_hint.t -> (a, l * r) Holed_hint.t = function
    | Const const_hint -> Const const_hint
    | Branch (x, x_hint, y, y_hint) ->
      Branch
        (x, holed_hint_of_comp_hint x_hint, y, holed_hint_of_comp_hint y_hint)
    | Nil -> Nil
    | Apply (morph_hint, subhint) ->
      let rec holed_hint_of_apply_comp_hint :
          type b a l r.
          (b, l * r) Holed_hint.t ->
          (b, a, l * r) Comp_hint.Morph_hint.t ->
          (a, l * r) Holed_hint.t =
       fun subhint -> function
        | Base (morph_hint, morph) -> Apply (Base (morph_hint, morph), subhint)
        | None a_obj -> Apply (None a_obj, subhint)
        | Hole_compose morph -> Apply (Hole_compose morph, subhint)
        | Compose (h1, h2) ->
          holed_hint_of_apply_comp_hint
            (holed_hint_of_apply_comp_hint subhint h2)
            h1
      in
      let subhint' = holed_hint_of_comp_hint subhint in
      holed_hint_of_apply_comp_hint subhint' morph_hint

  let hint_of_comp_hint a_obj x =
    hint_of_holed_hint a_obj (holed_hint_of_comp_hint x)

  type any_morph = Any_morph : ('a, 'b, 'd) C.morph -> any_morph

  module VarMap = Map.Make (struct
    type t = int * any_morph

    let compare (i1, m1) (i2, m2) =
      match Int.compare i1 i2 with 0 -> compare m1 m2 | i -> i
  end)

  (** Map the function to the list, and returns the first [Error] found;
      Returns [Ok ()] if no error. *)
  let find_error (f : 'x -> ('a, 'b) Result.t) (t : 'x VarMap.t) :
      ('a, 'b) Result.t =
    let r = ref (Ok ()) in
    let _ =
      VarMap.for_all
        (fun _ x ->
          match f x with
          | Ok () -> true
          | Error _ as e ->
            r := e;
            false)
        t
    in
    !r

  let var_map_to_list t = VarMap.fold (fun _ a xs -> a :: xs) t []

  type 'a var =
    { mutable vlower : 'a lmorphvar VarMap.t;
          (** A list of variables directly under the current variable.
        Each is a pair [f] [v], and we have [f v <= u] where [u] is the current
        variable.
        TODO: consider using hashset for quicker deduplication *)
      mutable upper : 'a;  (** The precise upper bound of the variable *)
      mutable upper_hint : ('a, right_only) Comp_hint.t;
          (** Hints for [upper] *)
      mutable lower : 'a;
          (** The *conservative* lower bound of the variable.
       Why conservative: if a user calls [submode c u] where [c] is
        some constant and [u] some variable, we can modify [u.lower] of course.
       Idealy we should also modify all [v.lower] where [v] is variable above [u].
       However, we only have [vlower] not [vupper]. Therefore, the [lower] of
       higher variables are not updated immediately, hence conservative. Those
       [lower] of higher variables can be made precise later on demand, see
       [zap_to_floor_var_aux].

       One might argue for an additional [vupper] field, so that [lower] are
       always precise. While this might be doable, we note that the "hotspot" of
       the mode solver is to detect conflict, which is already achieved without
       precise [lower]. Adding [vupper] and keeping [lower] precise will come
       at extra cost. *)
      (* To summarize, INVARIANT:
         - For any variable [v], we have [v.lower <= v.upper].
         - Variables that have been fully constrained will have
         [v.lower = v.upper]. Note that adding a boolean field indicating that
         won't help much.
         - For any [v] and [f u \in v.vlower], we have [f u.upper <= v.upper], but not
         necessarily [f u.lower <= v.lower]. *)
      mutable lower_hint : ('a, left_only) Comp_hint.t;
          (** Hints for [lower] *)
      id : int  (** For identification/printing *)
    }

  and 'b lmorphvar = ('b, left_only) morphvar

  and ('b, 'd) morphvar =
    | Amorphvar :
        'a var * ('a, 'b, 'd) C.morph * ('a, 'b, 'd) Comp_hint.Morph_hint.t
        -> ('b, 'd) morphvar
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  let get_key (Amorphvar (v, m, _)) = v.id, Any_morph m

  module VarSet = Set.Make (Int)

  type change =
    | Cupper : 'a var * 'a * ('a, right_only) Comp_hint.t -> change
    | Clower : 'a var * 'a * ('a, left_only) Comp_hint.t -> change
    | Cvlower : 'a var * 'a lmorphvar VarMap.t -> change

  type changes = change list

  let undo_change = function
    | Cupper (v, upper, upper_hint) ->
      v.upper <- upper;
      v.upper_hint <- upper_hint
    | Clower (v, lower, lower_hint) ->
      v.lower <- lower;
      v.lower_hint <- lower_hint
    | Cvlower (v, vlower) -> v.vlower <- vlower

  let empty_changes = []

  let undo_changes l = List.iter undo_change l

  (** [append_changes l0 l1] returns a log that's equivalent to [l0] followed by
  [l1]. *)
  let append_changes l0 l1 = l1 @ l0

  type ('a, 'd) mode =
    | Amode :
        'a * ('a, 'l * 'r_) Comp_hint.t * ('a, 'l_ * 'r) Comp_hint.t
        -> ('a, 'l * 'r) mode
    | Amodevar : ('a, 'd) morphvar -> ('a, 'd) mode
    | Amodejoin :
        'a
        * ('a, 'l * disallowed) Comp_hint.t
        * ('a, 'l * disallowed) morphvar VarMap.t
        -> ('a, 'l * disallowed) mode
        (** [Amodejoin a c [mv0, mv1, ..]] represents [a join mv0 join mv1 join ..]
          with the hint [c] for [a] (the morphvars have their own hints). *)
    | Amodemeet :
        'a
        * ('a, disallowed * 'r) Comp_hint.t
        * ('a, disallowed * 'r) morphvar VarMap.t
        -> ('a, disallowed * 'r) mode
        (** [Amodemeet a c [mv0, mv1, ..]] represents [a meet mv0 meet mv1 meet ..]
          with the hint [c] for [a] (the morphvars have their own hints). *)
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  (** Prints a mode variable, including the set of variables below it
      (recursively). To handle cycles, [traversed] is the set of variables that
      we have already printed and will be skipped. An example of cycle:

      Consider a lattice containing three elements A = {0, 1, 2} with the linear
      lattice structure: 0 < 1 < 2. Furthermore, we define a morphism
      f : A -> A
      f 0 = 0
      f 1 = 2
      f 2 = 2

      Note that f has a left right, which allows us to write f on the LHS of
      submode. Say we create a unconstrained variable [x], and invoke submode:
      f x <= x
      this would result in adding (f, x) into the [vlower] of [x]. That is,
      there will be a self-loop on [x].
      *)
  let rec print_var : type a. ?traversed:VarSet.t -> a C.obj -> _ -> a var -> _
      =
   fun ?traversed obj ppf v ->
    Format.fprintf ppf "modevar#%x[%a .. %a]" v.id (C.print obj) v.lower
      (C.print obj) v.upper;
    match traversed with
    | None -> ()
    | Some traversed ->
      if VarSet.mem v.id traversed
      then ()
      else
        let traversed = VarSet.add v.id traversed in
        let p = print_morphvar ~traversed obj in
        Format.fprintf ppf "{%a}" (Format.pp_print_list p)
          (var_map_to_list v.vlower)

  and print_morphvar :
      type a l r.
      ?traversed:VarSet.t -> a C.obj -> _ -> (a, l * r) morphvar -> _ =
   fun ?traversed dst ppf (Amorphvar (v, f, _)) ->
    let src = C.src dst f in
    Format.fprintf ppf "%a(%a)" (C.print_morph dst) f (print_var ?traversed src)
      v

  let print_raw :
      type a l r.
      ?verbose:bool -> a C.obj -> Format.formatter -> (a, l * r) mode -> unit =
   fun ?(verbose = false) (obj : a C.obj) ppf m ->
    let traversed = if verbose then Some VarSet.empty else Option.None in
    match m with
    | Amode (a, _, _) -> C.print obj ppf a
    | Amodevar mv -> print_morphvar ?traversed obj ppf mv
    | Amodejoin (a, _, mvs) ->
      Format.fprintf ppf "join(%a,%a)" (C.print obj) a
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
           (print_morphvar ?traversed obj))
        (var_map_to_list mvs)
    | Amodemeet (a, _, mvs) ->
      Format.fprintf ppf "meet(%a,%a)" (C.print obj) a
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
           (print_morphvar ?traversed obj))
        (var_map_to_list mvs)

  module Morphvar = Magic_allow_disallow (struct
    type ('a, _, 'd) sided = ('a, 'd) morphvar constraint 'd = 'l * 'r

    let allow_left :
        type a l r. (a, allowed * r) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m, h) ->
        Amorphvar
          (v, C.allow_left m, Comp_hint.Morph_hint.Allow_disallow.allow_left h)

    let allow_right :
        type a l r. (a, l * allowed) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m, h) ->
        Amorphvar
          (v, C.allow_right m, Comp_hint.Morph_hint.Allow_disallow.allow_right h)

    let disallow_left :
        type a l r. (a, l * r) morphvar -> (a, disallowed * r) morphvar =
      function
      | Amorphvar (v, m, h) ->
        Amorphvar
          ( v,
            C.disallow_left m,
            Comp_hint.Morph_hint.Allow_disallow.disallow_left h )

    let disallow_right :
        type a l r. (a, l * r) morphvar -> (a, l * disallowed) morphvar =
      function
      | Amorphvar (v, m, h) ->
        Amorphvar
          ( v,
            C.disallow_right m,
            Comp_hint.Morph_hint.Allow_disallow.disallow_right h )
  end)

  include Magic_allow_disallow (struct
    type ('a, _, 'd) sided = ('a, 'd) mode constraint 'd = 'l * 'r

    let allow_left : type a l r. (a, allowed * r) mode -> (a, l * r) mode =
      function
      | Amode (c, h_lower, h_upper) ->
        Amode (c, Comp_hint.Allow_disallow.allow_left h_lower, h_upper)
      | Amodevar mv -> Amodevar (Morphvar.allow_left mv)
      | Amodejoin (c, h, mvs) ->
        Amodejoin
          ( c,
            Comp_hint.Allow_disallow.allow_left h,
            VarMap.map Morphvar.allow_left mvs )

    let allow_right : type a l r. (a, l * allowed) mode -> (a, l * r) mode =
      function
      | Amode (c, h_lower, h_upper) ->
        Amode (c, h_lower, Comp_hint.Allow_disallow.allow_right h_upper)
      | Amodevar mv -> Amodevar (Morphvar.allow_right mv)
      | Amodemeet (c, h, mvs) ->
        Amodemeet
          ( c,
            Comp_hint.Allow_disallow.allow_right h,
            VarMap.map Morphvar.allow_right mvs )

    let disallow_left : type a l r. (a, l * r) mode -> (a, disallowed * r) mode
        = function
      | Amode (c, h_lower, h_upper) ->
        Amode (c, Comp_hint.Allow_disallow.disallow_left h_lower, h_upper)
      | Amodevar mv -> Amodevar (Morphvar.disallow_left mv)
      | Amodejoin (c, h, mvs) ->
        Amodejoin
          ( c,
            Comp_hint.Allow_disallow.disallow_left h,
            VarMap.map Morphvar.disallow_left mvs )
      | Amodemeet (c, h, mvs) ->
        Amodemeet (c, h, VarMap.map Morphvar.disallow_left mvs)

    let disallow_right : type a l r. (a, l * r) mode -> (a, l * disallowed) mode
        = function
      | Amode (c, h_lower, h_upper) ->
        Amode (c, h_lower, Comp_hint.Allow_disallow.disallow_right h_upper)
      | Amodevar mv -> Amodevar (Morphvar.disallow_right mv)
      | Amodejoin (c, h, mvs) ->
        Amodejoin (c, h, VarMap.map Morphvar.disallow_right mvs)
      | Amodemeet (c, h, mvs) ->
        Amodemeet
          ( c,
            Comp_hint.Allow_disallow.disallow_right h,
            VarMap.map Morphvar.disallow_right mvs )
  end)

  let mlower dst (Amorphvar (var, morph, _hint)) = C.apply dst morph var.lower

  let mlower_hint (Amorphvar (var, _morph, hint)) : _ Comp_hint.t =
    Apply
      ( Comp_hint.Morph_hint.Allow_disallow.disallow_right hint,
        Comp_hint.Allow_disallow.disallow_right var.lower_hint )

  let mupper dst (Amorphvar (var, morph, _hint)) = C.apply dst morph var.upper

  let mupper_hint (Amorphvar (var, _morph, hint)) : _ Comp_hint.t =
    Apply
      ( Comp_hint.Morph_hint.Allow_disallow.disallow_left hint,
        Comp_hint.Allow_disallow.disallow_left var.upper_hint )

  let min (type a) (obj : a C.obj) = Amode (C.min obj, Const Skip, Nil)

  let max (type a) (obj : a C.obj) = Amode (C.max obj, Nil, Const Skip)

  let of_const _obj ?hint a =
    let hint : _ Comp_hint.t =
      match hint with Option.None -> Nil | Some h -> Const (Base h)
    in
    Amode (a, hint, hint)

  let apply_morphvar dst morph morph_hint (Amorphvar (var, morph', morph'_hint))
      =
    (* TODO - handle [Wait_compose] here (make special [compose] function) *)
    Amorphvar
      (var, C.compose dst morph morph', Compose (morph_hint, morph'_hint))

  let apply_ :
      type a b l r.
      b C.obj ->
      hint:(a, b, l * r) Comp_hint.Morph_hint.t ->
      (a, b, l * r) C.morph ->
      (a, l * r) mode ->
      (b, l * r) mode =
   fun dst ~(hint : (a, b, l * r) Comp_hint.Morph_hint.t) morph m ->
    match m with
    | Amode (a, a_hint_lower, a_hint_upper) ->
      Amode
        ( C.apply dst morph a,
          (* TODO - check if need to handle [Wait_compose] here *)
          Apply
            ( Comp_hint.Morph_hint.Allow_disallow.disallow_right hint,
              Comp_hint.Allow_disallow.disallow_right a_hint_lower ),
          Apply
            ( Comp_hint.Morph_hint.Allow_disallow.disallow_left hint,
              Comp_hint.Allow_disallow.disallow_left a_hint_upper ) )
    | Amodevar mv -> Amodevar (apply_morphvar dst morph hint mv)
    | Amodejoin (a, a_hint, vs) ->
      Amodejoin
        ( C.apply dst morph a,
          (* TODO - check if need to handle [Wait_compose] here *)
          Apply (hint, a_hint),
          VarMap.map (apply_morphvar dst morph hint) vs )
    | Amodemeet (a, a_hint, vs) ->
      Amodemeet
        ( C.apply dst morph a,
          (* TODO - check if need to handle [Wait_compose] here *)
          Apply (hint, a_hint),
          VarMap.map (apply_morphvar dst morph hint) vs )

  let apply dst ?(hint = `None) morph =
    apply_ dst
      ~hint:
        (match hint with
        | `None -> None (C.src dst morph)
        | `Hint small_morph_hint -> Base (small_morph_hint, morph)
        | `Hole -> Hole_compose morph)
      morph

  let hint_biased_join obj a a_hint b b_hint =
    (* A version of [hint_join] that assumes that a <= b is false,
       so won't perform this check. *)
    if C.le obj b a then a_hint else Comp_hint.Branch (a, a_hint, b, b_hint)

  let hint_join obj a a_hint b b_hint =
    (* This just provides a minor optimization to avoid having unnecessarily-nested Branch hints.
       When a join, [join a b], is performed, the hint we use for the result should be a [Branch]
       if both arguments are relevant, but if we have that [a <= b] in the lattice structure then
       we don't really need to consider the hint for [a], and vice versa for [b]'s hint. *)
    if C.le obj a b then b_hint else hint_biased_join obj a a_hint b b_hint

  let hint_biased_meet obj a a_hint b b_hint =
    (* A version of [hint_meet] that assumes that a <= b is false,
       so won't perform this check. *)
    if C.le obj b a then b_hint else Comp_hint.Branch (a, a_hint, b, b_hint)

  let hint_meet obj a a_hint b b_hint =
    (* Comments for [hint_join] apply here similarly but the other way round
       (since this is for meet, not join) *)
    if C.le obj a b then a_hint else hint_biased_meet obj a a_hint b b_hint

  (** Calling [update_lower ~log obj v a a_hint] assumes that [not (a <= v.lower)].
      Arguments are not checked and used directly. They must satisfy the
        INVARIANT listed above. *)
  let update_lower (type a) ~log (obj : a C.obj) v a a_hint =
    (match log with
    | Option.None -> ()
    | Some log -> log := Clower (v, v.lower, v.lower_hint) :: !log);
    let new_lower = C.join obj v.lower a in
    let new_lower_hint = hint_biased_join obj a a_hint v.lower v.lower_hint in
    v.lower <- new_lower;
    v.lower_hint <- new_lower_hint

  (** Calling [update_upper ~log obj v a a_hint] assumes that [not (v.upper <= a)].
      Arguments are not checked and used directly. They must satisfy the
        INVARIANT listed above. *)
  let update_upper (type a) ~log (obj : a C.obj) v a a_hint =
    (match log with
    | Option.None -> ()
    | Some log -> log := Cupper (v, v.upper, v.upper_hint) :: !log);
    let new_upper = C.meet obj v.upper a in
    let new_upper_hint = hint_biased_meet obj v.upper v.upper_hint a a_hint in
    v.upper <- new_upper;
    v.upper_hint <- new_upper_hint

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let set_vlower ~log v vlower =
    (match log with
    | Option.None -> ()
    | Some log -> log := Cvlower (v, v.vlower) :: !log);
    v.vlower <- vlower

  let submode_cv :
      type a.
      log:_ ->
      a C.obj ->
      a ->
      (a, left_only) Comp_hint.t ->
      a var ->
      (unit, a * (a, right_only) Comp_hint.t) Result.t =
    fun (type a) ~log (obj : a C.obj) a' a'_hint v ->
     if C.le obj a' v.lower
     then Ok ()
     else if not (C.le obj a' v.upper)
     then Error (v.upper, v.upper_hint)
     else (
       update_lower ~log obj v a' a'_hint;
       if C.le obj v.upper v.lower then set_vlower ~log v VarMap.empty;
       Ok ())

  let submode_cmv :
      type a l.
      log:_ ->
      a C.obj ->
      a ->
      (a, left_only) Comp_hint.t ->
      (a, l * allowed) morphvar ->
      (unit, a * (a, right_only) Comp_hint.t) Result.t =
   fun ~log obj a a_hint (Amorphvar (v, f, f_hint) as mv) ->
    let mlower = mlower obj mv in
    let mupper = mupper obj mv in
    let mupper_hint = mupper_hint mv in
    if C.le obj a mlower
    then Ok ()
    else if not (C.le obj a mupper)
    then Error (mupper, mupper_hint)
    else
      (* At this point we know [a <= f v], therefore [a] is in the downward
         closure of [f]'s image. Therefore, asking [a <= f v] is equivalent to
         asking [f' a <= v]. *)
      let f' = C.left_adjoint obj f in
      let f'_hint = Comp_hint.Morph_hint.left_adjoint obj f_hint in
      let src = C.src obj f in
      let a' = C.apply src f' a in
      let a'_hint = Comp_hint.Apply (f'_hint, a_hint) in
      submode_cv ~log src a' a'_hint v |> Result.get_ok;
      Ok ()

  (** Returns [Ok ()] if success; [Error x] if failed, and [x] is the next best
    (read: strictly higher) guess to replace the constant argument that MIGHT
    succeed. *)
  let rec submode_vc :
      type a.
      log:_ ->
      a C.obj ->
      a var ->
      a ->
      (a, right_only) Comp_hint.t ->
      (unit, a * (a, left_only) Comp_hint.t) Result.t =
    fun (type a) ~log (obj : a C.obj) v a' a'_hint ->
     if C.le obj v.upper a'
     then Ok ()
     else if not (C.le obj v.lower a')
     then Error (v.lower, v.lower_hint)
     else (
       update_upper ~log obj v a' a'_hint;
       let r =
         v.vlower
         |> find_error (fun mu ->
                let r = submode_mvc ~log obj mu a' a'_hint in
                (if Result.is_ok r
                then
                  (* Optimization: update [v.lower] based on [mlower u].*)
                  let mu_lower = mlower obj mu in
                  let mu_lower_hint = mlower_hint mu in
                  if not (C.le obj mu_lower v.lower)
                  then update_lower ~log obj v mu_lower mu_lower_hint);
                r)
       in
       if C.le obj v.upper v.lower then set_vlower ~log v VarMap.empty;
       r)

  and submode_mvc :
        'a 'r.
        log:change list ref option ->
        'a C.obj ->
        ('a, allowed * 'r) morphvar ->
        'a ->
        ('a, right_only) Comp_hint.t ->
        (unit, 'a * ('a, left_only) Comp_hint.t) Result.t =
   fun ~log obj (Amorphvar (v, f, f_hint) as mv) a a_hint ->
    (* See [submode_cmv] for why we need the following seemingly redundant
       lines. *)
    let mupper = mupper obj mv in
    let mlower = mlower obj mv in
    let mlower_hint = mlower_hint mv in
    if C.le obj mupper a
    then Ok ()
    else if not (C.le obj mlower a)
    then Error (mlower, mlower_hint)
    else
      let f' = C.right_adjoint obj f in
      let f'_hint = Comp_hint.Morph_hint.right_adjoint obj f_hint in
      let src = C.src obj f in
      let a' = C.apply src f' a in
      let a'_hint = Comp_hint.Apply (f'_hint, a_hint) in
      (* If [mlower] was precise, then the check
         [not (C.le obj (mlower obj mv) a)] should guarantee the following call
         to return [Ok ()]. However, [mlower] is not precise *)
      (* not using [Result.map_error] to avoid allocating closure *)
      match submode_vc ~log src v a' a'_hint with
      | Ok () -> Ok ()
      | Error (e, e_hint) ->
        Error
          ( C.apply obj f e,
            Apply
              (Comp_hint.Morph_hint.Allow_disallow.disallow_right f_hint, e_hint)
          )

  let eq_morphvar :
      type a l0 r0 l1 r1. (a, l0 * r0) morphvar -> (a, l1 * r1) morphvar -> bool
      =
   fun (Amorphvar (v0, f0, _f0_hint) as mv0)
       (Amorphvar (v1, f1, _f1_hint) as mv1) ->
    (* To align l0/l1, r0/r1; The existing disallow_left/right] is for [mode],
       not [morphvar]. *)
    Morphvar.(
      disallow_left (disallow_right mv0) == disallow_left (disallow_right mv1))
    || match C.eq_morph f0 f1 with None -> false | Some Refl -> v0 == v1

  let submode_mvmv (type a) ~log (dst : a C.obj)
      (Amorphvar (v, f, f_hint) as mv) (Amorphvar (u, g, g_hint) as mu) =
    if C.le dst (mupper dst mv) (mlower dst mu)
    then Ok ()
    else if eq_morphvar mv mu
    then Ok ()
    else
      let muupper = mupper dst mu in
      let muupper_hint = mupper_hint mu in
      (* The call f v <= g u translates to three steps:
         1. f v <= g u.upper
         2. f v.lower <= g u
         3. adding g' (f v) to the u.vlower, where g' is the left adjoint of g.
      *)
      match submode_mvc ~log dst mv muupper muupper_hint with
      | Error (a, a_hint) -> Error (a, a_hint, muupper, muupper_hint)
      | Ok () -> (
        let mvlower = mlower dst mv in
        let mvlower_hint = mlower_hint mv in
        match submode_cmv ~log dst mvlower mvlower_hint mu with
        | Error (a, a_hint) -> Error (mvlower, mvlower_hint, a, a_hint)
        | Ok () ->
          (* At this point, we know that [f v <= g u.upper], which means [f v]
             lies within the downward closure of [g]'s image. Therefore, asking [f
             v <= g u] is equivalent to asking [g' f v <= u] *)
          let g' = C.left_adjoint dst g in
          let g'_hint = Comp_hint.Morph_hint.left_adjoint dst g_hint in
          let src = C.src dst g in
          let g'f = C.compose src g' (C.disallow_right f) in
          let g'f_hint =
            Comp_hint.Morph_hint.Compose
              ( g'_hint,
                Comp_hint.Morph_hint.Allow_disallow.disallow_right f_hint )
          in
          let x = Amorphvar (v, g'f, g'f_hint) in
          let key = get_key x in
          if not (VarMap.mem key u.vlower)
          then set_vlower ~log u (VarMap.add key x u.vlower);
          Ok ())

  let cnt_id = ref 0

  let fresh ?upper ?upper_hint ?lower ?lower_hint ?vlower obj =
    let id = !cnt_id in
    cnt_id := id + 1;
    let upper = Option.value upper ~default:(C.max obj) in
    let upper_hint = Option.value upper_hint ~default:Comp_hint.Nil in
    let lower = Option.value lower ~default:(C.min obj) in
    let lower_hint = Option.value lower_hint ~default:Comp_hint.Nil in
    let vlower = Option.value vlower ~default:VarMap.empty in
    { upper; upper_hint; lower; lower_hint; vlower; id }

  let submode (type a r l) (obj : a C.obj) (a : (a, allowed * r) mode)
      (b : (a, l * allowed) mode) ~log =
    let submode_cc ~log:_ obj left left_hint_lower right right_hint_upper =
      if C.le obj left right
      then Ok ()
      else
        Error
          { left;
            left_hint = hint_of_comp_hint obj left_hint_lower;
            right;
            right_hint = hint_of_comp_hint obj right_hint_upper
          }
    in
    let submode_mvc ~log obj v right right_hint_upper =
      Result.map_error
        (fun (left, left_hint) ->
          { left;
            left_hint = hint_of_comp_hint obj left_hint;
            right;
            right_hint = hint_of_comp_hint obj right_hint_upper
          })
        (submode_mvc ~log obj v right right_hint_upper)
    in
    let submode_cmv ~log obj left left_hint_lower v =
      Result.map_error
        (fun (right, right_hint) ->
          { left;
            left_hint = hint_of_comp_hint obj left_hint_lower;
            right;
            right_hint = hint_of_comp_hint obj right_hint
          })
        (submode_cmv ~log obj left left_hint_lower v)
    in
    let submode_mvmv ~log obj v u =
      Result.map_error
        (fun (left, left_hint, right, right_hint) ->
          { left;
            left_hint = hint_of_comp_hint obj left_hint;
            right;
            right_hint = hint_of_comp_hint obj right_hint
          })
        (submode_mvmv ~log obj v u)
    in
    match a, b with
    | ( Amode (left, left_hint_lower, _left_hint_upper),
        Amode (right, _right_hint_lower, right_hint_upper) ) ->
      submode_cc ~log obj left
        (Comp_hint.Allow_disallow.disallow_right left_hint_lower)
        right
        (Comp_hint.Allow_disallow.disallow_left right_hint_upper)
    | Amodevar v, Amode (right, _right_hint_lower, right_hint_upper) ->
      submode_mvc ~log obj v right
        (Comp_hint.Allow_disallow.disallow_left right_hint_upper)
    | Amode (left, left_hint_lower, _left_hint_upper), Amodevar v ->
      submode_cmv ~log obj left
        (Comp_hint.Allow_disallow.disallow_right left_hint_lower)
        v
    | Amodevar v, Amodevar u -> submode_mvmv ~log obj v u
    | Amode (a, a_hint_lower, _a_hint_upper), Amodemeet (b, b_hint, mvs) ->
      Result.bind
        (submode_cc ~log obj a
           (Comp_hint.Allow_disallow.disallow_right a_hint_lower)
           b b_hint)
        (fun () ->
          find_error
            (fun mv ->
              submode_cmv ~log obj a
                (Comp_hint.Allow_disallow.disallow_right a_hint_lower)
                mv)
            mvs)
    | Amodevar mv, Amodemeet (b, b_hint, mvs) ->
      Result.bind (submode_mvc ~log obj mv b b_hint) (fun () ->
          find_error (fun mv' -> submode_mvmv ~log obj mv mv') mvs)
    | Amodejoin (a, a_hint, mvs), Amode (b, _b_hint_lower, b_hint_upper) ->
      Result.bind
        (submode_cc ~log obj a a_hint b
           (Comp_hint.Allow_disallow.disallow_left b_hint_upper))
        (fun () ->
          find_error
            (fun mv' ->
              submode_mvc ~log obj mv' b
                (Comp_hint.Allow_disallow.disallow_left b_hint_upper))
            mvs)
    | Amodejoin (a, a_hint, mvs), Amodevar mv ->
      Result.bind (submode_cmv ~log obj a a_hint mv) (fun () ->
          find_error (fun mv' -> submode_mvmv ~log obj mv' mv) mvs)
    | Amodejoin (a, a_hint, mvs), Amodemeet (b, b_hint, mus) ->
      (* TODO: mabye create a intermediate variable? *)
      Result.bind (submode_cc ~log obj a a_hint b b_hint) (fun () ->
          Result.bind
            (find_error (fun mv -> submode_mvc ~log obj mv b b_hint) mvs)
            (fun () ->
              Result.bind
                (find_error (fun mu -> submode_cmv ~log obj a a_hint mu) mus)
                (fun () ->
                  find_error
                    (fun mu ->
                      find_error (fun mv -> submode_mvmv ~log obj mv mu) mvs)
                    mus)))

  let cons_dedup x xs = VarMap.add (get_key x) x xs

  let union_prefer_left t0 t1 = VarMap.union (fun _ a _b -> Some a) t0 t1

  let join (type a r) obj l =
    let rec loop :
        a ->
        (a, 'd) Comp_hint.t ->
        (a, allowed * disallowed) morphvar VarMap.t ->
        (a, allowed * r) mode list ->
        (a, allowed * disallowed) mode =
     fun a a_hint_lower mvs rest ->
      if C.le obj (C.max obj) a
      then
        (* In this case, [a] is the maximum element, and we can use [a_hint_lower]
           as the output's lower hint *)
        Amode (a, a_hint_lower, Const Skip)
      else
        match rest with
        | [] -> Amodejoin (a, a_hint_lower, mvs)
        | mv :: xs -> (
          match disallow_right mv with
          | Amode (b, b_hint_lower, _b_hint_upper) ->
            loop (C.join obj a b)
              (hint_join obj a a_hint_lower b
                 (Comp_hint.Allow_disallow.disallow_right b_hint_lower))
              mvs xs
          (* some minor optimization: if [a] is lower than [mlower mv], we
              should keep the latter instead. This helps to fail early in
             [submode] *)
          | Amodevar mv ->
            let mvlower = mlower obj mv in
            loop (C.join obj a mvlower)
              (hint_join obj a a_hint_lower mvlower (mlower_hint mv))
              (cons_dedup mv mvs) xs
          | Amodejoin (b, b_hint, mvs') ->
            loop (C.join obj a b)
              (hint_join obj a a_hint_lower b b_hint)
              (union_prefer_left mvs' mvs)
              xs)
    in
    loop (C.min obj) (Const Skip) VarMap.empty l

  let meet (type a l) obj l =
    let rec loop :
        a ->
        (a, 'd) Comp_hint.t ->
        (a, disallowed * allowed) morphvar VarMap.t ->
        (a, l * allowed) mode list ->
        (a, disallowed * allowed) mode =
     fun a a_hint_upper mvs rest ->
      if C.le obj a (C.min obj)
      then
        (* In this case, [a] is the minimum element, and we can use [a_hint_upper]
           as the output's upper hint *)
        Amode (a, Const Skip, a_hint_upper)
      else
        match rest with
        | [] -> Amodemeet (a, a_hint_upper, mvs)
        | mv :: xs -> (
          match disallow_left mv with
          | Amode (b, _b_hint_lower, b_hint_upper) ->
            loop (C.meet obj a b)
              (hint_meet obj a a_hint_upper b
                 (Comp_hint.Allow_disallow.disallow_left b_hint_upper))
              mvs xs
            (* some minor optimization: if [a] is higher than [mupper mv], we should keep the latter instead. This helps to fail early in [submode_log] *)
          | Amodevar mv ->
            let mvupper = mupper obj mv in
            loop (C.meet obj a mvupper)
              (hint_meet obj a a_hint_upper mvupper (mupper_hint mv))
              (cons_dedup mv mvs) xs
          | Amodemeet (b, b_hint, mvs') ->
            loop (C.meet obj a b)
              (hint_meet obj a a_hint_upper b b_hint)
              (union_prefer_left mvs' mvs)
              xs)
    in
    loop (C.max obj) (Const Skip) VarMap.empty l

  let get_loose_ceil : type a l r. a C.obj -> (a, l * r) mode -> a =
   fun obj m ->
    match m with
    | Amode (a, _a_hint_lower, _a_hint_upper) -> a
    | Amodevar mv -> mupper obj mv
    | Amodemeet (a, _a_hint, mvs) ->
      VarMap.fold (fun _ mv acc -> C.meet obj acc (mupper obj mv)) mvs a
    | Amodejoin (a, _a_hint, mvs) ->
      VarMap.fold (fun _ mv acc -> C.join obj acc (mupper obj mv)) mvs a

  let get_loose_floor : type a l r. a C.obj -> (a, l * r) mode -> a =
   fun obj m ->
    match m with
    | Amode (a, _a_hint_lower, _a_hint_upper) -> a
    | Amodevar mv -> mlower obj mv
    | Amodejoin (a, _a_hint, mvs) ->
      VarMap.fold (fun _ mv acc -> C.join obj acc (mupper obj mv)) mvs a
    | Amodemeet (a, _a_hint, mvs) ->
      VarMap.fold (fun _ mv acc -> C.meet obj acc (mlower obj mv)) mvs a

  (* Due to our biased implementation, the ceil is precise. *)
  let get_ceil = get_loose_ceil

  let zap_to_ceil : type a l. a C.obj -> (a, l * allowed) mode -> log:_ -> a =
   fun obj m ~log ->
    let ceil = get_ceil obj m in
    submode obj (Amode (ceil, Nil, Nil)) m ~log |> Result.get_ok;
    ceil

  (** Zap [mv] to its lower bound. Returns the [log] of the zapping, in
      case the caller are only interested in the lower bound and wants to
      reverse the zapping.

      As mentioned in [var], [mlower mv] is not precise; to get the precise
      lower bound of [mv], we call [submode mv (mlower mv)]. This will propagate
      to all its children, which might fail because some children's lower bound
      [a] is more up-to-date than [mv]. In that case, we call [submode mv a]. We
      repeat this process until no failure, and we will get the precise lower
      bound.

      The loop is guaranteed to terminate, because for each iteration our
      guessed lower bound is strictly higher; and all lattices are finite.
      *)
  let zap_to_floor_morphvar_aux (type a r) (obj : a C.obj)
      (mv : (a, allowed * r) morphvar) =
    let rec loop lower =
      let log = ref empty_changes in
      (* We provide an arbitrary, throwaway hint to this call to [submode_mvc]
         as it will either succeed or we will undo the change and not use the hint *)
      let r = submode_mvc ~log:(Some log) obj mv lower Nil in
      match r with
      | Ok () -> !log, lower
      | Error (a, _a_hint) ->
        (* Note, the hint here, [_a_hint], isn't useful as it is just for explaining why
           [a] is too high, but this is not useful as all [a] is then used for is as an
           upper bound (not a lower bound) in the next iteration. *)
        undo_changes !log;
        loop (C.join obj a lower)
    in
    loop (mlower obj mv)

  (** Zaps a morphvar to its floor and returns the floor. [commit] could be
      [Some log], in which case the zapping is appended to [log]; it could also
      be [None], in which case the zapping is reverted. The latter is useful
      when the caller only wants to know the floor without zapping. *)
  let zap_to_floor_morphvar obj mv ~commit =
    let log_, lower = zap_to_floor_morphvar_aux obj mv in
    (match commit with
    | Option.None -> undo_changes log_
    | Some log -> log := append_changes !log log_);
    lower

  let zap_to_floor : type a r. a C.obj -> (a, allowed * r) mode -> log:_ -> a =
   fun obj m ~log ->
    match m with
    | Amode (a, _a_hint_lower, _a_hint_upper) -> a
    | Amodevar mv -> zap_to_floor_morphvar obj mv ~commit:log
    | Amodejoin (a, _a_hint, mvs) ->
      let floor =
        VarMap.fold
          (fun _ mv acc ->
            C.join obj acc (zap_to_floor_morphvar obj mv ~commit:None))
          mvs a
      in
      VarMap.iter
        (fun _ mv ->
          (* Since this call to [submode_mvc] should always succeed,
             we provide a throwaway, arbitrary hint *)
          submode_mvc obj mv floor Nil ~log |> Result.get_ok)
        mvs;
      floor

  let get_floor : type a r. a C.obj -> (a, allowed * r) mode -> a =
   fun obj m ->
    match m with
    | Amode (a, _a_hint_lower, _a_hint_upper) -> a
    | Amodevar mv -> zap_to_floor_morphvar obj mv ~commit:None
    | Amodejoin (a, _a_hint, mvs) ->
      VarMap.fold
        (fun _ mv acc ->
          C.join obj acc (zap_to_floor_morphvar obj mv ~commit:None))
        mvs a

  let print :
      type a l r.
      ?verbose:bool -> a C.obj -> Format.formatter -> (a, l * r) mode -> unit =
   fun ?verbose (obj : a C.obj) ppf m ->
    let ceil = get_loose_ceil obj m in
    let floor = get_loose_floor obj m in
    if C.le obj ceil floor
    then C.print obj ppf ceil
    else print_raw ?verbose obj ppf m

  let newvar obj =
    Amodevar (Amorphvar (fresh obj, C.id, Base (H.morph_skip, C.id)))

  let newvar_above (type a r) (obj : a C.obj) (m : (a, allowed * r) mode) =
    match disallow_right m with
    | Amode (a, a_hint_lower, _a_hint_upper) ->
      if C.le obj (C.max obj) a
      then
        ( Amode (a, Comp_hint.Allow_disallow.allow_left a_hint_lower, Const Skip),
          false )
      else
        ( Amodevar
            (Amorphvar
               ( fresh ~lower:a
                   ~lower_hint:
                     (Comp_hint.Allow_disallow.disallow_right a_hint_lower)
                   obj,
                 C.id,
                 Base (H.morph_skip, C.id) )),
          true )
    | Amodevar mv ->
      (* [~lower] is not precise (because [mlower mv] is not precise), but
         it doesn't need to be *)
      ( Amodevar
          (Amorphvar
             ( fresh ~lower:(mlower obj mv) ~lower_hint:(mlower_hint mv)
                 ~vlower:(VarMap.singleton (get_key mv) mv)
                 obj,
               C.id,
               Base (H.morph_skip, C.id) )),
        true )
    | Amodejoin (a, a_hint, mvs) ->
      (* [~lower] is not precise here, but it doesn't need to be *)
      ( Amodevar
          (Amorphvar
             ( fresh ~lower:a ~lower_hint:a_hint ~vlower:mvs obj,
               C.id,
               Base (H.morph_skip, C.id) )),
        true )

  let newvar_below (type a l) (obj : a C.obj) (m : (a, l * allowed) mode) =
    match disallow_left m with
    | Amode (a, _a_hint_lower, a_hint_upper) ->
      if C.le obj a (C.min obj)
      then
        ( Amode
            (a, Const Skip, Comp_hint.Allow_disallow.allow_right a_hint_upper),
          false )
      else
        ( Amodevar
            (Amorphvar
               ( fresh ~upper:a
                   ~upper_hint:
                     (Comp_hint.Allow_disallow.disallow_left a_hint_upper)
                   obj,
                 C.id,
                 Base (H.morph_skip, C.id) )),
          true )
    | Amodevar mv ->
      let u = fresh obj in
      let mu = Amorphvar (u, C.id, Base (H.morph_skip, C.id)) in
      submode_mvmv obj ~log:None mu mv |> Result.get_ok;
      allow_left (Amodevar mu), true
    | Amodemeet (a, a_hint, mvs) ->
      let u = fresh obj in
      let mu = Amorphvar (u, C.id, Base (H.morph_skip, C.id)) in
      submode_mvc obj ~log:None mu a a_hint |> Result.get_ok;
      VarMap.iter
        (fun _ mv -> submode_mvmv obj ~log:None mu mv |> Result.get_ok)
        mvs;
      allow_left (Amodevar mu), true
end
[@@inline always]
