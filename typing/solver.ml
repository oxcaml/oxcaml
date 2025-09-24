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
    | Apply :
        'd H.Morph.t * ('b, 'a, 'd) C.morph * ('b, 'd) ahint
        -> ('a, 'd) hint
    | Const : 'd H.Const.t -> ('a, 'd) hint
    | Branch : 'd branch * ('a, 'd) ahint * ('a, 'd) ahint -> ('a, 'd) hint
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  and ('a, 'd) ahint = 'a * ('a, 'd) hint constraint 'd = _ * _

  type 'a error =
    { left : ('a, left_only) ahint;
      right : ('a, right_only) ahint
    }

  (** Hint used internally by the solver. This can be converted to [ahint] by [populate].
  *)
  module Comp_hint = struct
    module Morph_hint = struct
      (** [('a, 'b, 'd) t] is a hint for a morphism from ['a] to ['b] with allowance ['d]
      *)
      type ('a, 'b, 'd) t =
        | Base : 'd H.Morph.t * ('a, 'b, 'd) C.morph -> ('a, 'b, 'd) t
        | Compose :
            ('b, 'c, 'l * 'r) t * ('a, 'b, 'l * 'r) t
            -> ('a, 'c, 'l * 'r) t
        | Id : ('a, 'a, 'l * 'r) t
            (** Short-hand for [Base (H.id, C.id)] to save memory *)
        constraint 'd = _ * _
      [@@ocaml.warning "-62"]

      let left_adjoint =
        let rec aux :
            type a b l.
            b C.obj ->
            (a, b, l * allowed) t ->
            a C.obj * (b, a, allowed * disallowed) t =
         fun b_obj -> function
          | Id -> b_obj, Id
          | Base (small_morph_hint, morph) ->
            ( C.src b_obj morph,
              Base
                ( H.Morph.left_adjoint small_morph_hint,
                  C.left_adjoint b_obj morph ) )
          | Compose (f_morph_hint, g_morph_hint) ->
            let mid, f_morph_hint_adj = aux b_obj f_morph_hint in
            let src, g_morph_hint_adj = aux mid g_morph_hint in
            src, Compose (g_morph_hint_adj, f_morph_hint_adj)
        in
        fun b_obj h -> snd (aux b_obj h)

      let right_adjoint =
        let rec aux :
            type a b r.
            b C.obj ->
            (a, b, allowed * r) t ->
            a C.obj * (b, a, disallowed * allowed) t =
         fun b_obj -> function
          | Id -> b_obj, Id
          | Base (small_morph_hint, morph) ->
            ( C.src b_obj morph,
              Base
                ( H.Morph.right_adjoint small_morph_hint,
                  C.right_adjoint b_obj morph ) )
          | Compose (f_morph_hint, g_morph_hint) ->
            let mid, f_morph_hint_adj = aux b_obj f_morph_hint in
            let src, g_morph_hint_adj = aux mid g_morph_hint in
            src, Compose (g_morph_hint_adj, f_morph_hint_adj)
        in
        fun b_obj h -> snd (aux b_obj h)

      include Magic_allow_disallow (struct
        type ('a, 'b, 'd) sided = ('a, 'b, 'd) t constraint 'd = 'l * 'r

        let rec allow_left :
            type l a b r. (a, b, allowed * r) t -> (a, b, l * r) t =
         fun h ->
          match h with
          | Id -> Id
          | Base (morph_hint, morph) ->
            Base (H.Morph.allow_left morph_hint, C.allow_left morph)
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (allow_left a_morph_hint, allow_left b_morph_hint)

        let rec allow_right :
            type a b l r. (a, b, l * allowed) t -> (a, b, l * r) t =
         fun h ->
          match h with
          | Id -> Id
          | Base (morph_hint, morph) ->
            Base (H.Morph.allow_right morph_hint, C.allow_right morph)
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (allow_right a_morph_hint, allow_right b_morph_hint)

        let rec disallow_left :
            type a b l r. (a, b, l * r) t -> (a, b, disallowed * r) t =
         fun h ->
          match h with
          | Id -> Id
          | Base (morph_hint, morph) ->
            Base (H.Morph.disallow_left morph_hint, C.disallow_left morph)
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (disallow_left a_morph_hint, disallow_left b_morph_hint)

        let rec disallow_right :
            type a b l r. (a, b, l * r) t -> (a, b, l * disallowed) t =
         fun h ->
          match h with
          | Id -> Id
          | Base (morph_hint, morph) ->
            Base (H.Morph.disallow_right morph_hint, C.disallow_right morph)
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (disallow_right a_morph_hint, disallow_right b_morph_hint)
      end)

      let rec populate :
          type b a l r.
          a C.obj ->
          (b, a, l * r) t ->
          (b C.obj -> (b, l * r) ahint) ->
          (a, l * r) ahint =
       fun obj_a hint cont ->
        match hint with
        | Id -> cont obj_a
        | Base (morph_hint, morph) ->
          let obj_b = C.src obj_a morph in
          let ahint = cont obj_b in
          let a = C.apply obj_a morph (fst ahint) in
          a, Apply (morph_hint, morph, ahint)
        | Compose (h1, h2) ->
          populate obj_a h1 (fun obj_mid -> populate obj_mid h2 cont)
    end

    type ('a, 'd) t =
      | Apply : ('b, 'a, 'd) Morph_hint.t * ('b, 'd) t -> ('a, 'd) t
      | Const : 'd H.Const.t * 'a -> ('a, 'd) t
      | Branch : 'd branch * ('a, 'd) t * ('a, 'd) t -> ('a, 'd) t
      | Min : ('a, 'l * disallowed) t
          (** Short-hand for [Const (H.Const.min, C.min) to save memory] *)
      | Max : ('a, disallowed * 'r) t
          (** Short-hand for [Const (H.Const.max, C.max) to save memory] *)
      | Unknown : 'a -> ('a, 'l * 'r) t
          (** Short-hand for [Const (H.Const.unknown, a) to save memory] *)
      constraint 'd = _ * _
    [@@ocaml.warning "-62"]

    include Magic_allow_disallow (struct
      type ('a, _, 'd) sided = ('a, 'd) t constraint 'd = 'l * 'r

      let rec allow_left : type a l r. (a, allowed * r) t -> (a, l * r) t =
        function
        | Apply (f_hint, h) -> Apply (Morph_hint.allow_left f_hint, allow_left h)
        | Const (h, c) -> Const (H.Const.allow_left h, c)
        | Branch (Join, h1, h2) -> Branch (Join, allow_left h1, allow_left h2)
        | Min -> Min
        | Unknown c -> Unknown c

      let rec allow_right : type a l r. (a, l * allowed) t -> (a, l * r) t =
        function
        | Apply (f_hint, h) ->
          Apply (Morph_hint.allow_right f_hint, allow_right h)
        | Const (h, c) -> Const (H.Const.allow_right h, c)
        | Branch (Meet, h1, h2) -> Branch (Meet, allow_right h1, allow_right h2)
        | Max -> Max
        | Unknown c -> Unknown c

      let rec disallow_left : type a l r. (a, l * r) t -> (a, disallowed * r) t
          = function
        | Apply (f_hint, h) ->
          Apply (Morph_hint.disallow_left f_hint, disallow_left h)
        | Const (h, c) -> Const (H.Const.disallow_left h, c)
        | Branch (Join, h1, h2) ->
          Branch (Join, disallow_left h1, disallow_left h2)
        | Branch (Meet, h1, h2) ->
          Branch (Meet, disallow_left h1, disallow_left h2)
        | Min -> Min
        | Max -> Max
        | Unknown c -> Unknown c

      let rec disallow_right : type a l r. (a, l * r) t -> (a, l * disallowed) t
          = function
        | Apply (f_hint, h) ->
          Apply (Morph_hint.disallow_right f_hint, disallow_right h)
        | Const (h, c) -> Const (H.Const.disallow_right h, c)
        | Branch (Join, h1, h2) ->
          Branch (Join, disallow_right h1, disallow_right h2)
        | Branch (Meet, h1, h2) ->
          Branch (Meet, disallow_right h1, disallow_right h2)
        | Min -> Min
        | Max -> Max
        | Unknown c -> Unknown c
    end)

    (** This is for removing compositions. This function doesn't contain any [assert false]
  as it is just for a straightforward transformation *)
    let rec populate : type a l r. a C.obj -> (a, l * r) t -> (a, l * r) ahint =
     fun obj_a -> function
      | Min -> C.min obj_a, Const H.Const.min
      | Max -> C.max obj_a, Const H.Const.max
      | Unknown c -> c, Const H.Const.unknown
      | Const (const_hint, const) -> const, Const const_hint
      | Apply (morph_hint, hint) ->
        Morph_hint.populate obj_a morph_hint (fun src -> populate src hint)
      | Branch (b, hint1, hint2) -> (
        let ahint1 = populate obj_a hint1 in
        let ahint2 = populate obj_a hint2 in
        match b with
        | Join ->
          let a = C.join obj_a (fst ahint1) (fst ahint2) in
          a, Branch (Join, ahint1, ahint2)
        | Meet ->
          let a = C.meet obj_a (fst ahint1) (fst ahint2) in
          a, Branch (Meet, ahint1, ahint2))
  end

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

  type anyvar = Var : 'a var -> anyvar [@@unboxed]

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
        'a * ('a, 'l * _) Comp_hint.t * ('a, _ * 'r) Comp_hint.t
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
    let traversed = if verbose then Some VarSet.empty else None in
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
        Amorphvar (v, C.allow_left m, Comp_hint.Morph_hint.allow_left h)

    let allow_right :
        type a l r. (a, l * allowed) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m, h) ->
        Amorphvar (v, C.allow_right m, Comp_hint.Morph_hint.allow_right h)

    let disallow_left :
        type a l r. (a, l * r) morphvar -> (a, disallowed * r) morphvar =
      function
      | Amorphvar (v, m, h) ->
        Amorphvar (v, C.disallow_left m, Comp_hint.Morph_hint.disallow_left h)

    let disallow_right :
        type a l r. (a, l * r) morphvar -> (a, l * disallowed) morphvar =
      function
      | Amorphvar (v, m, h) ->
        Amorphvar (v, C.disallow_right m, Comp_hint.Morph_hint.disallow_right h)
  end)

  include Magic_allow_disallow (struct
    type ('a, _, 'd) sided = ('a, 'd) mode constraint 'd = 'l * 'r

    let allow_left : type a l r. (a, allowed * r) mode -> (a, l * r) mode =
      function
      | Amode (c, h_lower, h_upper) ->
        Amode (c, Comp_hint.allow_left h_lower, h_upper)
      | Amodevar mv -> Amodevar (Morphvar.allow_left mv)
      | Amodejoin (c, h, mvs) ->
        Amodejoin (c, Comp_hint.allow_left h, VarMap.map Morphvar.allow_left mvs)

    let allow_right : type a l r. (a, l * allowed) mode -> (a, l * r) mode =
      function
      | Amode (c, h_lower, h_upper) ->
        Amode (c, h_lower, Comp_hint.allow_right h_upper)
      | Amodevar mv -> Amodevar (Morphvar.allow_right mv)
      | Amodemeet (c, h, mvs) ->
        Amodemeet
          (c, Comp_hint.allow_right h, VarMap.map Morphvar.allow_right mvs)

    let disallow_left : type a l r. (a, l * r) mode -> (a, disallowed * r) mode
        = function
      | Amode (c, h_lower, h_upper) ->
        Amode (c, Comp_hint.disallow_left h_lower, h_upper)
      | Amodevar mv -> Amodevar (Morphvar.disallow_left mv)
      | Amodejoin (c, h, mvs) ->
        Amodejoin
          (c, Comp_hint.disallow_left h, VarMap.map Morphvar.disallow_left mvs)
      | Amodemeet (c, h, mvs) ->
        Amodemeet (c, h, VarMap.map Morphvar.disallow_left mvs)

    let disallow_right : type a l r. (a, l * r) mode -> (a, l * disallowed) mode
        = function
      | Amode (c, h_lower, h_upper) ->
        Amode (c, h_lower, Comp_hint.disallow_right h_upper)
      | Amodevar mv -> Amodevar (Morphvar.disallow_right mv)
      | Amodejoin (c, h, mvs) ->
        Amodejoin (c, h, VarMap.map Morphvar.disallow_right mvs)
      | Amodemeet (c, h, mvs) ->
        Amodemeet
          (c, Comp_hint.disallow_right h, VarMap.map Morphvar.disallow_right mvs)
  end)

  let mlower dst (Amorphvar (var, morph, _hint)) = C.apply dst morph var.lower

  let mlower_hint (Amorphvar (var, _morph, hint)) : _ Comp_hint.t =
    Apply
      ( Comp_hint.Morph_hint.disallow_right hint,
        Comp_hint.disallow_right var.lower_hint )

  let mupper dst (Amorphvar (var, morph, _hint)) = C.apply dst morph var.upper

  let mupper_hint (Amorphvar (var, _morph, hint)) : _ Comp_hint.t =
    Apply
      ( Comp_hint.Morph_hint.disallow_left hint,
        Comp_hint.disallow_left var.upper_hint )

  let min (type a) (obj : a C.obj) =
    let c = C.min obj in
    Amode (c, Min, Unknown c)

  let max (type a) (obj : a C.obj) =
    let c = C.max obj in
    Amode (c, Unknown c, Max)

  let of_const _obj ?hint a =
    let hint : _ Comp_hint.t =
      match hint with None -> Unknown a | Some h -> Const (h, a)
    in
    Amode (a, hint, hint)

  let apply_morphvar dst morph morph_hint (Amorphvar (var, morph', morph'_hint))
      =
    Amorphvar
      (var, C.compose dst morph morph', Compose (morph_hint, morph'_hint))

  let apply :
      type a b l r.
      b C.obj ->
      ?hint:(l * r) H.Morph.t ->
      (a, b, l * r) C.morph ->
      (a, l * r) mode ->
      (b, l * r) mode =
   fun dst ?(hint = H.Morph.unknown) morph m ->
    let hint = Comp_hint.Morph_hint.Base (hint, morph) in
    match m with
    | Amode (a, a_hint_lower, a_hint_upper) ->
      Amode
        ( C.apply dst morph a,
          Apply
            ( Comp_hint.Morph_hint.disallow_right hint,
              Comp_hint.disallow_right a_hint_lower ),
          Apply
            ( Comp_hint.Morph_hint.disallow_left hint,
              Comp_hint.disallow_left a_hint_upper ) )
    | Amodevar mv -> Amodevar (apply_morphvar dst morph hint mv)
    | Amodejoin (a, a_hint, vs) ->
      let hint = Comp_hint.Morph_hint.disallow_right hint in
      Amodejoin
        ( C.apply dst morph a,
          Apply (hint, a_hint),
          VarMap.map (apply_morphvar dst morph hint) vs )
    | Amodemeet (a, a_hint, vs) ->
      let hint = Comp_hint.Morph_hint.disallow_left hint in
      Amodemeet
        ( C.apply dst morph a,
          Apply (hint, a_hint),
          VarMap.map (apply_morphvar dst morph hint) vs )

  module Unhint = struct
    type ('a, 'd) t =
      | Unhint : ('b, 'a, 'd) C.morph * ('b, 'd) mode -> ('a, 'd) t
      constraint 'd = 'l * 'r
    [@@ocaml.warning "-62"]

    let unhint : type a l r. (a, l * r) mode -> (a, l * r) t =
     fun m -> Unhint (C.id, m)

    let hint :
        type a l r.
        a C.obj -> ?hint:(l * r) H.Morph.t -> (a, l * r) t -> (a, l * r) mode =
     fun dst ?hint (Unhint (morph, m)) -> apply dst ?hint morph m

    let apply :
        type a b l r.
        b C.obj -> (a, b, l * r) C.morph -> (a, l * r) t -> (b, l * r) t =
     fun dst morph (Unhint (f, m)) -> Unhint (C.compose dst morph f, m)
  end

  let hint_biased_join obj a a_hint b b_hint =
    (* A version of [hint_join] that assumes that a <= b is false,
       so won't perform this check. *)
    if C.le obj b a then a_hint else Comp_hint.Branch (Join, a_hint, b_hint)

  let hint_join obj a a_hint b b_hint =
    (* This just provides a minor optimization to avoid having unnecessarily-nested Branch hints.
       When a join, [join a b], is performed, the hint we use for the result should be a [Branch]
       if both arguments are relevant, but if we have that [a <= b] in the lattice structure then
       we don't really need to consider the hint for [a], and vice versa for [b]'s hint. *)
    if C.le obj a b then b_hint else hint_biased_join obj a a_hint b b_hint

  let hint_biased_meet obj a a_hint b b_hint =
    (* A version of [hint_meet] that assumes that a <= b is false,
       so won't perform this check. *)
    if C.le obj b a then b_hint else Comp_hint.Branch (Meet, a_hint, b_hint)

  let hint_meet obj a a_hint b b_hint =
    (* Comments for [hint_join] apply here similarly but the other way round
       (since this is for meet, not join) *)
    if C.le obj a b then a_hint else hint_biased_meet obj a a_hint b b_hint

  (** Calling [update_lower ~log obj v a a_hint] assumes that [not (a <= v.lower)].
      Arguments are not checked and used directly. They must satisfy the
        INVARIANT listed above. *)
  let update_lower (type a) ~log (obj : a C.obj) v a a_hint =
    (match log with
    | None -> ()
    | Some log -> log := Clower (v, v.lower, v.lower_hint) :: !log);
    v.lower <- C.join obj v.lower a;
    v.lower_hint <- hint_biased_join obj a a_hint v.lower v.lower_hint

  (** Calling [update_upper ~log obj v a a_hint] assumes that [not (v.upper <= a)].
      Arguments are not checked and used directly. They must satisfy the
        INVARIANT listed above. *)
  let update_upper (type a) ~log (obj : a C.obj) v a a_hint =
    (match log with
    | None -> ()
    | Some log -> log := Cupper (v, v.upper, v.upper_hint) :: !log);
    v.upper <- C.meet obj v.upper a;
    v.upper_hint <- hint_biased_meet obj v.upper v.upper_hint a a_hint

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let set_vlower ~log v vlower =
    (match log with
    | None -> ()
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
            Apply (Comp_hint.Morph_hint.disallow_right f_hint, e_hint) )

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
              (g'_hint, Comp_hint.Morph_hint.disallow_right f_hint)
          in
          let x = Amorphvar (v, g'f, g'f_hint) in
          let key = get_key x in
          if not (VarMap.mem key u.vlower)
          then set_vlower ~log u (VarMap.add key x u.vlower);
          Ok ())

  let vars = ref (0, [])

  let fresh ?upper ?upper_hint ?lower ?lower_hint ?vlower obj =
    let id, l = !vars in
    let upper, upper_hint =
      match upper, upper_hint with
      | None, None -> C.max obj, Comp_hint.Max
      | None, Some _ -> assert false
      | Some upper, None -> upper, Comp_hint.Unknown upper
      | Some upper, Some upper_hint -> upper, upper_hint
    in
    let lower, lower_hint =
      match lower, lower_hint with
      | None, None -> C.min obj, Comp_hint.Min
      | None, Some _ -> assert false
      | Some lower, None -> lower, Comp_hint.Unknown lower
      | Some lower, Some lower_hint -> lower, lower_hint
    in
    let vlower = Option.value vlower ~default:VarMap.empty in
    let var = { upper; upper_hint; lower; lower_hint; vlower; id } in
    vars := id + 1, Var var :: l;
    var

  let unhint_morphvar (Amorphvar (v, f, _)) =
    Amorphvar (v, f, Comp_hint.Morph_hint.Base (H.Morph.unknown, f))

  let unhint_var v =
    v.upper_hint <- Comp_hint.Unknown v.upper;
    v.lower_hint <- Comp_hint.Unknown v.lower;
    v.vlower <- VarMap.map unhint_morphvar v.vlower

  let unhint_vars () =
    let _, l = !vars in
    List.iter (fun (Var v) -> unhint_var v) l

  type ('a, 'd) hint_raw = ('a, 'd) Comp_hint.t

  type 'a error_raw =
    { left : 'a;
      left_hint : ('a, left_only) hint_raw;
      right : 'a;
      right_hint : ('a, right_only) hint_raw
    }

  let submode (type a r l) (obj : a C.obj) (a : (a, allowed * r) mode)
      (b : (a, l * allowed) mode) ~log =
    let submode_cc ~log:_ obj left left_hint right right_hint =
      if C.le obj left right
      then Ok ()
      else Error { left; left_hint; right; right_hint }
    in
    let submode_mvc ~log obj v right right_hint =
      Result.map_error
        (fun (left, left_hint) -> { left; left_hint; right; right_hint })
        (submode_mvc ~log obj v right right_hint)
    in
    let submode_cmv ~log obj left left_hint v =
      Result.map_error
        (fun (right, right_hint) -> { left; left_hint; right; right_hint })
        (submode_cmv ~log obj left left_hint v)
    in
    let submode_mvmv ~log obj v u =
      Result.map_error
        (fun (left, left_hint, right, right_hint) ->
          { left; left_hint; right; right_hint })
        (submode_mvmv ~log obj v u)
    in
    match a, b with
    | ( Amode (left, left_hint_lower, _left_hint_upper),
        Amode (right, _right_hint_lower, right_hint_upper) ) ->
      submode_cc ~log obj left
        (Comp_hint.disallow_right left_hint_lower)
        right
        (Comp_hint.disallow_left right_hint_upper)
    | Amodevar v, Amode (right, _right_hint_lower, right_hint_upper) ->
      submode_mvc ~log obj v right (Comp_hint.disallow_left right_hint_upper)
    | Amode (left, left_hint_lower, _left_hint_upper), Amodevar v ->
      submode_cmv ~log obj left (Comp_hint.disallow_right left_hint_lower) v
    | Amodevar v, Amodevar u -> submode_mvmv ~log obj v u
    | Amode (a, a_hint_lower, _a_hint_upper), Amodemeet (b, b_hint, mvs) ->
      Result.bind
        (submode_cc ~log obj a (Comp_hint.disallow_right a_hint_lower) b b_hint)
        (fun () ->
          find_error
            (fun mv ->
              submode_cmv ~log obj a (Comp_hint.disallow_right a_hint_lower) mv)
            mvs)
    | Amodevar mv, Amodemeet (b, b_hint, mvs) ->
      Result.bind (submode_mvc ~log obj mv b b_hint) (fun () ->
          find_error (fun mv' -> submode_mvmv ~log obj mv mv') mvs)
    | Amodejoin (a, a_hint, mvs), Amode (b, _b_hint_lower, b_hint_upper) ->
      Result.bind
        (submode_cc ~log obj a a_hint b (Comp_hint.disallow_left b_hint_upper))
        (fun () ->
          find_error
            (fun mv' ->
              submode_mvc ~log obj mv' b (Comp_hint.disallow_left b_hint_upper))
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

  let populate_hint obj a hint =
    let ahint = Comp_hint.populate obj hint in
    assert (Misc.Le_result.equal ~le:(C.le obj) (fst ahint) a);
    ahint

  let populate_error obj { left; left_hint; right; right_hint } =
    let left = populate_hint obj left left_hint in
    let right = populate_hint obj right right_hint in
    { left; right }

  let cons_dedup x xs = VarMap.add (get_key x) x xs

  let union_prefer_left t0 t1 = VarMap.union (fun _ a _b -> Some a) t0 t1

  let join (type a r) obj l =
    let rec loop :
        a ->
        (a, allowed * disallowed) Comp_hint.t ->
        (a, allowed * disallowed) morphvar VarMap.t ->
        (a, allowed * r) mode list ->
        (a, allowed * disallowed) mode =
     fun a a_hint_lower mvs rest ->
      if C.le obj (C.max obj) a
      then
        (* In this case, [a] is the maximum element, and we can use
           [a_hint_lower] as the output's lower hint *)
        Amode (a, a_hint_lower, Max)
      else
        match rest with
        | [] -> Amodejoin (a, a_hint_lower, mvs)
        | mv :: xs -> (
          match disallow_right mv with
          | Amode (b, b_hint_lower, _b_hint_upper) ->
            loop (C.join obj a b)
              (hint_join obj a a_hint_lower b
                 (Comp_hint.disallow_right b_hint_lower))
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
    loop (C.min obj) Min VarMap.empty l

  let meet (type a l) obj l =
    let rec loop :
        a ->
        (a, disallowed * allowed) Comp_hint.t ->
        (a, disallowed * allowed) morphvar VarMap.t ->
        (a, l * allowed) mode list ->
        (a, disallowed * allowed) mode =
     fun a a_hint_upper mvs rest ->
      if C.le obj a (C.min obj)
      then
        (* In this case, [a] is the minimum element, and we can use [a_hint_upper]
           as the output's upper hint *)
        Amode (a, Min, a_hint_upper)
      else
        match rest with
        | [] -> Amodemeet (a, a_hint_upper, mvs)
        | mv :: xs -> (
          match disallow_left mv with
          | Amode (b, _b_hint_lower, b_hint_upper) ->
            loop (C.meet obj a b)
              (hint_meet obj a a_hint_upper b
                 (Comp_hint.disallow_left b_hint_upper))
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
    loop (C.max obj) Max VarMap.empty l

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
    (* We want a hint to explain why [ceil] is high. However, we only have hint
       for why [ceil] is low. There is no good hint to use. *)
    submode obj (Amode (ceil, Unknown ceil, Unknown ceil)) m ~log
    |> Result.get_ok;
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
      (* We want a hint for why [lower] is low, but we only have hint for why [lower] is
         high. There is no good hint to use. *)
      let r = submode_mvc ~log:(Some log) obj mv lower (Unknown lower) in
      match r with
      | Ok () -> !log, lower
      | Error (a, _) ->
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
    | None -> undo_changes log_
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
          (* We want a hint for why [floor] is low. However, we only have hint
             for why [floor] is high. There is no hint to use. *)
          submode_mvc obj mv floor (Unknown floor) ~log |> Result.get_ok)
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

  let newvar obj = Amodevar (Amorphvar (fresh obj, C.id, Id))

  let newvar_above (type a r) (obj : a C.obj) (m : (a, allowed * r) mode) =
    match disallow_right m with
    | Amode (a, a_hint_lower, _a_hint_upper) ->
      if C.le obj (C.max obj) a
      then Amode (a, Comp_hint.allow_left a_hint_lower, Max), false
      else
        ( Amodevar
            (Amorphvar
               ( fresh ~lower:a
                   ~lower_hint:(Comp_hint.disallow_right a_hint_lower)
                   obj,
                 C.id,
                 Id )),
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
               Id )),
        true )
    | Amodejoin (a, a_hint, mvs) ->
      (* [~lower] is not precise here, but it doesn't need to be *)
      ( Amodevar
          (Amorphvar
             (fresh ~lower:a ~lower_hint:a_hint ~vlower:mvs obj, C.id, Id)),
        true )

  let newvar_below (type a l) (obj : a C.obj) (m : (a, l * allowed) mode) =
    match disallow_left m with
    | Amode (a, _a_hint_lower, a_hint_upper) ->
      if C.le obj a (C.min obj)
      then Amode (a, Min, Comp_hint.allow_right a_hint_upper), false
      else
        ( Amodevar
            (Amorphvar
               ( fresh ~upper:a
                   ~upper_hint:(Comp_hint.disallow_left a_hint_upper)
                   obj,
                 C.id,
                 Id )),
          true )
    | Amodevar mv ->
      let u = fresh obj in
      let mu = Amorphvar (u, C.id, Id) in
      submode_mvmv obj ~log:None mu mv |> Result.get_ok;
      allow_left (Amodevar mu), true
    | Amodemeet (a, a_hint, mvs) ->
      let u = fresh obj in
      let mu = Amorphvar (u, C.id, Id) in
      submode_mvc obj ~log:None mu a a_hint |> Result.get_ok;
      VarMap.iter
        (fun _ mv -> submode_mvmv obj ~log:None mu mv |> Result.get_ok)
        mvs;
      allow_left (Amodevar mu), true
end
[@@inline always]
