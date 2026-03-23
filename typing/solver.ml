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
module Fmt = Format_doc

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

  (** Hint used internally by the solver. This can be converted to [ahint] by
      [populate]. *)
  module Comp_hint = struct
    module Morph_hint = struct
      (** [('a, 'b, 'd) t] is a hint for a morphism from ['a] to ['b] with
          allowance ['d] *)
      type ('a, 'b, 'd) t =
        | Base : 'd H.Morph.t * ('a, 'b, 'd) C.morph -> ('a, 'b, 'd) t
        | Compose :
            ('b, 'c, 'l * 'r) t * ('a, 'b, 'l * 'r) t
            -> ('a, 'c, 'l * 'r) t
        | Id : ('a, 'a, 'l * 'r) t
            (** Short-hand for [Base (H.id, C.id)] to save memory *)
        constraint 'd = _ * _
      [@@ocaml.warning "-62"]

      let rec left_adjoint : type a b l.
          H.Pinpoint.t ->
          b C.obj ->
          (a, b, l * allowed) t ->
          H.Pinpoint.t * a C.obj * (b, a, allowed * disallowed) t =
       fun pp b_obj -> function
        | Id -> pp, b_obj, Id
        | Base (small_morph_hint, morph) ->
          let pp, small_morph_hint = H.Morph.left_adjoint pp small_morph_hint in
          ( pp,
            C.src b_obj morph,
            Base (small_morph_hint, C.left_adjoint b_obj morph) )
        | Compose (f_morph_hint, g_morph_hint) ->
          let mid_pp, mid, f_morph_hint_adj =
            left_adjoint pp b_obj f_morph_hint
          in
          let src_pp, src, g_morph_hint_adj =
            left_adjoint mid_pp mid g_morph_hint
          in
          src_pp, src, Compose (g_morph_hint_adj, f_morph_hint_adj)

      let rec right_adjoint : type a b r.
          H.Pinpoint.t ->
          b C.obj ->
          (a, b, allowed * r) t ->
          H.Pinpoint.t * a C.obj * (b, a, disallowed * allowed) t =
       fun pp b_obj -> function
        | Id -> pp, b_obj, Id
        | Base (small_morph_hint, morph) ->
          let pp, small_morph_hint =
            H.Morph.right_adjoint pp small_morph_hint
          in
          ( pp,
            C.src b_obj morph,
            Base (small_morph_hint, C.right_adjoint b_obj morph) )
        | Compose (f_morph_hint, g_morph_hint) ->
          let mid_pp, mid, f_morph_hint_adj =
            right_adjoint pp b_obj f_morph_hint
          in
          let src_pp, src, g_morph_hint_adj =
            right_adjoint mid_pp mid g_morph_hint
          in
          src_pp, src, Compose (g_morph_hint_adj, f_morph_hint_adj)

      include Magic_allow_disallow (struct
        type ('a, 'b, 'd) sided = ('a, 'b, 'd) t constraint 'd = 'l * 'r

        let rec allow_left : type l a b r.
            (a, b, allowed * r) t -> (a, b, l * r) t =
         fun h ->
          match h with
          | Id -> Id
          | Base (morph_hint, morph) ->
            Base (H.Morph.allow_left morph_hint, C.allow_left morph)
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (allow_left a_morph_hint, allow_left b_morph_hint)

        let rec allow_right : type a b l r.
            (a, b, l * allowed) t -> (a, b, l * r) t =
         fun h ->
          match h with
          | Id -> Id
          | Base (morph_hint, morph) ->
            Base (H.Morph.allow_right morph_hint, C.allow_right morph)
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (allow_right a_morph_hint, allow_right b_morph_hint)

        let rec disallow_left : type a b l r.
            (a, b, l * r) t -> (a, b, disallowed * r) t =
         fun h ->
          match h with
          | Id -> Id
          | Base (morph_hint, morph) ->
            Base (H.Morph.disallow_left morph_hint, C.disallow_left morph)
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (disallow_left a_morph_hint, disallow_left b_morph_hint)

        let rec disallow_right : type a b l r.
            (a, b, l * r) t -> (a, b, l * disallowed) t =
         fun h ->
          match h with
          | Id -> Id
          | Base (morph_hint, morph) ->
            Base (H.Morph.disallow_right morph_hint, C.disallow_right morph)
          | Compose (a_morph_hint, b_morph_hint) ->
            Compose (disallow_right a_morph_hint, disallow_right b_morph_hint)
      end)

      let rec populate : type b a l r.
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

    (** This is for removing compositions. This function doesn't contain any
        [assert false] as it is just for a straightforward transformation *)
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

  type key = Key : 'b C.obj * int * ('a, 'b, 'd) C.morph -> key

  module VarMap = Map.Make (struct
    type t = key

    let compare (Key (obj1, id1, m1)) (Key (obj2, id2, m2)) =
      let c = Int.compare id1 id2 in
      if c <> 0
      then c
      else
        match C.compare_obj obj1 obj2 with
        | Less_than -> -1
        | Greater_than -> 1
        | Equal -> Misc.comparison_result (C.compare_morph obj1 m1 m2)
  end)

  (** Map the function to the list, and returns the first [Error] found; Returns
      [Ok ()] if no error. *)
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
    { level : int;
          (** The level of the variable. This has the same meaning as the level
              field of a [type_expr]. *)
      mutable vlower : 'a lmorphvar VarMap.t;
          (** A list of variables directly under the current variable. Each is a
              pair [f] [v], and we have [f v <= u] where [u] is the current
              variable. TODO: consider using hashset for quicker deduplication
          *)
      mutable vupper : 'a rmorphvar VarMap.t;
          (** A list of variables directly above the current variable. Each is a
              pair [f] [v], and we have [u <= f v] where [u] is the current
              variable. *)
      mutable lower : 'a;
          (** The *conservative* lower bound of the variable. Why conservative:
              if a user calls [submode c u] where [c] is some constant and [u]
              some variable, we can modify [u.lower] of course. Idealy we should
              also modify all [v.lower] where [v] is variable above [u].
              However, variables at a higher [level] are not reachable from [u].
              Therefore, the [lower] of higher variables are not updated
              immediately, hence conservative. Those [lower] of higher variables
              can be made precise later on demand, see [zap_to_floor_var_aux].
          *)
      mutable lower_hint : ('a, left_only) Comp_hint.t;
          (** Hints for [lower] *)
      mutable upper : 'a;
          (** The conservative upper bound of the variable. See [lower] to
              understand why the bound is conservative *)
      mutable upper_hint : ('a, right_only) Comp_hint.t;
          (** Hints for [upper] *)
      (* To summarize, INVARIANT:
         - For any variable [v], we have [v.lower <= v.upper].
         - Variables that have been fully constrained will have
         [v.lower = v.upper]. Note that adding a boolean field indicating that
         won't help much.
         - For any [v] and [f u \in v.vlower], [u.level <= v.level]
         - For any [v] and [f u \in v.vupper], [u.level < v.level]
         - For any [v] and [f u \in v.vlower], we have [f u.upper <= v.upper],
           but not necessarily [f u.lower <= v.lower].
         - For any [v] and [f u \in v.vupper], we have [v.lower <= f u.lower],
           but not necessarily [v.upper <= f u.upper].
         - for all [f u \in v.vlower] and [g w \in v.vupper] we
           have either [g'f \in w.vlower] or [f'g \in u.vupper]. *)
      id : int  (** For identification/printing *)
    }

  and 'b lmorphvar = ('b, left_only) morphvar

  and 'b rmorphvar = ('b, right_only) morphvar

  and ('b, 'd) morphvar =
    | Amorphvar :
        'a var * ('a, 'b, 'd) C.morph * ('a, 'b, 'd) Comp_hint.Morph_hint.t
        -> ('b, 'd) morphvar
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  type anyvar = Var : 'a var -> anyvar [@@unboxed]

  let get_key dst (Amorphvar (v, m, _)) = Key (dst, v.id, m)

  module VarSet = Set.Make (Int)

  type change =
    | Cupper : 'a var * 'a * ('a, right_only) Comp_hint.t -> change
    | Clower : 'a var * 'a * ('a, left_only) Comp_hint.t -> change
    | Cvlower : 'a var * 'a lmorphvar VarMap.t -> change
    | Cvupper : 'a var * 'a rmorphvar VarMap.t -> change

  type changes = change list

  let undo_change = function
    | Cupper (v, upper, upper_hint) ->
      v.upper <- upper;
      v.upper_hint <- upper_hint
    | Clower (v, lower, lower_hint) ->
      v.lower <- lower;
      v.lower_hint <- lower_hint
    | Cvlower (v, vlower) -> v.vlower <- vlower
    | Cvupper (v, vupper) -> v.vupper <- vupper

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
        (** [Amodejoin a c [mv0, mv1, ..]] represents
            [a join mv0 join mv1 join ..] with the hint [c] for [a] (the
            morphvars have their own hints). *)
    | Amodemeet :
        'a
        * ('a, disallowed * 'r) Comp_hint.t
        * ('a, disallowed * 'r) morphvar VarMap.t
        -> ('a, disallowed * 'r) mode
        (** [Amodemeet a c [mv0, mv1, ..]] represents
            [a meet mv0 meet mv1 meet ..] with the hint [c] for [a] (the
            morphvars have their own hints). *)
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  (** Prints a mode variable, including the set of variables related to it
      (recursively). To handle cycles, [traversed] is the set of variables that
      we have already printed and will be skipped. An example of cycle:

      Consider a lattice A containing three elements 0, 1, and 2 with the linear
      lattice structure: 0 < 1 < 2. Furthermore, we define a morphism
      {v
      f : A -> A
      f 0 = 0
      f 1 = 2
      f 2 = 2
      v}

      Note that f has a left adjoint, which allows us to write f on the LHS of
      submode. Say we create a unconstrained variable [x], and invoke submode:
      [f x <= x] this would result in adding (f, x) into the [vlower] of [x].
      That is, there will be a self-loop on [x]. *)
  let rec print_var : type a. ?traversed:VarSet.t -> a C.obj -> _ -> a var -> _
      =
   fun ?traversed obj ppf v ->
    Fmt.fprintf ppf "modevar#%x<%x>[%a .. %a]" v.id v.level (C.print obj)
      v.lower (C.print obj) v.upper;
    match traversed with
    | None -> ()
    | Some traversed ->
      if VarSet.mem v.id traversed
      then ()
      else if (not (VarMap.is_empty v.vlower)) || not (VarMap.is_empty v.vupper)
      then begin
        let traversed = VarSet.add v.id traversed in
        Fmt.fprintf ppf "{%a--%a}"
          (Fmt.pp_print_list (print_morphvar ~traversed obj))
          (var_map_to_list v.vlower)
          (Fmt.pp_print_list (print_morphvar ~traversed obj))
          (var_map_to_list v.vupper)
      end

  and print_morphvar : type a l r.
      ?traversed:VarSet.t -> a C.obj -> _ -> (a, l * r) morphvar -> _ =
   fun ?traversed dst ppf (Amorphvar (v, f, _)) ->
    let src = C.src dst f in
    Fmt.fprintf ppf "%a(%a)" (C.print_morph dst) f (print_var ?traversed src) v

  let print_raw : type a l r.
      ?verbose:bool -> a C.obj -> Fmt.formatter -> (a, l * r) mode -> unit =
   fun ?(verbose = false) (obj : a C.obj) ppf m ->
    let traversed = if verbose then Some VarSet.empty else None in
    match m with
    | Amode (a, _, _) -> C.print obj ppf a
    | Amodevar mv -> print_morphvar ?traversed obj ppf mv
    | Amodejoin (a, _, mvs) ->
      Fmt.fprintf ppf "join(%a,%a)" (C.print obj) a
        (Fmt.pp_print_list
           ~pp_sep:(fun ppf () -> Fmt.fprintf ppf ",")
           (print_morphvar ?traversed obj))
        (var_map_to_list mvs)
    | Amodemeet (a, _, mvs) ->
      Fmt.fprintf ppf "meet(%a,%a)" (C.print obj) a
        (Fmt.pp_print_list
           ~pp_sep:(fun ppf () -> Fmt.fprintf ppf ",")
           (print_morphvar ?traversed obj))
        (var_map_to_list mvs)

  module Morphvar = Magic_allow_disallow (struct
    type ('a, _, 'd) sided = ('a, 'd) morphvar constraint 'd = 'l * 'r

    let allow_left : type a l r.
        (a, allowed * r) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m, h) ->
        Amorphvar (v, C.allow_left m, Comp_hint.Morph_hint.allow_left h)

    let allow_right : type a l r.
        (a, l * allowed) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m, h) ->
        Amorphvar (v, C.allow_right m, Comp_hint.Morph_hint.allow_right h)

    let disallow_left : type a l r.
        (a, l * r) morphvar -> (a, disallowed * r) morphvar = function
      | Amorphvar (v, m, h) ->
        Amorphvar (v, C.disallow_left m, Comp_hint.Morph_hint.disallow_left h)

    let disallow_right : type a l r.
        (a, l * r) morphvar -> (a, l * disallowed) morphvar = function
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

  let check_level_morphvar : type a l r. (a, l * r) morphvar -> int -> bool =
   fun (Amorphvar (v, _, _)) i -> v.level = i

  let check_level : type a l r. (a, l * r) mode -> int -> bool =
   fun m i ->
    match m with
    | Amode _ -> true
    | Amodevar mv -> check_level_morphvar mv i
    | Amodemeet (_, _, mvs) ->
      VarMap.fold (fun _ mv acc -> acc && check_level_morphvar mv i) mvs true
    | Amodejoin (_, _, mvs) ->
      VarMap.fold (fun _ mv acc -> acc && check_level_morphvar mv i) mvs true

  let check_level_var : type a l r. (a, l * r) mode -> int -> bool =
   fun m i ->
    match m with
    | Amode _ -> false
    | Amodevar mv -> check_level_morphvar mv i
    | Amodemeet (_, _, mvs) ->
      VarMap.fold
        (fun _ mv acc -> acc && check_level_morphvar mv i)
        mvs
        (not (VarMap.is_empty mvs))
    | Amodejoin (_, _, mvs) ->
      VarMap.fold
        (fun _ mv acc -> acc && check_level_morphvar mv i)
        mvs
        (not (VarMap.is_empty mvs))

  let apply_morphvar dst morph morph_hint (Amorphvar (var, morph', morph'_hint))
      =
    Amorphvar
      (var, C.compose dst morph morph', Compose (morph_hint, morph'_hint))

  let apply : type a b l r.
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
      let vs =
        VarMap.fold
          (fun _ mv acc ->
            let mv = apply_morphvar dst morph hint mv in
            VarMap.add (get_key dst mv) mv acc)
          vs VarMap.empty
      in
      Amodejoin (C.apply dst morph a, Apply (hint, a_hint), vs)
    | Amodemeet (a, a_hint, vs) ->
      let hint = Comp_hint.Morph_hint.disallow_left hint in
      let vs =
        VarMap.fold
          (fun _ mv acc ->
            let mv = apply_morphvar dst morph hint mv in
            VarMap.add (get_key dst mv) mv acc)
          vs VarMap.empty
      in
      Amodemeet (C.apply dst morph a, Apply (hint, a_hint), vs)

  module Unhint = struct
    type ('a, 'd) t =
      | Unhint : ('b, 'a, 'd) C.morph * ('b, 'd) mode -> ('a, 'd) t
      constraint 'd = 'l * 'r
    [@@ocaml.warning "-62"]

    let unhint : type a l r. (a, l * r) mode -> (a, l * r) t =
     fun m -> Unhint (C.id, m)

    let hint : type a l r.
        a C.obj -> ?hint:(l * r) H.Morph.t -> (a, l * r) t -> (a, l * r) mode =
     fun dst ?hint (Unhint (morph, m)) -> apply dst ?hint morph m

    let apply : type a b l r.
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

  (** Calling [update_lower ~log obj v a a_hint] assumes that
      [not (a <= v.lower)]. Arguments are not checked and used directly. They
      must satisfy the INVARIANT listed above. *)
  let update_lower (type a) ~log (obj : a C.obj) v a a_hint =
    (match log with
    | None -> ()
    | Some log -> log := Clower (v, v.lower, v.lower_hint) :: !log);
    v.lower <- C.join obj v.lower a;
    v.lower_hint <- hint_biased_join obj a a_hint v.lower v.lower_hint

  (** Calling [update_upper ~log obj v a a_hint] assumes that
      [not (v.upper <= a)]. Arguments are not checked and used directly. They
      must satisfy the INVARIANT listed above. *)
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

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let set_vupper ~log v vupper =
    (match log with
    | None -> ()
    | Some log -> log := Cvupper (v, v.vupper) :: !log);
    v.vupper <- vupper

  (** Returns [Ok ()] if success; [Error x] if failed, and [x] is the next best
      (read: strictly lower) guess to replace the constant argument that MIGHT
      succeed. *)
  let rec submode_cv : type a.
      log:_ ->
      H.Pinpoint.t ->
      a C.obj ->
      a ->
      (a, left_only) Comp_hint.t ->
      a var ->
      (unit, a * (a, right_only) Comp_hint.t) Result.t =
   fun (type a) ~log pp (obj : a C.obj) a' a'_hint v ->
    if C.le obj a' v.lower
    then Ok ()
    else if not (C.le obj a' v.upper)
    then Error (v.upper, v.upper_hint)
    else (
      update_lower ~log obj v a' a'_hint;
      let r =
        v.vupper
        |> find_error (fun mu ->
            let r = submode_cmv ~log pp obj a' a'_hint mu in
            (if Result.is_ok r && VarMap.is_empty v.vlower
             then
               (* Optimization: update [v.upper] based on [mupper u].*)
               let mu_upper = mupper obj mu in
               if not (C.le obj mu_upper v.upper)
               then update_upper ~log obj v mu_upper (mupper_hint mu));
            r)
      in
      if C.le obj v.upper v.lower
      then begin
        set_vlower ~log v VarMap.empty;
        set_vupper ~log v VarMap.empty
      end;
      r)

  and submode_cmv : type a l.
      log:_ ->
      H.Pinpoint.t ->
      a C.obj ->
      a ->
      (a, left_only) Comp_hint.t ->
      (a, l * allowed) morphvar ->
      (unit, a * (a, right_only) Comp_hint.t) Result.t =
   fun ~log pp obj a a_hint (Amorphvar (v, f, f_hint) as mv) ->
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
      let src_pp, src, f'_hint =
        Comp_hint.Morph_hint.left_adjoint pp obj f_hint
      in
      let a' = C.apply src f' a in
      let a'_hint = Comp_hint.Apply (f'_hint, a_hint) in
      match submode_cv ~log src_pp src a' a'_hint v with
      | Ok () -> Ok ()
      | Error (e, e_hint) ->
        Error
          ( C.apply obj f e,
            Comp_hint.Apply (Comp_hint.Morph_hint.disallow_left f_hint, e_hint)
          )

  (** Returns [Ok ()] if success; [Error x] if failed, and [x] is the next best
      (read: strictly higher) guess to replace the constant argument that MIGHT
      succeed. *)
  let rec submode_vc : type a.
      log:_ ->
      H.Pinpoint.t ->
      a C.obj ->
      a var ->
      a ->
      (a, right_only) Comp_hint.t ->
      (unit, a * (a, left_only) Comp_hint.t) Result.t =
   fun (type a) ~log pp (obj : a C.obj) v a' a'_hint ->
    if C.le obj v.upper a'
    then Ok ()
    else if not (C.le obj v.lower a')
    then Error (v.lower, v.lower_hint)
    else (
      update_upper ~log obj v a' a'_hint;
      let r =
        v.vlower
        |> find_error (fun mu ->
            let r = submode_mvc ~log pp obj mu a' a'_hint in
            (if Result.is_ok r && VarMap.is_empty v.vupper
             then
               (* Optimization: update [v.lower] based on [mlower u].*)
               let mu_lower = mlower obj mu in
               let mu_lower_hint = mlower_hint mu in
               if not (C.le obj mu_lower v.lower)
               then update_lower ~log obj v mu_lower mu_lower_hint);
            r)
      in
      if C.le obj v.upper v.lower
      then begin
        set_vlower ~log v VarMap.empty;
        set_vupper ~log v VarMap.empty
      end;
      r)

  and submode_mvc :
      'a 'r.
      log:change list ref option ->
      H.Pinpoint.t ->
      'a C.obj ->
      ('a, allowed * 'r) morphvar ->
      'a ->
      ('a, right_only) Comp_hint.t ->
      (unit, 'a * ('a, left_only) Comp_hint.t) Result.t =
   fun ~log pp obj (Amorphvar (v, f, f_hint) as mv) a a_hint ->
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
      let src_pp, src, f'_hint =
        Comp_hint.Morph_hint.right_adjoint pp obj f_hint
      in
      let a' = C.apply src f' a in
      let a'_hint = Comp_hint.Apply (f'_hint, a_hint) in
      (* If [mlower] was precise, then the check
         [not (C.le obj (mlower obj mv) a)] should guarantee the following call
         to return [Ok ()]. However, [mlower] is not precise *)
      (* not using [Result.map_error] to avoid allocating closure *)
      match submode_vc ~log src_pp src v a' a'_hint with
      | Ok () -> Ok ()
      | Error (e, e_hint) ->
        Error
          ( C.apply obj f e,
            Apply (Comp_hint.Morph_hint.disallow_right f_hint, e_hint) )

  let eq_morphvar : type a l0 r0 l1 r1.
      a C.obj -> (a, l0 * r0) morphvar -> (a, l1 * r1) morphvar -> bool =
   fun dst (Amorphvar (v0, f0, _f0_hint) as mv0)
       (Amorphvar (v1, f1, _f1_hint) as mv1) ->
    (* To align l0/l1, r0/r1; The existing disallow_left/right] is for [mode],
       not [morphvar]. *)
    Morphvar.(
      disallow_left (disallow_right mv0) == disallow_left (disallow_right mv1))
    ||
    match C.equal_morph dst f0 f1 with
    | None -> false
    | Some Refl -> v0 == v1

  let rec submode_mvmv : type a l r.
      log:_ ->
      H.Pinpoint.t ->
      a C.obj ->
      (a, allowed * r) morphvar ->
      (a, l * allowed) morphvar ->
      (_, a * (a, _) Comp_hint.t * a * (a, _) Comp_hint.t) result =
   fun ~log pp dst (Amorphvar (v, f, f_hint) as mv)
       (Amorphvar (u, g, g_hint) as mu) ->
    if C.le dst (mupper dst mv) (mlower dst mu)
    then Ok ()
    else if eq_morphvar dst mv mu
    then Ok ()
    else
      let muupper = mupper dst mu in
      let muupper_hint = mupper_hint mu in
      (* The call f v <= g u translates to the following step:
         1. f v <= g u.upper
         2. f v.lower <= g u
         3. If v.level <= u.level, for all (h w) in u.vupper, g' (f v) <= h w
         4. If v.level <= u.level adding g' (f v) to u.vlower, where g' is the left adjoint of g
         5. If v.level > u.level, for all (h w) in v.vlower, h w <= f' (g u)
         6. If v.level > u.level adding f' (g u) to v.vupper, where f' is the right adjoint of f
         Steps 3 and 4 are implemented by [add_vlower], steps 5 and 6 by [add_vupper].
      *)
      match submode_mvc ~log pp dst mv muupper muupper_hint with
      | Error (a, a_hint) -> Error (a, a_hint, muupper, muupper_hint)
      | Ok () -> (
        let mvlower = mlower dst mv in
        let mvlower_hint = mlower_hint mv in
        match submode_cmv ~log pp dst mvlower mvlower_hint mu with
        | Error (a, a_hint) -> Error (mvlower, mvlower_hint, a, a_hint)
        | Ok () ->
          if v.level <= u.level
          then add_vlower ~log pp dst v f f_hint mv u g g_hint
          else add_vupper ~log pp dst v f f_hint u g g_hint mu)

  (* Add a vlower entry for the relationship [f v <= g u] if necessary. *)
  and add_vlower : type a b c l r.
      log:_ ->
      H.Pinpoint.t ->
      b C.obj ->
      a var ->
      (a, b, allowed * r) C.morph ->
      (a, b, allowed * r) Comp_hint.Morph_hint.t ->
      (b, allowed * r) morphvar ->
      c var ->
      (c, b, l * allowed) C.morph ->
      (c, b, l * allowed) Comp_hint.Morph_hint.t ->
      (_, b * (b, _) Comp_hint.t * b * (b, _) Comp_hint.t) result =
   fun ~log pp dst v f f_hint mv u g g_hint ->
    let g' = C.left_adjoint dst g in
    let _, src, g'_hint = Comp_hint.Morph_hint.left_adjoint pp dst g_hint in
    let g'f = C.compose src g' (C.disallow_right f) in
    let g'f_hint =
      Comp_hint.Morph_hint.Compose
        (g'_hint, Comp_hint.Morph_hint.disallow_right f_hint)
    in
    let x = Amorphvar (v, g'f, g'f_hint) in
    let key = get_key src x in
    if VarMap.mem key u.vlower
    then Ok ()
    else begin
      set_vlower ~log u (VarMap.add key x u.vlower);
      find_error
        (fun (Amorphvar (w, h, h_hint)) ->
          let gh = C.compose dst (C.disallow_left g) h in
          let gh_hint =
            Comp_hint.Morph_hint.Compose
              (Comp_hint.Morph_hint.disallow_left g_hint, h_hint)
          in
          let y = Amorphvar (w, gh, gh_hint) in
          submode_mvmv ~log pp dst mv y)
        u.vupper
    end

  (* Add a vupper entry for the relationship [f v <= g u] if necessary. *)
  and add_vupper : type a b c l r.
      log:_ ->
      H.Pinpoint.t ->
      b C.obj ->
      a var ->
      (a, b, allowed * r) C.morph ->
      (a, b, allowed * r) Comp_hint.Morph_hint.t ->
      c var ->
      (c, b, l * allowed) C.morph ->
      (c, b, l * allowed) Comp_hint.Morph_hint.t ->
      (b, l * allowed) morphvar ->
      (_, b * (b, _) Comp_hint.t * b * (b, _) Comp_hint.t) result =
   fun ~log pp dst v f f_hint u g g_hint mu ->
    let f' = C.right_adjoint dst f in
    let _, src, f'_hint = Comp_hint.Morph_hint.right_adjoint pp dst f_hint in
    let f'g = C.compose src f' (C.disallow_left g) in
    let f'g_hint =
      Comp_hint.Morph_hint.Compose
        (f'_hint, Comp_hint.Morph_hint.disallow_left g_hint)
    in
    let x = Amorphvar (u, f'g, f'g_hint) in
    let key = get_key src x in
    if VarMap.mem key v.vupper
    then Ok ()
    else begin
      set_vupper ~log v (VarMap.add key x v.vupper);
      find_error
        (fun (Amorphvar (w, h, h_hint)) ->
          let fh = C.compose dst (C.disallow_right f) h in
          let fh_hint =
            Comp_hint.Morph_hint.Compose
              (Comp_hint.Morph_hint.disallow_right f_hint, h_hint)
          in
          let y = Amorphvar (w, fh, fh_hint) in
          submode_mvmv ~log pp dst y mu)
        v.vlower
    end

  let vars = ref (0, [])

  let debug_modes = ref true

  let fresh ?upper ?upper_hint ?lower ?lower_hint ?vlower ?vupper ~level obj =
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
    let vupper = Option.value vupper ~default:VarMap.empty in
    let level = if !debug_modes then id mod 5 else level in
    let var =
      { level; upper; upper_hint; lower; lower_hint; vlower; vupper; id }
    in
    vars := id + 1, Var var :: l;
    var

  let unhint_morphvar (Amorphvar (v, f, _)) =
    Amorphvar (v, f, Comp_hint.Morph_hint.Base (H.Morph.unknown, f))

  let unhint_var v =
    v.upper_hint <- Comp_hint.Unknown v.upper;
    v.lower_hint <- Comp_hint.Unknown v.lower;
    v.vlower <- VarMap.map unhint_morphvar v.vlower;
    v.vupper <- VarMap.map unhint_morphvar v.vupper

  let erase_hints () =
    let _, l = !vars in
    List.iter (fun (Var v) -> unhint_var v) l

  type ('a, 'd) hint_raw = ('a, 'd) Comp_hint.t

  type 'a error_raw =
    { left : 'a;
      left_hint : ('a, left_only) hint_raw;
      right : 'a;
      right_hint : ('a, right_only) hint_raw
    }

  let submode (type a r l) (pp : H.Pinpoint.t) (obj : a C.obj)
      (a : (a, allowed * r) mode) (b : (a, l * allowed) mode) ~log =
    let submode_cc ~log:_ _pp obj left left_hint right right_hint =
      if C.le obj left right
      then Ok ()
      else Error { left; left_hint; right; right_hint }
    in
    let submode_mvc ~log pp obj v right right_hint =
      Result.map_error
        (fun (left, left_hint) -> { left; left_hint; right; right_hint })
        (submode_mvc ~log pp obj v right right_hint)
    in
    let submode_cmv ~log pp obj left left_hint v =
      Result.map_error
        (fun (right, right_hint) -> { left; left_hint; right; right_hint })
        (submode_cmv ~log pp obj left left_hint v)
    in
    let submode_mvmv ~log pp obj v u =
      Result.map_error
        (fun (left, left_hint, right, right_hint) ->
          { left; left_hint; right; right_hint })
        (submode_mvmv ~log pp obj v u)
    in
    match a, b with
    | ( Amode (left, left_hint_lower, _left_hint_upper),
        Amode (right, _right_hint_lower, right_hint_upper) ) ->
      submode_cc ~log pp obj left
        (Comp_hint.disallow_right left_hint_lower)
        right
        (Comp_hint.disallow_left right_hint_upper)
    | Amodevar v, Amode (right, _right_hint_lower, right_hint_upper) ->
      submode_mvc ~log pp obj v right (Comp_hint.disallow_left right_hint_upper)
    | Amode (left, left_hint_lower, _left_hint_upper), Amodevar v ->
      submode_cmv ~log pp obj left (Comp_hint.disallow_right left_hint_lower) v
    | Amodevar v, Amodevar u -> submode_mvmv ~log pp obj v u
    | Amode (a, a_hint_lower, _a_hint_upper), Amodemeet (b, b_hint, mvs) ->
      Result.bind
        (submode_cc ~log pp obj a
           (Comp_hint.disallow_right a_hint_lower)
           b b_hint)
        (fun () ->
          find_error
            (fun mv ->
              submode_cmv ~log pp obj a
                (Comp_hint.disallow_right a_hint_lower)
                mv)
            mvs)
    | Amodevar mv, Amodemeet (b, b_hint, mvs) ->
      Result.bind (submode_mvc ~log pp obj mv b b_hint) (fun () ->
          find_error (fun mv' -> submode_mvmv ~log pp obj mv mv') mvs)
    | Amodejoin (a, a_hint, mvs), Amode (b, _b_hint_lower, b_hint_upper) ->
      Result.bind
        (submode_cc ~log pp obj a a_hint b
           (Comp_hint.disallow_left b_hint_upper))
        (fun () ->
          find_error
            (fun mv' ->
              submode_mvc ~log pp obj mv' b
                (Comp_hint.disallow_left b_hint_upper))
            mvs)
    | Amodejoin (a, a_hint, mvs), Amodevar mv ->
      Result.bind (submode_cmv ~log pp obj a a_hint mv) (fun () ->
          find_error (fun mv' -> submode_mvmv ~log pp obj mv' mv) mvs)
    | Amodejoin (a, a_hint, mvs), Amodemeet (b, b_hint, mus) ->
      (* TODO: mabye create a intermediate variable? *)
      Result.bind (submode_cc ~log pp obj a a_hint b b_hint) (fun () ->
          Result.bind
            (find_error (fun mv -> submode_mvc ~log pp obj mv b b_hint) mvs)
            (fun () ->
              Result.bind
                (find_error (fun mu -> submode_cmv ~log pp obj a a_hint mu) mus)
                (fun () ->
                  find_error
                    (fun mu ->
                      find_error (fun mv -> submode_mvmv ~log pp obj mv mu) mvs)
                    mus)))

  let populate_hint obj a hint =
    let ahint = Comp_hint.populate obj hint in
    assert (Misc.Le_result.equal ~le:(C.le obj) (fst ahint) a);
    ahint

  let populate_error obj { left; left_hint; right; right_hint } =
    let left = populate_hint obj left left_hint in
    let right = populate_hint obj right right_hint in
    { left; right }

  let add_morphvar dst x xs = VarMap.add (get_key dst x) x xs

  let union_morphvars t0 t1 = VarMap.union (fun _ a _b -> Some a) t0 t1

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
              (add_morphvar obj mv mvs) xs
          | Amodejoin (b, b_hint, mvs') ->
            loop (C.join obj a b)
              (hint_join obj a a_hint_lower b b_hint)
              (union_morphvars mvs' mvs) xs)
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
              (add_morphvar obj mv mvs) xs
          | Amodemeet (b, b_hint, mvs') ->
            loop (C.meet obj a b)
              (hint_meet obj a a_hint_upper b b_hint)
              (union_morphvars mvs' mvs) xs)
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

  (** Zap [mv] to its lower bound. Returns the [log] of the zapping, in case the
      caller are only interested in the lower bound and wants to reverse the
      zapping.

      As mentioned in [var], [mlower mv] is not precise; to get the precise
      lower bound of [mv], we call [submode mv (mlower mv)]. This will propagate
      to all its children, which might fail because some children's lower bound
      [a] is more up-to-date than [mv]. In that case, we call [submode mv a]. We
      repeat this process until no failure, and we will get the precise lower
      bound.

      The loop is guaranteed to terminate, because for each iteration our
      guessed lower bound is strictly higher; and all lattices are finite. *)
  let zap_to_floor_morphvar_aux (type a r) (obj : a C.obj)
      (mv : (a, allowed * r) morphvar) =
    let rec loop lower =
      let log = ref empty_changes in
      (* We want a hint for why [lower] is low, but we only have hint for why [lower] is
         high. There is no good hint to use. *)
      let r =
        submode_mvc ~log:(Some log) H.Pinpoint.unknown obj mv lower
          (Unknown lower)
      in
      match r with
      | Ok () -> !log, lower
      | Error (a, _) ->
        undo_changes !log;
        loop (C.join obj a lower)
    in
    loop (mlower obj mv)

  (** Zap [mv] to its upper bound. Returns the [log] of the zapping, in case the
      caller are only interested in the lower bound and wants to reverse the
      zapping.

      [mupper mv] is not precise; to get the precise upper bound of [mv], we
      call [submode (mupper mv) mv ]. This will propagate to all its children,
      which might fail because some children's upper bound [a] is more
      up-to-date than [mv]. In that case, we call [submode a mv]. We repeat this
      process until no failure, and we will get the precise lower bound.

      The loop is guaranteed to terminate, because for each iteration our
      guessed lower bound is strictly higher; and all lattices are finite. *)
  let zap_to_ceil_morphvar_aux (type a l) (obj : a C.obj)
      (mv : (a, l * allowed) morphvar) =
    let rec loop upper =
      let log = ref empty_changes in
      let r =
        submode_cmv ~log:(Some log) H.Pinpoint.unknown obj upper (Unknown upper)
          mv
      in
      match r with
      | Ok () -> !log, upper
      | Error (a, _) ->
        undo_changes !log;
        loop (C.meet obj a upper)
    in
    loop (mupper obj mv)

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
          submode_mvc H.Pinpoint.unknown obj mv floor (Unknown floor) ~log
          |> Result.get_ok)
        mvs;
      floor

  (** Zaps a morphvar to its ceiling and returns the ceiling. [commit] could be
      [Some log], in which case the zapping is appended to [log]; it could also
      be [None], in which case the zapping is reverted. The latter is useful
      when the caller only wants to know the ceiling without zapping. *)
  let zap_to_ceil_morphvar obj mv ~commit =
    let log_, upper = zap_to_ceil_morphvar_aux obj mv in
    (match commit with
    | None -> undo_changes log_
    | Some log -> log := append_changes !log log_);
    upper

  let zap_to_ceil : type a l. a C.obj -> (a, l * allowed) mode -> log:_ -> a =
   fun obj m ~log ->
    match m with
    | Amode (a, _a_hint_lower, _a_hint_upper) -> a
    | Amodevar mv -> zap_to_ceil_morphvar obj mv ~commit:log
    | Amodemeet (a, _a_hint, mvs) ->
      let ceil =
        VarMap.fold
          (fun _ mv acc ->
            C.meet obj acc (zap_to_ceil_morphvar obj mv ~commit:None))
          mvs a
      in
      VarMap.iter
        (fun _ mv ->
          let ok =
            submode_cmv H.Pinpoint.unknown obj ceil (Unknown ceil) mv ~log
          in
          assert (Result.is_ok ok))
        mvs;
      ceil

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

  let get_ceil : type a l. a C.obj -> (a, l * allowed) mode -> a =
   fun obj m ->
    match m with
    | Amode (a, _a_hint_lower, _a_hint_upper) -> a
    | Amodevar mv -> zap_to_ceil_morphvar obj mv ~commit:None
    | Amodemeet (a, _a_hint, mvs) ->
      VarMap.fold
        (fun _ mv acc ->
          C.meet obj acc (zap_to_ceil_morphvar obj mv ~commit:None))
        mvs a

  let to_const_exn obj m =
    let floor = get_floor obj m in
    let ceil = get_ceil obj m in
    if C.le obj ceil floor
    then ceil
    else
      Misc.fatal_errorf "mode is not tight: floor = %a, ceil = %a"
        (Fmt.compat (C.print obj))
        floor
        (Fmt.compat (C.print obj))
        ceil

  let print : type a l r.
      ?verbose:bool -> a C.obj -> Fmt.formatter -> (a, l * r) mode -> unit =
   fun ?verbose (obj : a C.obj) ppf m ->
    let ceil = get_loose_ceil obj m in
    let floor = get_loose_floor obj m in
    if C.le obj ceil floor
    then C.print obj ppf ceil
    else print_raw ?verbose obj ppf m

  let newvar obj level =
    let u = fresh ~level obj in
    Amodevar (Amorphvar (u, C.id, Id))

  let newvar_above (type a r) (obj : a C.obj) (level : int)
      (m : (a, allowed * r) mode) =
    match disallow_right m with
    | Amode (a, a_hint_lower, _a_hint_upper) ->
      if C.le obj (C.max obj) a
      then Amode (a, Comp_hint.allow_left a_hint_lower, Max), false
      else
        ( Amodevar
            (Amorphvar
               ( fresh ~lower:a
                   ~lower_hint:(Comp_hint.disallow_right a_hint_lower)
                   ~level obj,
                 C.id,
                 Id )),
          true )
    | Amodevar (Amorphvar (v, _, _) as mv) ->
      if v.level <= level
      then
        (* [~lower] is not precise (because [mlower mv] is not precise), but
         it doesn't need to be *)
        ( Amodevar
            (Amorphvar
               ( fresh ~lower:(mlower obj mv) ~lower_hint:(mlower_hint mv)
                   ~vlower:(VarMap.singleton (get_key obj mv) mv)
                   ~level obj,
                 C.id,
                 Id )),
          true )
      else
        let u = fresh ~level obj in
        let mu = Amorphvar (u, C.id, Id) in
        let ok = submode_mvmv ~log:None H.Pinpoint.unknown obj mv mu in
        assert (Result.is_ok ok);
        allow_right (Amodevar mu), true
    | Amodejoin (a, a_hint, mvs) ->
      if VarMap.for_all (fun _ (Amorphvar (v, _, _)) -> v.level <= level) mvs
      then
        (* [~lower] is not precise here, but it doesn't need to be *)
        ( Amodevar
            (Amorphvar
               ( fresh ~lower:a ~lower_hint:a_hint ~vlower:mvs ~level obj,
                 C.id,
                 Id )),
          true )
      else
        let u = fresh ~level obj in
        let mu = Amorphvar (u, C.id, Id) in
        submode_cmv H.Pinpoint.unknown obj ~log:None a a_hint mu
        |> Result.get_ok;
        VarMap.iter
          (fun _ mv ->
            let ok = submode_mvmv ~log:None H.Pinpoint.unknown obj mv mu in
            assert (Result.is_ok ok))
          mvs;
        allow_right (Amodevar mu), true

  let newvar_below (type a l) (obj : a C.obj) (level : int)
      (m : (a, l * allowed) mode) =
    match disallow_left m with
    | Amode (a, _a_hint_lower, a_hint_upper) ->
      if C.le obj a (C.min obj)
      then Amode (a, Min, Comp_hint.allow_right a_hint_upper), false
      else
        ( Amodevar
            (Amorphvar
               ( fresh ~upper:a
                   ~upper_hint:(Comp_hint.disallow_left a_hint_upper)
                   ~level obj,
                 C.id,
                 Id )),
          true )
    | Amodevar (Amorphvar (v, _, _) as mv) ->
      if v.level < level
      then
        (* [~upper] is not precise (because [mupper mv] is not precise), but
         it doesn't need to be *)
        ( Amodevar
            (Amorphvar
               ( fresh ~upper:(mupper obj mv) ~upper_hint:(mupper_hint mv)
                   ~vupper:(VarMap.singleton (get_key obj mv) mv)
                   ~level obj,
                 C.id,
                 Id )),
          true )
      else
        let u = fresh ~level obj in
        let mu = Amorphvar (u, C.id, Id) in
        submode_mvmv H.Pinpoint.unknown obj ~log:None mu mv |> Result.get_ok;
        allow_left (Amodevar mu), true
    | Amodemeet (a, a_hint, mvs) ->
      if VarMap.for_all (fun _ (Amorphvar (v, _, _)) -> v.level < level) mvs
      then
        (* [~upper] is not precise here, but it doesn't need to be *)
        ( Amodevar
            (Amorphvar
               ( fresh ~upper:a ~upper_hint:a_hint ~vupper:mvs ~level obj,
                 C.id,
                 Id )),
          true )
      else
        let u = fresh ~level obj in
        let mu = Amorphvar (u, C.id, Id) in
        submode_mvc H.Pinpoint.unknown obj ~log:None mu a a_hint
        |> Result.get_ok;
        VarMap.iter
          (fun _ mv ->
            submode_mvmv H.Pinpoint.unknown obj ~log:None mu mv |> Result.get_ok)
          mvs;
        allow_left (Amodevar mu), true
end
[@@inline always]
