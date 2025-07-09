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

module Solver_mono (Hint : Hint) (C : Lattices_mono) = struct
  type 'a hint =
    | Morph : Hint.morph * ('b, 'a, 'd) C.morph * 'b hint -> 'a hint
    | Const : Hint.const -> 'a hint
    | Branch : 'a * 'a hint * 'a * 'a hint -> 'a hint

  type 'a error =
    { left : 'a;
      left_hint : 'a hint;
      right : 'a;
      right_hint : 'a hint
    }

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
      mutable upper_hint : 'a hint;  (** Hints for [upper] *)
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
      mutable lower_hint : 'a hint;  (** Hints for [lower] *)
      id : int  (** For identification/printing *)
    }

  and 'b lmorphvar = ('b, left_only) morphvar

  and ('b, 'd) morphvar =
    | Amorphvar :
        'a var * ('a, 'b, 'd) C.morph * Hint.morph
        -> ('b, 'd) morphvar
    constraint 'd = _ * _
  [@@ocaml.warning "-62"]

  let get_key (Amorphvar (v, m, _)) = v.id, Any_morph m

  module VarSet = Set.Make (Int)

  type change =
    | Cupper : 'a var * 'a * 'a hint -> change
    | Clower : 'a var * 'a * 'a hint -> change
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
    | Amode : 'a * 'a hint -> ('a, 'l * 'r) mode
    | Amodevar : ('a, 'd) morphvar -> ('a, 'd) mode
    | Amodejoin :
        'a * 'a hint * ('a, 'l * disallowed) morphvar VarMap.t
        -> ('a, 'l * disallowed) mode
        (** [Amodejoin a c [mv0, mv1, ..]] represents [a join mv0 join mv1 join ..] with hint [c]. *)
    | Amodemeet :
        'a * 'a hint * ('a, disallowed * 'r) morphvar VarMap.t
        -> ('a, disallowed * 'r) mode
        (** [Amodemeet a c [mv0, mv1, ..]] represents [a meet mv0 meet mv1 meet ..] with hint [c]. *)
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
    | Amode (a, _) -> C.print obj ppf a
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
      | Amorphvar (v, m, h) -> Amorphvar (v, C.allow_left m, h)

    let allow_right :
        type a l r. (a, l * allowed) morphvar -> (a, l * r) morphvar = function
      | Amorphvar (v, m, h) -> Amorphvar (v, C.allow_right m, h)

    let disallow_left :
        type a l r. (a, l * r) morphvar -> (a, disallowed * r) morphvar =
      function
      | Amorphvar (v, m, h) -> Amorphvar (v, C.disallow_left m, h)

    let disallow_right :
        type a l r. (a, l * r) morphvar -> (a, l * disallowed) morphvar =
      function
      | Amorphvar (v, m, h) -> Amorphvar (v, C.disallow_right m, h)
  end)

  include Magic_allow_disallow (struct
    type ('a, _, 'd) sided = ('a, 'd) mode constraint 'd = 'l * 'r

    let allow_left : type a l r. (a, allowed * r) mode -> (a, l * r) mode =
      function
      | Amode (c, h) -> Amode (c, h)
      | Amodevar mv -> Amodevar (Morphvar.allow_left mv)
      | Amodejoin (c, h, mvs) ->
        Amodejoin (c, h, VarMap.map Morphvar.allow_left mvs)

    let allow_right : type a l r. (a, l * allowed) mode -> (a, l * r) mode =
      function
      | Amode (c, h) -> Amode (c, h)
      | Amodevar mv -> Amodevar (Morphvar.allow_right mv)
      | Amodemeet (c, h, mvs) ->
        Amodemeet (c, h, VarMap.map Morphvar.allow_right mvs)

    let disallow_left : type a l r. (a, l * r) mode -> (a, disallowed * r) mode
        = function
      | Amode (c, h) -> Amode (c, h)
      | Amodevar mv -> Amodevar (Morphvar.disallow_left mv)
      | Amodejoin (c, h, mvs) ->
        Amodejoin (c, h, VarMap.map Morphvar.disallow_left mvs)
      | Amodemeet (c, h, mvs) ->
        Amodemeet (c, h, VarMap.map Morphvar.disallow_left mvs)

    let disallow_right : type a l r. (a, l * r) mode -> (a, l * disallowed) mode
        = function
      | Amode (c, h) -> Amode (c, h)
      | Amodevar mv -> Amodevar (Morphvar.disallow_right mv)
      | Amodejoin (c, h, mvs) ->
        Amodejoin (c, h, VarMap.map Morphvar.disallow_right mvs)
      | Amodemeet (c, h, mvs) ->
        Amodemeet (c, h, VarMap.map Morphvar.disallow_right mvs)
  end)

  let mlower dst (Amorphvar (var, morph, hint)) =
    C.apply dst morph var.lower, Morph (hint, morph, var.lower_hint)

  let mupper dst (Amorphvar (var, morph, hint)) =
    C.apply dst morph var.upper, Morph (hint, morph, var.upper_hint)

  let min (type a) (obj : a C.obj) = Amode (C.min obj, Const Hint.const_none)

  let max (type a) (obj : a C.obj) = Amode (C.max obj, Const Hint.const_none)

  let of_const _obj ?(hint = Hint.const_none) a = Amode (a, Const hint)

  let apply_morphvar dst morph morph_hint
      (Amorphvar (var, morph', _morph'_hint)) =
    Amorphvar (var, C.compose dst morph morph', morph_hint)

  let apply :
      type a b l r.
      b C.obj ->
      ?hint:Hint.morph ->
      (a, b, l * r) C.morph ->
      (a, l * r) mode ->
      (b, l * r) mode =
   fun dst ?(hint = Hint.morph_none) morph m ->
    let create_const_morph_hint h = Morph (hint, morph, h) in
    match m with
    | Amode (a, h) -> Amode (C.apply dst morph a, create_const_morph_hint h)
    | Amodevar mv -> Amodevar (apply_morphvar dst morph hint mv)
    | Amodejoin (a, h, vs) ->
      Amodejoin
        ( C.apply dst morph a,
          create_const_morph_hint h,
          VarMap.map (apply_morphvar dst morph hint) vs )
    | Amodemeet (a, h, vs) ->
      Amodemeet
        ( C.apply dst morph a,
          create_const_morph_hint h,
          VarMap.map (apply_morphvar dst morph hint) vs )

  let choose_hint_for_join_cc obj a a_hint b b_hint =
    (* This just provides a minor optimization to avoid having unnecessarily-nested Branch hints.
       When a join, [join a b], is performed, the hint we use for the result should be a [Branch]
       if both arguments are relevant, but if we have that [a < b] in the lattice structure then
       we don't really need to consider the hint for [a], and vice versa for [b]'s hint. *)
    if C.le obj a b
    then b_hint
    else if C.le obj b a
    then a_hint
    else Branch (a, a_hint, b, b_hint)

  let choose_hint_for_meet_cc obj a a_hint b b_hint =
    (* Comments for [choose_hint_for_join_cc] apply here similarly but the other way round (since this is for meet, not join) *)
    if C.le obj a b
    then a_hint
    else if C.le obj b a
    then b_hint
    else Branch (a, a_hint, b, b_hint)

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let update_lower (type a) ~log (obj : a C.obj) v a a_hint =
    (match log with
    | None -> ()
    | Some log -> log := Clower (v, v.lower, v.lower_hint) :: !log);
    let new_lower = C.join obj v.lower a in
    let new_lower_hint =
      choose_hint_for_join_cc obj v.lower v.lower_hint a a_hint
    in
    v.lower <- new_lower;
    v.lower_hint <- new_lower_hint

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let update_upper (type a) ~log (obj : a C.obj) v a a_hint =
    (match log with
    | None -> ()
    | Some log -> log := Cupper (v, v.upper, v.upper_hint) :: !log);
    let new_upper = C.meet obj v.upper a in
    let new_upper_hint =
      choose_hint_for_meet_cc obj v.upper v.upper_hint a a_hint
    in
    v.upper <- new_upper;
    v.upper_hint <- new_upper_hint

  (** Arguments are not checked and used directly. They must satisfy the
      INVARIANT listed above. *)
  let set_vlower ~log v vlower =
    (match log with
    | None -> ()
    | Some log -> log := Cvlower (v, v.vlower) :: !log);
    v.vlower <- vlower

  let submode_cv :
      type a.
      log:_ -> a C.obj -> a -> a hint -> a var -> (unit, a * a hint) Result.t =
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
      a hint ->
      (a, l * allowed) morphvar ->
      (unit, a * a hint) Result.t =
   fun ~log obj a a_hint (Amorphvar (v, f, f_hint) as mv) ->
    let mlower, _mlower_hint = mlower obj mv in
    let mupper, mupper_hint = mupper obj mv in
    if C.le obj a mlower
    then Ok ()
    else if not (C.le obj a mupper)
    then Error (mupper, mupper_hint)
    else
      (* At this point we know [a <= f v], therefore [a] is in the downward
         closure of [f]'s image. Therefore, asking [a <= f v] is equivalent to
         asking [f' a <= v]. *)
      let f' = C.left_adjoint obj f in
      let f'_hint = Hint.left_adjoint f_hint in
      let src = C.src obj f in
      let a' = C.apply src f' a in
      let a'_hint = Morph (f'_hint, f', a_hint) in
      submode_cv ~log src a' a'_hint v |> Result.get_ok;
      Ok ()

  (** Returns [Ok ()] if success; [Error x] if failed, and [x] is the next best
    (read: strictly higher) guess to replace the constant argument that MIGHT
    succeed. *)
  let rec submode_vc :
      type a.
      log:_ -> a C.obj -> a var -> a -> a hint -> (unit, a * a hint) Result.t =
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
                  let mu_lower, mu_lower_hint = mlower obj mu in
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
        'a hint ->
        (unit, 'a * 'a hint) Result.t =
   fun ~log obj (Amorphvar (v, f, f_hint) as mv) a a_hint ->
    (* See [submode_cmv] for why we need the following seemingly redundant
       lines. *)
    let mupper, _mupper_hint = mupper obj mv in
    let mlower, mlower_hint = mlower obj mv in
    if C.le obj mupper a
    then Ok ()
    else if not (C.le obj mlower a)
    then Error (mlower, mlower_hint)
    else
      let f' = C.right_adjoint obj f in
      let f'_hint = Hint.right_adjoint f_hint in
      let src = C.src obj f in
      let a' = C.apply src f' a in
      let a'_hint = Morph (f'_hint, f', a_hint) in
      (* If [mlower] was precise, then the check
         [not (C.le obj (mlower obj mv) a)] should guarantee the following call
         to return [Ok ()]. However, [mlower] is not precise *)
      (* not using [Result.map_error] to avoid allocating closure *)
      match submode_vc ~log src v a' a'_hint with
      | Ok () -> Ok ()
      | Error (e, e_hint) -> Error (C.apply obj f e, Morph (f_hint, f, e_hint))

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
    let muupper, muupper_hint = mupper dst mu in
    let mvupper, _mvupper_hint = mupper dst mv in
    let mulower, _mulower_hint = mlower dst mu in
    let mvlower, mvlower_hint = mlower dst mv in
    if C.le dst mvupper mulower
    then Ok ()
    else if eq_morphvar mv mu
    then Ok ()
    else
      (* The call f v <= g u translates to three steps:
         1. f v <= g u.upper
         2. f v.lower <= g u
         3. adding g' (f v) to the u.vlower, where g' is the left adjoint of g.
      *)
      match submode_mvc ~log dst mv muupper muupper_hint with
      | Error (a, a_hint) -> Error (a, a_hint, muupper, muupper_hint)
      | Ok () -> (
        match submode_cmv ~log dst mvlower mvlower_hint mu with
        | Error (a, a_hint) -> Error (mvlower, mvlower_hint, a, a_hint)
        | Ok () ->
          (* At this point, we know that [f v <= g u.upper], which means [f v]
             lies within the downward closure of [g]'s image. Therefore, asking [f
             v <= g u] is equivalent to asking [g' f v <= u] *)
          let g' = C.left_adjoint dst g in
          let g'_hint = Hint.left_adjoint g_hint in
          let src = C.src dst g in
          let g'f = C.compose src g' (C.disallow_right f) in
          let x = Amorphvar (v, g'f, Hint.compose g'_hint f_hint) in
          let key = get_key x in
          if not (VarMap.mem key u.vlower)
          then set_vlower ~log u (VarMap.add key x u.vlower);
          Ok ())

  let cnt_id = ref 0

  let fresh ?upper ?upper_hint ?lower ?lower_hint ?vlower obj =
    let id = !cnt_id in
    cnt_id := id + 1;
    let upper = Option.value upper ~default:(C.max obj) in
    let upper_hint = Option.value upper_hint ~default:(Const Hint.const_none) in
    let lower = Option.value lower ~default:(C.min obj) in
    let lower_hint = Option.value lower_hint ~default:(Const Hint.const_none) in
    let vlower = Option.value vlower ~default:VarMap.empty in
    { upper; upper_hint; lower; lower_hint; vlower; id }

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
    | Amode (left, left_hint), Amode (right, right_hint) ->
      submode_cc ~log obj left left_hint right right_hint
    | Amodevar v, Amode (right, right_hint) ->
      submode_mvc ~log obj v right right_hint
    | Amode (left, left_hint), Amodevar v ->
      submode_cmv ~log obj left left_hint v
    | Amodevar v, Amodevar u -> submode_mvmv ~log obj v u
    | Amode (a, a_hint), Amodemeet (b, b_hint, mvs) ->
      Result.bind (submode_cc ~log obj a a_hint b b_hint) (fun () ->
          find_error (fun mv -> submode_cmv ~log obj a a_hint mv) mvs)
    | Amodevar mv, Amodemeet (b, b_hint, mvs) ->
      Result.bind (submode_mvc ~log obj mv b b_hint) (fun () ->
          find_error (fun mv' -> submode_mvmv ~log obj mv mv') mvs)
    | Amodejoin (a, a_hint, mvs), Amode (b, b_hint) ->
      Result.bind (submode_cc ~log obj a a_hint b b_hint) (fun () ->
          find_error (fun mv' -> submode_mvc ~log obj mv' b b_hint) mvs)
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
        a hint ->
        (a, allowed * disallowed) morphvar VarMap.t ->
        (a, allowed * r) mode list ->
        (a, allowed * disallowed) mode =
     fun a a_hint mvs rest ->
      if C.le obj (C.max obj) a
      then Amode (C.max obj, a_hint)
      else
        match rest with
        | [] -> Amodejoin (a, a_hint, mvs)
        | mv :: xs -> (
          match disallow_right mv with
          | Amode (b, b_hint) ->
            loop (C.join obj a b)
              (choose_hint_for_join_cc obj a a_hint b b_hint)
              mvs xs
          (* some minor optimization: if [a] is lower than [mlower mv], we
              should keep the latter instead. This helps to fail early in
             [submode] *)
          | Amodevar mv ->
            let b, b_hint = mlower obj mv in
            loop (C.join obj a b)
              (choose_hint_for_join_cc obj a a_hint b b_hint)
              (cons_dedup mv mvs) xs
          | Amodejoin (b, b_hint, mvs') ->
            loop (C.join obj a b)
              (choose_hint_for_join_cc obj a a_hint b b_hint)
              (union_prefer_left mvs' mvs)
              xs)
    in
    loop (C.min obj) (Const Hint.const_none) VarMap.empty l

  let meet (type a l) obj l =
    let rec loop :
        a ->
        a hint ->
        (a, disallowed * allowed) morphvar VarMap.t ->
        (a, l * allowed) mode list ->
        (a, disallowed * allowed) mode =
     fun a a_hint mvs rest ->
      if C.le obj a (C.min obj)
      then Amode (C.min obj, a_hint)
      else
        match rest with
        | [] -> Amodemeet (a, a_hint, mvs)
        | mv :: xs -> (
          match disallow_left mv with
          | Amode (b, b_hint) ->
            loop (C.meet obj a b)
              (choose_hint_for_meet_cc obj a a_hint b b_hint)
              mvs xs
            (* some minor optimization: if [a] is higher than [mupper mv], we should keep the latter instead. This helps to fail early in [submode_log] *)
          | Amodevar mv ->
            let b, b_hint = mupper obj mv in
            loop (C.meet obj a b)
              (choose_hint_for_meet_cc obj a a_hint b b_hint)
              (cons_dedup mv mvs) xs
          | Amodemeet (b, b_hint, mvs') ->
            loop (C.meet obj a b)
              (choose_hint_for_meet_cc obj a a_hint b b_hint)
              (union_prefer_left mvs' mvs)
              xs)
    in
    loop (C.max obj) (Const Hint.const_none) VarMap.empty l

  let get_loose_ceil : type a l r. a C.obj -> (a, l * r) mode -> a =
   fun obj m ->
    match m with
    | Amode (a, _a_hint) -> a
    | Amodevar mv -> mupper obj mv |> fst
    | Amodemeet (a, _a_hint, mvs) ->
      VarMap.fold
        (fun _ mv acc ->
          let mvupper, _mvupper_hint = mupper obj mv in
          C.meet obj acc mvupper)
        mvs a
    | Amodejoin (a, _a_hint, mvs) ->
      VarMap.fold
        (fun _ mv acc ->
          let mvupper, _mvupper_hint = mupper obj mv in
          C.join obj acc mvupper)
        mvs a

  let get_loose_floor : type a l r. a C.obj -> (a, l * r) mode -> a =
   fun obj m ->
    match m with
    | Amode (a, _a_hint) -> a
    | Amodevar mv -> mlower obj mv |> fst
    | Amodejoin (a, _a_hint, mvs) ->
      VarMap.fold
        (fun _ mv acc ->
          let mvupper, _mvupper_hint = mupper obj mv in
          C.join obj acc mvupper)
        mvs a
    | Amodemeet (a, _a_hint, mvs) ->
      VarMap.fold
        (fun _ mv acc ->
          let mvlower, _mvlower_hint = mlower obj mv in
          C.meet obj acc mvlower)
        mvs a

  (* Due to our biased implementation, the ceil is precise. *)
  let get_ceil = get_loose_ceil

  let zap_to_ceil : type a l. a C.obj -> (a, l * allowed) mode -> log:_ -> a =
   fun obj m ~log ->
    let ceil = get_ceil obj m in
    submode obj (Amode (ceil, Const Hint.const_none)) m ~log |> Result.get_ok;
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
    let rec loop lower lower_hint =
      let log = ref empty_changes in
      let r = submode_mvc ~log:(Some log) obj mv lower lower_hint in
      match r with
      | Ok () -> !log, lower, lower_hint
      | Error (a, a_hint) ->
        undo_changes !log;
        loop (C.join obj a lower)
          (choose_hint_for_join_cc obj a a_hint lower lower_hint)
    in
    let lower, lower_hint = mlower obj mv in
    loop lower lower_hint

  (** Zaps a morphvar to its floor and returns the floor. [commit] could be
      [Some log], in which case the zapping is appended to [log]; it could also
      be [None], in which case the zapping is reverted. The latter is useful
      when the caller only wants to know the floor without zapping. *)
  let zap_to_floor_morphvar obj mv ~commit =
    let log_, lower, lower_hint = zap_to_floor_morphvar_aux obj mv in
    (match commit with
    | None -> undo_changes log_
    | Some log -> log := append_changes !log log_);
    lower, lower_hint

  let zap_to_floor : type a r. a C.obj -> (a, allowed * r) mode -> log:_ -> a =
   fun obj m ~log ->
    match m with
    | Amode (a, _a_hint) -> a
    | Amodevar mv -> zap_to_floor_morphvar obj mv ~commit:log |> fst
    | Amodejoin (a, a_hint, mvs) ->
      let floor, floor_hint =
        VarMap.fold
          (fun _ mv (acc, acc_hint) ->
            let mv_floor, mv_floor_hint =
              zap_to_floor_morphvar obj mv ~commit:None
            in
            ( C.join obj acc mv_floor,
              choose_hint_for_join_cc obj acc acc_hint mv_floor mv_floor_hint ))
          mvs (a, a_hint)
      in
      VarMap.iter
        (fun _ mv -> submode_mvc obj mv floor floor_hint ~log |> Result.get_ok)
        mvs;
      floor

  let get_floor : type a r. a C.obj -> (a, allowed * r) mode -> a =
   fun obj m ->
    match m with
    | Amode (a, _a_hint) -> a
    | Amodevar mv -> zap_to_floor_morphvar obj mv ~commit:None |> fst
    | Amodejoin (a, _a_hint, mvs) ->
      VarMap.fold
        (fun _ mv acc ->
          let mv_floor, _mv_floor_hint =
            zap_to_floor_morphvar obj mv ~commit:None
          in
          C.join obj acc mv_floor)
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

  let newvar obj = Amodevar (Amorphvar (fresh obj, C.id, Hint.morph_none))

  let newvar_above (type a r) (obj : a C.obj) (m : (a, allowed * r) mode) =
    match disallow_right m with
    | Amode (a, a_hint) ->
      if C.le obj (C.max obj) a
      then Amode (a, a_hint), false
      else
        ( Amodevar
            (Amorphvar
               (fresh ~lower:a ~lower_hint:a_hint obj, C.id, Hint.morph_none)),
          true )
    | Amodevar mv ->
      (* [~lower] is not precise (because [mlower mv] is not precise), but
         it doesn't need to be *)
      let mv_lower, mv_lower_hint = mlower obj mv in
      ( Amodevar
          (Amorphvar
             ( fresh ~lower:mv_lower ~lower_hint:mv_lower_hint
                 ~vlower:(VarMap.singleton (get_key mv) mv)
                 obj,
               C.id,
               Hint.morph_none )),
        true )
    | Amodejoin (a, a_hint, mvs) ->
      (* [~lower] is not precise here, but it doesn't need to be *)
      ( Amodevar
          (Amorphvar
             ( fresh ~lower:a ~lower_hint:a_hint ~vlower:mvs obj,
               C.id,
               Hint.morph_none )),
        true )

  let newvar_below (type a l) (obj : a C.obj) (m : (a, l * allowed) mode) =
    match disallow_left m with
    | Amode (a, a_hint) ->
      if C.le obj a (C.min obj)
      then Amode (a, a_hint), false
      else
        ( Amodevar
            (Amorphvar
               (fresh ~upper:a ~upper_hint:a_hint obj, C.id, Hint.morph_none)),
          true )
    | Amodevar mv ->
      let u = fresh obj in
      let mu = Amorphvar (u, C.id, Hint.morph_none) in
      submode_mvmv obj ~log:None mu mv |> Result.get_ok;
      allow_left (Amodevar mu), true
    | Amodemeet (a, a_hint, mvs) ->
      let u = fresh obj in
      let mu = Amorphvar (u, C.id, Hint.morph_none) in
      submode_mvc obj ~log:None mu a a_hint |> Result.get_ok;
      VarMap.iter
        (fun _ mv -> submode_mvmv obj ~log:None mu mv |> Result.get_ok)
        mvs;
      allow_left (Amodevar mu), true
end
[@@inline always]
