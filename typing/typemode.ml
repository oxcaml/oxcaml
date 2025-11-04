open Location
open Mode
open Jkind_axis

type 'ax annot_type =
  | Modifier : 'a Axis.t annot_type
  | Mode : 'a Alloc.Axis.t annot_type
  | Modality : 'a Modality.Axis.t annot_type

let print_annot_type (type a) ppf (annot_type : a annot_type) =
  match annot_type with
  | Modifier -> Format.fprintf ppf "modifier"
  | Mode -> Format.fprintf ppf "mode"
  | Modality -> Format.fprintf ppf "modality"

let print_annot_axis (type a) (annot_type : a annot_type) ppf (ax : a) =
  match annot_type with
  | Modifier -> Format.fprintf ppf "%s" (Axis.name ax)
  | Mode -> Alloc.Axis.print ppf ax
  | Modality ->
    let (P ax) = Modality.Axis.to_value (P ax) in
    Value.Axis.print ppf ax

type forbidden_modality_kind =
  | Global_and_unique
      (** [@@ global unique] must be forbidden, with [global] implying
          [aliased]. Otherwise, borrowing would be unsound:

  {v
      type 'a t = { x : 'a @@ global unique }

      let clone (x @ unique) =
        borrow {x} ~f:(fun (t @ local) -> t.x : 'a @ global) (* leak *)
  v}
  *)

type error =
  | Forbidden_modality : 'a annot_type * forbidden_modality_kind -> error
  | Duplicated_axis : 'a annot_type * 'a -> error
  | Unrecognized_modifier : 'a annot_type * string -> error

exception Error of Location.t * error

module Mode_axis_pair = struct
  type t = P : 'a Alloc.Axis.t * 'a -> t

  type t_value = P : 'a Value.Axis.t * 'a -> t_value

  let to_value (P (ax, a) : t) : t_value =
    match Const.Axis.is_areality ax with
    | Left Refl -> P (Comonadic Areality, Const.locality_as_regionality a)
    | Right ax -> P (ax, a)

  let of_string s : t =
    let comonadic (type a) (ax : a Alloc.Comonadic.Axis.t) (a : a) : t =
      P (Comonadic ax, a)
    in
    let monadic (type a) (ax : a Alloc.Monadic.Axis.t) (a : a) : t =
      P (Monadic ax, a)
    in
    match[@warning "-18"] s with
    | "local" -> comonadic Areality Local
    (* "regional" is not supported *)
    | "global" -> comonadic Areality Global
    | "unique" -> monadic Uniqueness Unique
    | "aliased" -> monadic Uniqueness Aliased
    | "once" -> comonadic Linearity Once
    | "many" -> comonadic Linearity Many
    | "nonportable" -> comonadic Portability Nonportable
    | "sharable" -> comonadic Portability Sharable
    | "portable" -> comonadic Portability Portable
    | "contended" -> monadic Contention Contended
    | "shared" -> monadic Contention Shared
    | "uncontended" -> monadic Contention Uncontended
    | "unforkable" -> comonadic Forkable Unforkable
    | "forkable" -> comonadic Forkable Forkable
    | "yielding" -> comonadic Yielding Yielding
    | "unyielding" -> comonadic Yielding Unyielding
    | "stateless" -> comonadic Statefulness Stateless
    | "observing" -> comonadic Statefulness Observing
    | "stateful" -> comonadic Statefulness Stateful
    | "immutable" -> monadic Visibility Immutable
    | "read" -> monadic Visibility Read
    | "read_write" -> monadic Visibility Read_write
    | "static" -> monadic Staticity Static
    | "dynamic" -> monadic Staticity Dynamic
    | _ -> raise Not_found
end

module Modality_axis_pair = struct
  type t = P : 'a Modality.Axis.t * 'a -> t

  let of_string s : t =
    match[@warning "-18"]
      Mode_axis_pair.to_value (Mode_axis_pair.of_string s)
    with
    | P (Monadic ax, c) -> P (Monadic ax, Join_with c)
    | P (Comonadic ax, c) -> P (Comonadic ax, Meet_with c)
end

module Modifier_axis_pair = struct
  type t = P : 'a Axis.t * 'a -> t

  let of_string s : t =
    match[@warning "-18"] Modality_axis_pair.of_string s with
    | P (Monadic ax, m) -> P (Modal (Monadic ax), Modality m)
    | P (Comonadic ax, m) -> P (Modal (Comonadic ax), Modality m)
    | exception Not_found -> (
      let nonmodal (type a) (ax : a Axis.Nonmodal.t) (a : a) : t =
        P (Nonmodal ax, a)
      in
      match s with
      | "maybe_null" -> nonmodal Nullability Maybe_null
      | "non_null" -> nonmodal Nullability Non_null
      | "internal" -> nonmodal Externality Internal
      | "external64" -> nonmodal Externality External64
      | "external_" -> nonmodal Externality External
      | "maybe_separable" -> nonmodal Separability Maybe_separable
      | "separable" -> nonmodal Separability Separable
      | "non_float" -> nonmodal Separability Non_float
      | _ -> raise Not_found)
end

module Transled_modifiers = struct
  module Monadic = Mode.Crossing.Monadic
  module Comonadic = Mode.Crossing.Comonadic

  type t =
    { areality : Mode.Regionality.Const.t Comonadic.Atom.t Location.loc option;
      linearity : Mode.Linearity.Const.t Comonadic.Atom.t Location.loc option;
      uniqueness : Mode.Uniqueness.Const.t Monadic.Atom.t Location.loc option;
      portability :
        Mode.Portability.Const.t Comonadic.Atom.t Location.loc option;
      contention : Mode.Contention.Const.t Monadic.Atom.t Location.loc option;
      forkable : Mode.Forkable.Const.t Comonadic.Atom.t Location.loc option;
      yielding : Mode.Yielding.Const.t Comonadic.Atom.t Location.loc option;
      statefulness :
        Mode.Statefulness.Const.t Comonadic.Atom.t Location.loc option;
      visibility : Mode.Visibility.Const.t Monadic.Atom.t Location.loc option;
      staticity : Mode.Staticity.Const.t Monadic.Atom.t Location.loc option;
      (* CR-soon zqian: Create a functor [Mode.Value.Const.Make] to generate
         different type operators applied on mode constants. *)
      externality : Jkind_axis.Externality.t Location.loc option;
      nullability : Jkind_axis.Nullability.t Location.loc option;
      separability : Jkind_axis.Separability.t Location.loc option
    }

  let empty =
    { areality = None;
      linearity = None;
      uniqueness = None;
      portability = None;
      contention = None;
      forkable = None;
      yielding = None;
      statefulness = None;
      visibility = None;
      externality = None;
      nullability = None;
      separability = None;
      staticity = None
    }

  let get (type a) ~(axis : a Axis.t) (t : t) : a Location.loc option =
    match axis with
    | Modal (Comonadic Areality) -> t.areality
    | Modal (Comonadic Linearity) -> t.linearity
    | Modal (Monadic Uniqueness) -> t.uniqueness
    | Modal (Comonadic Portability) -> t.portability
    | Modal (Monadic Contention) -> t.contention
    | Modal (Comonadic Forkable) -> t.forkable
    | Modal (Comonadic Yielding) -> t.yielding
    | Modal (Comonadic Statefulness) -> t.statefulness
    | Modal (Monadic Visibility) -> t.visibility
    | Modal (Monadic Staticity) -> t.staticity
    | Nonmodal Externality -> t.externality
    | Nonmodal Nullability -> t.nullability
    | Nonmodal Separability -> t.separability

  let set (type a) ~(axis : a Axis.t) (t : t) (value : a Location.loc option) :
      t =
    match axis with
    | Modal (Comonadic Areality) -> { t with areality = value }
    | Modal (Comonadic Linearity) -> { t with linearity = value }
    | Modal (Monadic Uniqueness) -> { t with uniqueness = value }
    | Modal (Comonadic Portability) -> { t with portability = value }
    | Modal (Monadic Contention) -> { t with contention = value }
    | Modal (Comonadic Forkable) -> { t with forkable = value }
    | Modal (Comonadic Yielding) -> { t with yielding = value }
    | Modal (Comonadic Statefulness) -> { t with statefulness = value }
    | Modal (Monadic Visibility) -> { t with visibility = value }
    | Modal (Monadic Staticity) -> { t with staticity = value }
    | Nonmodal Externality -> { t with externality = value }
    | Nonmodal Nullability -> { t with nullability = value }
    | Nonmodal Separability -> { t with separability = value }
end

(* Since [unforkable yielding] is the default mode in presence of [local], the
   [global] modality must also apply [forkable unyielding] unless specified.

   Similarly [visibility]/[contention] and [statefulness]/[portability].

   [global] must imply [aliased] for soundness of borrowing. *)
let implied_modalities (Atom (ax, a) : Modality.atom) : Modality.atom list =
  match[@warning "-18"] ax, a with
  | Comonadic Areality, Meet_with a ->
    let f, y, u =
      match a with
      | Global ->
        ( Forkable.Const.Forkable,
          Yielding.Const.Unyielding,
          [Uniqueness.Const.Aliased] )
      | Local -> Forkable.Const.Unforkable, Yielding.Const.Yielding, []
      | Regional -> assert false
    in
    [ Modality.Atom (Comonadic Forkable, Meet_with f);
      Atom (Comonadic Yielding, Meet_with y) ]
    @ List.map (fun x -> Modality.Atom (Monadic Uniqueness, Join_with x)) u
  | Monadic Visibility, Join_with a ->
    let b : Contention.Const.t =
      match a with
      | Immutable -> Contended
      | Read -> Shared
      | Read_write -> Uncontended
    in
    [Atom (Monadic Contention, Join_with b)]
  | Comonadic Statefulness, Meet_with a ->
    let b : Portability.Const.t =
      match a with
      | Stateless -> Portable
      | Observing -> Sharable
      | Stateful -> Nonportable
    in
    [Atom (Comonadic Portability, Meet_with b)]
  | _ -> []

let enforce_forbidden_modalities ~loc annot_type m =
  match
    ( Modality.Const.proj (Comonadic Areality) m,
      Modality.Const.proj (Monadic Uniqueness) m )
  with
  | ( Meet_with Global,
      Modality.Monadic.Atom.Join_with Mode.Uniqueness.Const.Unique ) ->
    raise
      (Error (loc, Forbidden_modality (annot_type, Global_and_unique)))
  | _ -> ()

let transl_mod_bounds annots =
  let bounds_loc =
    match List.map (fun { loc; _ } -> loc) annots with
    | [] -> Location.none
    | _ :: _ as locs -> Location.merge locs
  in
  let step bounds_so_far { txt = Parsetree.Mode txt; loc } =
    match Modifier_axis_pair.of_string txt with
    | P (type a) ((axis, mode) : a Axis.t * a) ->
      let is_top = Per_axis.(le axis (max axis) mode) in
      if is_top
      then
        (* CR layouts v2.8: This warning is disabled for now because transl_type_decl
           results in 3 calls to transl_annots per user-written annotation. This results
           in the warning being reported 3 times. Internal ticket 2801. *)
        (* Location.prerr_warning new_raw.loc (Warnings.Mod_by_top new_raw.txt) *)
        ();
      let is_dup =
        Option.is_some (Transled_modifiers.get ~axis bounds_so_far)
      in
      if is_dup then raise (Error (loc, Duplicated_axis (Modifier, axis)));
      Transled_modifiers.set ~axis bounds_so_far (Some { txt = mode; loc })
    | exception Not_found -> (
      match txt with
      | "everything" ->
        Transled_modifiers.
          { areality =
              Some { txt = Per_axis.min (Modal (Comonadic Areality)); loc };
            linearity =
              Some { txt = Per_axis.min (Modal (Comonadic Linearity)); loc };
            uniqueness =
              Some { txt = Per_axis.min (Modal (Monadic Uniqueness)); loc };
            portability =
              Some { txt = Per_axis.min (Modal (Comonadic Portability)); loc };
            contention =
              Some { txt = Per_axis.min (Modal (Monadic Contention)); loc };
            forkable =
              Some { txt = Per_axis.min (Modal (Comonadic Forkable)); loc };
            yielding =
              Some { txt = Per_axis.min (Modal (Comonadic Yielding)); loc };
            externality = Some { txt = Externality.min; loc };
            statefulness =
              Some { txt = Per_axis.min (Modal (Comonadic Statefulness)); loc };
            visibility =
              Some { txt = Per_axis.min (Modal (Monadic Visibility)); loc };
            staticity = None;
            nullability =
              Transled_modifiers.get ~axis:(Nonmodal Nullability) bounds_so_far;
            separability =
              Transled_modifiers.get ~axis:(Nonmodal Separability) bounds_so_far
          }
      | _ -> raise (Error (loc, Unrecognized_modifier (Modifier, txt))))
  in
  let raw_modifiers = List.fold_left step Transled_modifiers.empty annots in
  let modality =
    let open Modality in
    let has_explicit axis =
      let (P axis) = Crossing.Axis.of_modality (P axis) in
      Option.is_some (Transled_modifiers.get ~axis:(Modal axis) raw_modifiers)
    in
    let add_implied axis value acc =
      List.fold_left
        (fun acc (Atom (axis', value')) ->
          if has_explicit axis' then acc else Const.set axis' value' acc)
        acc
        (implied_modalities (Atom (axis, value)))
    in
    let add_comonadic acc axis =
      match
        Transled_modifiers.get ~axis:(Modal (Comonadic axis)) raw_modifiers
      with
      | None -> acc
      | Some { txt = Modality value; _ } ->
        let acc = Const.set (Comonadic axis) value acc in
        add_implied (Comonadic axis) value acc
    in
    let add_monadic acc axis =
      match
        Transled_modifiers.get ~axis:(Modal (Monadic axis)) raw_modifiers
      with
      | None -> acc
      | Some { txt = Modality value; _ } ->
        let acc = Const.set (Monadic axis) value acc in
        add_implied (Monadic axis) value acc
    in
    let add acc = function
      | Value.Axis.P (Comonadic axis) -> add_comonadic acc axis
      | Value.Axis.P (Monadic axis) -> add_monadic acc axis
    in
    List.fold_left add Const.id Value.Axis.all
  in
  enforce_forbidden_modalities Modifier ~loc:bounds_loc modality;
  let open Types.Jkind_mod_bounds in
  let externality =
    Option.fold ~some:Location.get_txt ~none:Externality.max
      raw_modifiers.externality
  in
  let nullability =
    Option.fold ~some:Location.get_txt ~none:Nullability.max
      raw_modifiers.nullability
  in
  let separability =
    Option.fold ~some:Location.get_txt ~none:Separability.max
      raw_modifiers.separability
  in
  let crossing = Crossing.modality modality Crossing.max in
  create crossing ~externality ~nullability ~separability

let default_mode_annots (annots : Alloc.Const.Option.t) =
  (* [forkable] has a different default depending on whether [areality]
     is [global] or [local]. *)
  let forkable =
    match annots.forkable, annots.areality with
    | (Some _ as y), _ | y, None -> y
    | None, Some Locality.Const.Global -> Some Forkable.Const.Forkable
    | None, Some Locality.Const.Local -> Some Forkable.Const.Unforkable
  in
  (* Likewise for [yielding]. *)
  let yielding =
    match annots.yielding, annots.areality with
    | (Some _ as y), _ | y, None -> y
    | None, Some Locality.Const.Global -> Some Yielding.Const.Unyielding
    | None, Some Locality.Const.Local -> Some Yielding.Const.Yielding
  in
  (* Likewise for [contention]. *)
  let contention =
    match annots.contention, annots.visibility with
    | (Some _ as c), _ | c, None -> c
    | None, Some Visibility.Const.Immutable -> Some Contention.Const.Contended
    | None, Some Visibility.Const.Read -> Some Contention.Const.Shared
    | None, Some Visibility.Const.Read_write ->
      Some Contention.Const.Uncontended
  in
  (* Likewise for [portability]. *)
  let portability =
    match annots.portability, annots.statefulness with
    | (Some _ as p), _ | p, None -> p
    | None, Some Statefulness.Const.Stateless -> Some Portability.Const.Portable
    | None, Some Statefulness.Const.Observing -> Some Portability.Const.Sharable
    | None, Some Statefulness.Const.Stateful ->
      Some Portability.Const.Nonportable
  in
  { annots with forkable; yielding; contention; portability }

let transl_mode_annots annots : Alloc.Const.Option.t =
  let step modes_so_far { txt = Parsetree.Mode txt; loc } =
    Language_extension.assert_enabled ~loc Mode Language_extension.Stable;
    let (P (ax, a)) =
      try Mode_axis_pair.of_string txt
      with Not_found -> raise (Error (loc, Unrecognized_modifier (Mode, txt)))
    in
    if Option.is_some (Alloc.Const.Option.proj ax modes_so_far)
    then raise (Error (loc, Duplicated_axis (Mode, ax)))
    else Alloc.Const.Option.set ax (Some a) modes_so_far
  in
  List.fold_left step Alloc.Const.Option.none annots |> default_mode_annots

let untransl_mode_annots (modes : Mode.Alloc.Const.Option.t) =
  let print_to_string_opt print a = Option.map (Format.asprintf "%a" print) a in
  (* Untranslate [areality], [forkable], and [yielding]. *)
  let areality = print_to_string_opt Mode.Locality.Const.print modes.areality in
  let forkable =
    (* Since [forkable] has non-standard defaults, we special-case
       whether we want to print it here. *)
    match modes.forkable, modes.areality with
    | Some Forkable.Const.Unforkable, Some Locality.Const.Local
    | Some Forkable.Const.Forkable, Some Locality.Const.Global ->
      None
    | _, _ -> print_to_string_opt Mode.Forkable.Const.print modes.forkable
  in
  let yielding =
    match modes.yielding, modes.areality with
    | Some Yielding.Const.Yielding, Some Locality.Const.Local
    | Some Yielding.Const.Unyielding, Some Locality.Const.Global ->
      None
    | _, _ -> print_to_string_opt Mode.Yielding.Const.print modes.yielding
  in
  (* Untranslate [visibility] and [contention]. *)
  let visibility =
    print_to_string_opt Mode.Visibility.Const.print modes.visibility
  in
  let contention =
    match modes.visibility, modes.contention with
    | Some Visibility.Const.Immutable, Some Contention.Const.Contended
    | Some Visibility.Const.Read, Some Contention.Const.Shared
    | Some Visibility.Const.Read_write, Some Contention.Const.Uncontended ->
      None
    | _, _ -> print_to_string_opt Mode.Contention.Const.print modes.contention
  in
  (* Untranslate [statefulness] and [portability]. *)
  let statefulness =
    print_to_string_opt Mode.Statefulness.Const.print modes.statefulness
  in
  let portability =
    match modes.statefulness, modes.portability with
    | Some Statefulness.Const.Stateless, Some Portability.Const.Portable
    | Some Statefulness.Const.Observing, Some Portability.Const.Sharable
    | Some Statefulness.Const.Stateful, Some Portability.Const.Nonportable ->
      None
    | _, _ -> print_to_string_opt Mode.Portability.Const.print modes.portability
  in
  (* Untranslate remaining modes. *)
  let uniqueness =
    print_to_string_opt Mode.Uniqueness.Const.print modes.uniqueness
  in
  let linearity =
    print_to_string_opt Mode.Linearity.Const.print modes.linearity
  in
  List.filter_map
    (fun x ->
      Option.map (fun s -> { txt = Parsetree.Mode s; loc = Location.none }) x)
    [ areality;
      uniqueness;
      linearity;
      portability;
      contention;
      forkable;
      yielding;
      statefulness;
      visibility ]

let transl_modality ~maturity { txt = Parsetree.Modality modality; loc } =
  Language_extension.assert_enabled ~loc Mode maturity;
  let (P (ax, a)) =
    try Mode_axis_pair.(of_string modality |> to_value)
    with Not_found ->
      raise (Error (loc, Unrecognized_modifier (Modality, modality)))
  in
  let atom : Modality.atom =
    match[@warning "-18"] ax with
    | Comonadic ax -> Atom (Comonadic ax, Meet_with a)
    | Monadic ax -> Atom (Monadic ax, Join_with a)
  in
  atom, loc

let untransl_modality (a : Modality.atom) : Parsetree.modality loc =
  let s =
    match a with
    | Atom (Comonadic ax, Meet_with a) ->
      Format.asprintf "%a" (Value.Comonadic.Const.Per_axis.print ax) a
    | Atom (Monadic ax, Join_with a) ->
      Format.asprintf "%a" (Value.Monadic.Const.Per_axis.print ax) a
  in
  { txt = Modality s; loc = Location.none }

(* For now, mutable implies:
   1. [global forkable unyielding]. This is for compatibility with existing code
      and will be removed in the future.
   2. [many]. This is to remedy the coarse treatment of modalities in the
      uniqueness analysis.
      See [https://github.com/oxcaml/oxcaml/pull/4415#discussion_r2250801078].
   3. legacy modalities for all monadic axes. This will stay in the future.

   Implied modalities can be overriden. *)
(* CR zqian: remove [1] and [2] *)
let[@warning "-18"] mutable_implied_modalities ~for_mutable_variable mut =
  let comonadic : Modality.atom list =
    [ Atom (Comonadic Areality, Meet_with Regionality.Const.legacy);
      Atom (Comonadic Linearity, Meet_with Linearity.Const.legacy);
      Atom (Comonadic Forkable, Meet_with Forkable.Const.legacy);
      Atom (Comonadic Yielding, Meet_with Yielding.Const.legacy) ]
  in
  let monadic : Modality.atom list =
    [ Atom (Monadic Uniqueness, Join_with Uniqueness.Const.legacy);
      Atom (Monadic Contention, Join_with Contention.Const.legacy);
      Atom (Monadic Visibility, Join_with Visibility.Const.legacy);
      Atom (Monadic Staticity, Join_with Staticity.Const.legacy) ]
  in
  if mut
  then if for_mutable_variable then monadic else monadic @ comonadic
  else []

let mutable_implied_modalities ~for_mutable_variable mut =
  let l = mutable_implied_modalities ~for_mutable_variable mut in
  List.fold_left
    (fun t (Modality.Atom (ax, a)) -> Modality.Const.set ax a t)
    Modality.Const.id l

let idx_expected_modalities ~(mut : bool) =
  (* There are two design constraints on what modalities we allow in an index
     creation to contain. Because these are coupled, this function checks that
     they are equal.
      1. The default modalities (id for non-mutable fields, global many aliased
         forkable unyielding for mutable fields) should work.
      2. It should also be safe wrt to type signatures given to block index
         primitives (see [idx_imm.mli] and [idx_mut.mli] in [Stdlib_beta]). *)
  let modality_of_list l =
    List.fold_left
      (fun t (Modality.Atom (ax, a)) -> Modality.Const.set ax a t)
      Modality.Const.id l
  in
  let expected1 = mutable_implied_modalities mut ~for_mutable_variable:false in
  let expected2 =
    if mut
    then
      (* If this list is updated, the external bindings in the [Idx_imm] and
         [Idx_mut] modules in [Stdlib_beta] may also have to be updated. *)
      modality_of_list
        [ Atom (Comonadic Areality, Meet_with Regionality.Const.legacy);
          Atom (Comonadic Linearity, Meet_with Linearity.Const.legacy);
          Atom (Comonadic Forkable, Meet_with Forkable.Const.legacy);
          Atom (Comonadic Yielding, Meet_with Yielding.Const.legacy);
          Atom (Monadic Uniqueness, Join_with Uniqueness.Const.legacy);
          Atom (Monadic Staticity, Join_with Staticity.Const.legacy) ]
      [@warning "-18"]
    else Mode.Modality.Const.id
  in
  (* CR layouts v8: only perform this check at most twice: for [mut = true] and
     [mut = false] *)
  match Mode.Modality.Const.equate expected1 expected2 with
  | Ok () -> expected1
  | Error _ ->
    Misc.fatal_error
      "Typemode.idx_expected_modalities: mismatch with mutable implied \
       modalities"

let least_modalities_implying mut (t : Modality.Const.t) =
  let baseline =
    mutable_implied_modalities ~for_mutable_variable:false
      (Types.is_mutable mut)
  in
  let annotated = Modality.Const.(diff baseline t) in
  let implied = List.concat_map implied_modalities annotated in
  let exclude_implied =
    List.filter (fun x -> not @@ List.mem x implied) annotated
  in
  let overridden =
    List.filter_map
      (fun (Modality.Atom (ax, m_implied)) ->
        let m_projected = Modality.Const.proj ax t in
        if m_projected <> m_implied
        then Some (Modality.Atom (ax, m_projected))
        else None)
      implied
  in
  exclude_implied @ overridden

let untransl_mod_bounds (bounds : Types.Jkind_mod_bounds.t) : Parsetree.modes =
  let crossing = Types.Jkind_mod_bounds.crossing bounds in
  let modality =
    let open Modality in
    let add_comonadic acc axis =
      let (Crossing.Comonadic.Atom.Modality atom) =
        Crossing.proj (Crossing.Axis.Comonadic axis) crossing
      in
      Const.set (Comonadic axis) atom acc
    in
    let add_monadic acc axis =
      let (Crossing.Monadic.Atom.Modality atom) =
        Crossing.proj (Crossing.Axis.Monadic axis) crossing
      in
      Const.set (Monadic axis) atom acc
    in
    let add acc = function
      | Value.Axis.P (Comonadic axis) -> add_comonadic acc axis
      | Value.Axis.P (Monadic axis) -> add_monadic acc axis
    in
    List.fold_left add Modality.Const.id Value.Axis.all
  in
  let modality_annots =
    least_modalities_implying Types.Immutable modality
    |> List.map (fun atom ->
           let { Location.txt = Parsetree.Modality s; _ } =
             untransl_modality atom
           in
           { Location.txt = Parsetree.Mode s; loc = Location.none })
  in
  let nonmodal_annots =
    let open Types.Jkind_mod_bounds in
    let mk_annot default print value =
      if value = default
      then None
      else
        let s = Format.asprintf "%a" print value in
        Some { Location.txt = Parsetree.Mode s; loc = Location.none }
    in
    [ mk_annot Externality.max Externality.print (externality bounds);
      mk_annot Nullability.max Nullability.print (nullability bounds);
      mk_annot Separability.max Separability.print (separability bounds) ]
    |> List.filter_map Fun.id
  in
  modality_annots @ nonmodal_annots

let sort_dedup_modalities ~warn l =
  let open Modality in
  let compare (Atom (ax0, _), _) (Atom (ax1, _), _) =
    let (P ax0) = Axis.to_value (P ax0) in
    let (P ax1) = Axis.to_value (P ax1) in
    Mode.Value.Axis.compare ax0 ax1
  in
  let dedup ~on_dup =
    let rec loop x = function
      | [] -> [x]
      | y :: xs ->
        if compare x y = 0
        then (
          on_dup x y;
          loop y xs)
        else x :: loop y xs
    in
    function [] -> [] | x :: xs -> loop x xs
  in
  let on_dup (Atom (ax0, _), loc0) (Atom (ax1, a1), _) =
    if warn
    then
      let (P ax0) = Axis.to_value (P ax0) in
      let axis = Format.asprintf "%a" Mode.Value.Axis.print ax0 in
      let { txt = Modality overriden_by; _ } =
        untransl_modality (Atom (ax1, a1))
      in
      Location.prerr_warning loc0
        (Warnings.Modal_axis_specified_twice { axis; overriden_by })
  in
  l |> List.stable_sort compare |> dedup ~on_dup |> List.map fst

let transl_modalities ~maturity mut modalities =
  let modalities_loc =
    match List.map (fun { loc; _ } -> loc) modalities with
    | [] -> Location.none
    | _ :: _ as locs -> Location.merge locs
  in
  let mut_modalities =
    mutable_implied_modalities (Types.is_mutable mut)
      ~for_mutable_variable:false
  in
  let modalities = List.map (transl_modality ~maturity) modalities in
  (* axes listed in the order of implication. *)
  let modalities = sort_dedup_modalities ~warn:true modalities in
  let open Modality in
  (* - mut_modalities is applied before explicit modalities.
     - explicit modalities can override mut_modalities.
     - For the same axis, later modalities overrides earlier modalities. *)
  let modalities =
    List.fold_left
      (fun m (Atom (ax, a) as t) ->
        let m = Const.set ax a m in
        List.fold_left
          (fun m (Atom (ax, a)) -> Const.set ax a m)
          m (implied_modalities t))
      mut_modalities modalities
  in
  enforce_forbidden_modalities Modality ~loc:modalities_loc modalities;
  modalities

let let_mutable_modalities =
  mutable_implied_modalities true ~for_mutable_variable:true

let atomic_mutable_modalities =
  mutable_implied_modalities true ~for_mutable_variable:false

let untransl_modalities mut t =
  t
  |> least_modalities_implying mut
  |> List.map (fun x -> x, Location.none)
  |> sort_dedup_modalities ~warn:false
  |> List.map untransl_modality

let transl_alloc_mode modes =
  let opt = transl_mode_annots modes in
  Alloc.Const.Option.value opt ~default:Alloc.Const.legacy

(* Error reporting *)

let report_error ppf =
  let open Format in
  function
  | Duplicated_axis (annot_type, axis) ->
    fprintf ppf "The %a axis has already been specified."
      (print_annot_axis annot_type)
      axis
  | Forbidden_modality (annot_type, Global_and_unique) ->
    fprintf ppf "The %a %a can't be used together with %a" print_annot_type
      annot_type Misc.Style.inline_code "global" Misc.Style.inline_code "unique"
  | Unrecognized_modifier (annot_type, modifier) ->
    fprintf ppf "Unrecognized %a %s." print_annot_type annot_type modifier

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
