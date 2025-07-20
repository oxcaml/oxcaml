open Location
open Mode
open Jkind_axis

(* CR zqian: kind modifier can be either a modaity or externality/nullability.
   I.e., mode-like modifiers are just modalities and should be represented as
   such. Therefore, [transl_modalities] (not dealing with
   externality/nullability) will stay in this file, while [transl_modifiers]
   should go into [typekind.ml] and calls [transl_modalities]. *)

type modal = private |

type maybe_nonmodal = private |

type 'm annot_type =
  | Modifier : maybe_nonmodal annot_type
  | Mode : modal annot_type
  | Modality : modal annot_type

type error =
  | Duplicated_axis : _ Axis.t -> error
  | Unrecognized_modifier : _ annot_type * string -> error

exception Error of Location.t * error

module Modal_axis_pair = struct
  [@@@warning "-18"]

  type t = P : 'a Value.Axis.t * 'a -> t

  let of_string s : t =
    let comonadic : type a. a Value.Comonadic.axis -> a -> t =
     fun ax a -> P (Comonadic ax, a)
    in
    let monadic : type a. a Value.Monadic.axis -> a -> t =
     fun ax a -> P (Monadic ax, a)
    in
    match s with
    | "local" -> comonadic Areality Local
    | "global" -> comonadic Areality Global
    | "unique" -> monadic Uniqueness Unique
    | "aliased" -> monadic Uniqueness Aliased
    | "once" -> comonadic Linearity Once
    | "many" -> comonadic Linearity Many
    | "nonportable" -> comonadic Portability Nonportable
    | "portable" -> comonadic Portability Portable
    | "contended" -> monadic Contention Contended
    | "shared" -> monadic Contention Shared
    | "uncontended" -> monadic Contention Uncontended
    | "yielding" -> comonadic Yielding Yielding
    | "unyielding" -> comonadic Yielding Unyielding
    | "stateless" -> comonadic Statefulness Stateless
    | "observing" -> comonadic Statefulness Observing
    | "stateful" -> comonadic Statefulness Stateful
    | "immutable" -> monadic Visibility Immutable
    | "read" -> monadic Visibility Read
    | "read_write" -> monadic Visibility Read_write
    | _ -> raise Not_found
end

module Axis_pair = struct
  type t = P : 'a Axis.t * 'a -> t

  [@@@warning "-18"]

  let of_string s =
    match Modal_axis_pair.of_string s with
    | P (ax, a) ->
      let moda : _ Modality.raw =
        match ax with Monadic _ -> Join_with a | Comonadic _ -> Meet_with a
      in
      P (Modal ax, Crossing.Atom.(Modality moda))
    | exception Not_found -> (
      let nonmodal : type a. a Axis.Nonmodal.t -> a -> t =
       fun ax a -> P (Nonmodal ax, a)
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

let unpack_mode_annot { txt = Parsetree.Mode s; loc } = { txt = s; loc }

module Transled_modifiers = struct
  type t =
    { crossing : Crossing.t;
      externality : Jkind_axis.Externality.t Location.loc option;
      nullability : Jkind_axis.Nullability.t Location.loc option;
      separability : Jkind_axis.Separability.t Location.loc option
    }

  let empty =
    { crossing = Crossing.max;
      externality = None;
      nullability = None;
      separability = None
    }

  let get_nonmodal (type a) ~(ax : a Axis.Nonmodal.t) (t : t) :
      a Location.loc option =
    match ax with
    | Externality -> t.externality
    | Nullability -> t.nullability
    | Separability -> t.separability

  let set_nonmodal (type a) ~(ax : a Axis.Nonmodal.t) (t : t)
      (value : a Location.loc option) : t =
    match ax with
    | Externality -> { t with externality = value }
    | Nullability -> { t with nullability = value }
    | Separability -> { t with separability = value }
end

let transl_mod_bounds annots =
  let step (bounds_so_far : Transled_modifiers.t) { txt; loc } =
    match Axis_pair.of_string txt with
    | P (type a) (((Nonmodal ax as axis), mode) : a Axis.t * a) ->
      let (module A) = Axis.get_nonmodal ax in
      let is_top = A.le A.max mode in
      if is_top
      then
        (* CR layouts v2.8: This warning is disabled for now because transl_type_decl
           results in 3 calls to transl_annots per user-written annotation. This results
           in the warning being reported 3 times. *)
        (* Location.prerr_warning new_raw.loc (Warnings.Mod_by_top new_raw.txt) *)
        ();
      let is_dup =
        Option.is_some (Transled_modifiers.get_nonmodal ~ax bounds_so_far)
      in
      if is_dup then raise (Error (loc, Duplicated_axis axis));
      Transled_modifiers.set_nonmodal ~ax bounds_so_far
        (Some { txt = mode; loc })
    | P (type a) (((Modal ax as axis), a) : a Axis.t * a) ->
      let is_top = Crossing.Atom.(le ax (max ax) a) in
      if is_top
      then
        (* CR layouts v2.8: This warning is disabled for now because transl_type_decl
           results in 3 calls to transl_annots per user-written annotation. This results
           in the warning being reported 3 times. *)
        (* Location.prerr_warning new_raw.loc (Warnings.Mod_by_top new_raw.txt) *)
        ();
      let crossing = bounds_so_far.crossing in
      let old = Crossing.proj ax crossing in
      let is_not_set = Crossing.Atom.(le ax old (min ax)) in
      let is_dup = not is_not_set in
      if is_dup then raise (Error (loc, Duplicated_axis axis));
      let crossing = Crossing.set ax a crossing in
      { bounds_so_far with crossing }
    | exception Not_found -> (
      match txt with
      | "everything" ->
        Transled_modifiers.
          { crossing = Crossing.min;
            externality = Some { txt = Externality.min; loc };
            nullability =
              Transled_modifiers.get_nonmodal ~ax:Nullability bounds_so_far;
            separability =
              Transled_modifiers.get_nonmodal ~ax:Separability bounds_so_far
          }
      | _ -> raise Not_found)
  in
  let empty_modifiers = Transled_modifiers.empty in
  let modifiers = List.fold_left step empty_modifiers annots in
  (* Since [yielding] is the default mode in presence of [local],
     the [global] modifier must also apply [unyielding] unless specified. *)
  let modifiers =
    match
      ( Crossing.proj (Comonadic Yielding) modifiers.crossing,
        Crossing.proj (Comonadic Areality) modifiers.crossing )
    with
    | None, Some { txt = Locality.Const.Global; _ } ->
      Transled_modifiers.set ~axis:(Modal (Comonadic Yielding)) modifiers
        (Some { txt = Yielding.Const.Unyielding; loc = Location.none })
    | _, _ -> modifiers
  in
  (* Likewise, [immutable] => [contended], [read] => [shared]. *)
  let modifiers =
    match
      ( Transled_modifiers.get ~axis:(Modal (Monadic Contention)) modifiers,
        Transled_modifiers.get ~axis:(Modal (Monadic Visibility)) modifiers )
    with
    | None, Some { txt = Visibility.Const.Immutable; _ } ->
      Transled_modifiers.set ~axis:(Modal (Monadic Contention)) modifiers
        (Some { txt = Contention.Const.Contended; loc = Location.none })
    | None, Some { txt = Visibility.Const.Read; _ } ->
      Transled_modifiers.set ~axis:(Modal (Monadic Contention)) modifiers
        (Some { txt = Contention.Const.Shared; loc = Location.none })
    | _, _ -> modifiers
  in
  (* Likewise, [stateless] => [portable]. *)
  let modifiers =
    match
      ( Transled_modifiers.get ~axis:(Modal (Comonadic Portability)) modifiers,
        Transled_modifiers.get ~axis:(Modal (Comonadic Statefulness)) modifiers
      )
    with
    | None, Some { txt = Statefulness.Const.Stateless; _ } ->
      Transled_modifiers.set ~axis:(Modal (Comonadic Portability)) modifiers
        (Some { txt = Portability.Const.Portable; loc = Location.none })
    | _, _ -> modifiers
  in
  let open Types.Jkind_mod_bounds in
  let crossing = modifiers.crossing in
  let externality =
    Option.fold ~some:Location.get_txt ~none:Externality.max
      modifiers.externality
  in
  let nullability =
    Option.fold ~some:Location.get_txt ~none:Nullability.max
      modifiers.nullability
  in
  let separability =
    Option.fold ~some:Location.get_txt ~none:Separability.max
      modifiers.separability
  in
  create ~crossing ~externality ~nullability ~separability

let default_mode_annots (annots : Alloc.Const.Option.t) =
  (* [yielding] has a different default depending on whether [areality]
     is [global] or [local]. *)
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
    | None, Some Statefulness.Const.(Observing | Stateful) ->
      Some Portability.Const.Nonportable
  in
  { annots with yielding; contention; portability }

let transl_mode_annots annots : Alloc.Const.Option.t =
  let step modes_so_far { txt; loc } =
    Language_extension.assert_enabled ~loc Mode Stable;
    let (P (ax, a)) = Modal_axis_pair.of_string txt in
    if Option.is_some (Transled_modifiers.get ~axis modifiers_so_far)
    then raise (Error (annot.loc, Duplicated_axis axis));
    Alloc.Const.Option.set ax a modes_so_far
  in
  let empty_modes = Alloc.Const.Option.none in
  List.fold_left step empty_modes annots

let untransl_mode_annots (modes : Mode.Alloc.Const.Option.t) =
  let print_to_string_opt print a = Option.map (Format.asprintf "%a" print) a in
  (* Untranslate [areality] and [yielding]. *)
  let areality = print_to_string_opt Mode.Locality.Const.print modes.areality in
  let yielding =
    (* Since [yielding] has non-standard defaults, we special-case
       whether we want to print it here. *)
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
    | ( Some Statefulness.Const.(Observing | Stateful),
        Some Portability.Const.Nonportable ) ->
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
      yielding;
      statefulness;
      visibility ]

let transl_modality ~maturity { txt = Parsetree.Modality modality; loc } =
  Language_extension.assert_enabled ~loc Mode maturity;
  let (P (ax, a)) = Modal_axis_pair.of_string modality in
  let atom =
    match ax with
    | Modal_axis_pair (Comonadic Areality, mode) ->
      Modality.Atom
        (Comonadic Areality, Meet_with (Const.locality_as_regionality mode))
    | Comonadic _ -> Modality.Atom (ax, Meet_with a)
    | Monadic _ -> Modality.Atom (ax, Join_with a)
  in
  atom, loc

let untransl_modality (a : Modality.t) : Parsetree.modality loc =
  let s =
    match a with
    | Atom (ax, Meet_with a) -> Format.vsaprintf "%a" (Value.Const.print ax) a
    | Atom (ax, Join_with a) -> Format.vsaprintf "%a" (Value.Const.print ax) a
  in
  { txt = Modality s; loc = Location.none }

(* For now, mutable implies legacy modalities for both comonadic axes and
   monadic axes. In the future, implications on the comonadic axes will be
   removed. The implications on the monadic axes will stay. Implied modalities
   can be overriden. *)
(* CR zqian: decouple mutable and comonadic modalities *)
let mutable_implied_modalities ~for_mutable_variable (mut : Types.mutability) =
  let comonadic : Modality.t list =
    [ Atom (Comonadic Areality, Meet_with Regionality.Const.legacy);
      Atom (Comonadic Linearity, Meet_with Linearity.Const.legacy);
      Atom (Comonadic Portability, Meet_with Portability.Const.legacy);
      Atom (Comonadic Yielding, Meet_with Yielding.Const.legacy);
      Atom (Comonadic Statefulness, Meet_with Statefulness.Const.legacy) ]
  in
  let monadic : Modality.t list =
    [ Atom (Monadic Uniqueness, Join_with Uniqueness.Const.legacy);
      Atom (Monadic Contention, Join_with Contention.Const.legacy);
      Atom (Monadic Visibility, Join_with Visibility.Const.legacy) ]
  in
  match mut with
  | Immutable -> []
  | Mutable _ -> if for_mutable_variable then monadic else monadic @ comonadic

let mutable_implied_modalities ~for_mutable_variable (mut : Types.mutability) =
  let l = mutable_implied_modalities ~for_mutable_variable mut in
  List.fold_left
    (fun t (Modality.Atom (ax, a)) -> Modality.Value.Const.set ax a t)
    Modality.Value.Const.id l

(* Since [yielding] is the default mode in presence of [local],
   the [global] modality must also apply [unyielding] unless specified.

   Similarly for [visibility]/[contention] and [statefulness]/[portability]. *)
let implied_modalities (Atom (ax, a) : Modality.t) : Modality.t list =
  match ax, a with
  | Comonadic Areality, Meet_with a ->
    let b : Yielding.Const.t =
      match a with
      | Global -> Unyielding
      | Local -> Yielding
      | Regional -> assert false
    in
    [Atom (Comonadic Yielding, Meet_with b)]
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
      match a with Stateless -> Portable | Stateful | Observing -> Nonportable
    in
    [Atom (Comonadic Portability, Meet_with b)]
  | _ -> []

let least_modalities_implying mut (t : Modality.Value.Const.t) =
  let baseline = mutable_implied_modalities ~for_mutable_variable:false mut in
  let annotated = Modality.Value.Const.(diff baseline t) in
  let implied = List.concat_map implied_modalities annotated in
  let exclude_implied =
    List.filter (fun x -> not @@ List.mem x implied) annotated
  in
  let overridden =
    List.filter_map
      (fun (Modality.Atom (ax, m_implied)) ->
        let m_projected = Modality.Value.Const.proj ax t in
        if m_projected <> m_implied
        then Some (Modality.Atom (ax, m_projected))
        else None)
      implied
  in
  exclude_implied @ overridden

let sort_dedup_modalities ~warn l =
  let compare (Modality.Atom (ax0, _), _) (Modality.Atom (ax1, _), _) =
    Value.Axis.compare ax0 ax1
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
  let on_dup (Modality.Atom (ax0, _), loc0) (a1, _) =
    if warn
    then
      let axis = Format.asprintf "%a" Value.Axis.print ax0 in
      let { txt = Modality overriden_by; _ } = untransl_modality a1 in
      Location.prerr_warning loc0
        (Warnings.Modal_axis_specified_twice { axis; overriden_by })
  in
  l |> List.stable_sort compare |> dedup ~on_dup |> List.map fst

let transl_modalities ~maturity mut modalities =
  let mut_modalities =
    mutable_implied_modalities mut ~for_mutable_variable:false
  in
  let modalities = List.map (transl_modality ~maturity) modalities in
  (* axes listed in the order of implication. *)
  let modalities = sort_dedup_modalities ~warn:true modalities in
  let open Modality in
  (* - mut_modalities is applied before explicit modalities.
     - explicit modalities can override mut_modalities.
     - For the same axis, later modalities overrides earlier modalities. *)
  List.fold_left
    (fun m (Atom (ax, a) as t) ->
      let m = Value.Const.set ax a m in
      List.fold_left
        (fun m (Atom (ax, a)) -> Value.Const.set ax a m)
        m (implied_modalities t))
    mut_modalities modalities

let let_mutable_modalities m0 =
  mutable_implied_modalities (Mutable m0) ~for_mutable_variable:true

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
  | Duplicated_axis axis ->
    fprintf ppf "The %s axis has already been specified." (Axis.name axis)
  | Unrecognized_modifier (annot_type, modifier) ->
    let annot_type_str =
      match annot_type with
      | Modifier -> "modifier"
      | Mode -> "mode"
      | Modality -> "modality"
    in
    fprintf ppf "Unrecognized %s %s." annot_type_str modifier

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
