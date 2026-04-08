open Axis_lattice

module Opposite (L : Mode_intf.Lattice) : Mode_intf.Lattice with type t = L.t =
struct
  type t = L.t

  let min = L.max

  let max = L.min

  let le a b = L.le b a

  let equal = L.equal

  let join = L.meet

  let meet = L.join

  let print = L.print
end

type sample =
  { areality : Mode.Regionality.Const.t;
    linearity : Mode.Linearity.Const.t;
    uniqueness : Mode.Uniqueness.Const.t;
    portability : Mode.Portability.Const.t;
    contention : Mode.Contention.Const.t;
    forkable : Mode.Forkable.Const.t;
    yielding : Mode.Yielding.Const.t;
    statefulness : Mode.Statefulness.Const.t;
    visibility : Mode.Visibility.Const.t;
    staticity : Mode.Staticity.const;
    externality : Jkind_axis.Externality.t;
    nullability : Jkind_axis.Nullability.t;
    separability : Jkind_axis.Separability.t
  }

let sample_of_lattice x =
  { areality = areality x;
    linearity = linearity x;
    uniqueness = uniqueness x;
    portability = portability x;
    contention = contention x;
    forkable = forkable x;
    yielding = yielding x;
    statefulness = statefulness x;
    visibility = visibility x;
    staticity = staticity x;
    externality = externality x;
    nullability = nullability x;
    separability = separability x
  }

let lattice_of_sample sample =
  create ~areality:sample.areality ~linearity:sample.linearity
    ~uniqueness:sample.uniqueness ~portability:sample.portability
    ~contention:sample.contention ~forkable:sample.forkable
    ~yielding:sample.yielding ~statefulness:sample.statefulness
    ~visibility:sample.visibility ~staticity:sample.staticity
    ~externality:sample.externality ~nullability:sample.nullability
    ~separability:sample.separability

let base_samples = [sample_of_lattice bot; sample_of_lattice top]

let check_lattic_roundtrip label sample =
  let roundtripped = sample_of_lattice (lattice_of_sample sample) in
  if roundtripped <> sample
  then failwith (Format.asprintf "axis roundtrip failed: %s" label)

let mod_bounds_of_sample sample =
  let monadic =
    Mode.Crossing.Monadic.create
      ~uniqueness:
        (Mode.Crossing.Monadic.Atom.Modality
           (Mode.Modality.Monadic.Atom.Join_const sample.uniqueness))
      ~contention:
        (Mode.Crossing.Monadic.Atom.Modality
           (Mode.Modality.Monadic.Atom.Join_const sample.contention))
      ~visibility:
        (Mode.Crossing.Monadic.Atom.Modality
           (Mode.Modality.Monadic.Atom.Join_const sample.visibility))
      ~staticity:
        (Mode.Crossing.Monadic.Atom.Modality
           (Mode.Modality.Monadic.Atom.Join_const sample.staticity))
  in
  let comonadic =
    Mode.Crossing.Comonadic.create
      ~regionality:
        (Mode.Crossing.Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_const sample.areality))
      ~linearity:
        (Mode.Crossing.Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_const sample.linearity))
      ~portability:
        (Mode.Crossing.Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_const sample.portability))
      ~forkable:
        (Mode.Crossing.Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_const sample.forkable))
      ~yielding:
        (Mode.Crossing.Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_const sample.yielding))
      ~statefulness:
        (Mode.Crossing.Comonadic.Atom.Modality
           (Mode.Modality.Comonadic.Atom.Meet_const sample.statefulness))
  in
  Btype.Jkind0.Mod_bounds.create { monadic; comonadic }
    ~externality:sample.externality ~nullability:sample.nullability
    ~separability:sample.separability

let check_mod_bounds_roundtrip label sample =
  let bounds = mod_bounds_of_sample sample in
  let roundtripped =
    bounds |> Btype.Jkind0.Mod_bounds.to_axis_lattice
    |> Btype.Jkind0.Mod_bounds.of_axis_lattice
  in
  if not (Btype.Jkind0.Mod_bounds.equal bounds roundtripped)
  then failwith (Format.asprintf "mod_bounds roundtrip failed: %s" label)

let check_roundtripping label update values =
  List.iter
    (fun value ->
      List.iter
        (fun base_sample ->
          let sample = update base_sample value in
          check_lattic_roundtrip label sample;
          check_mod_bounds_roundtrip label sample)
        base_samples)
    values

let co_sub_reference_impl (type t)
    (module Axis : Mode_intf.Lattice with type t = t) (all_values : t list)
    (a : t) (b : t) =
  (* co_sub(a, b) = meet { c | a <= join(b, c) } *)
  List.fold_left
    (fun acc c -> if Axis.le a (Axis.join b c) then Axis.meet acc c else acc)
    Axis.max all_values

let check_operations (type t) (module Axis : Mode_intf.Lattice with type t = t)
    label update extract (all_values : t list) =
  List.iter
    (fun lhs_value ->
      List.iter
        (fun rhs_value ->
          List.iter
            (fun base_sample ->
              let lhs = lattice_of_sample (update base_sample lhs_value) in
              let rhs = lattice_of_sample (update base_sample rhs_value) in
              let expect_value op_name actual expected =
                if not (Axis.equal actual expected)
                then
                  failwith
                    (Format_doc.asprintf
                       "%s %s mismatch; expected=%a, actual=%a (lhs=%a, rhs=%a)"
                       label op_name Axis.print expected Axis.print actual
                       Axis.print lhs_value Axis.print rhs_value)
              in
              expect_value "join"
                (extract (join lhs rhs))
                (Axis.join lhs_value rhs_value);
              expect_value "meet"
                (extract (meet lhs rhs))
                (Axis.meet lhs_value rhs_value);
              expect_value "co_sub"
                (extract (co_sub lhs rhs))
                (co_sub_reference_impl
                   (module Axis)
                   all_values lhs_value rhs_value);
              let expected_leq = Axis.le lhs_value rhs_value in
              if leq lhs rhs <> expected_leq
              then failwith "statefulness leq mismatch")
            base_samples)
        all_values)
    all_values

let check_axis axis label update extract all_values =
  check_roundtripping label update all_values;
  check_operations axis label update extract all_values

let all_axis_sets =
  List.fold_right
    (fun (Jkind_axis.Axis.Pack axis) sets ->
      List.concat_map (fun set -> [set; Jkind_axis.Axis_set.add set axis]) sets)
    Jkind_axis.Axis.all
    [Jkind_axis.Axis_set.empty]

let mask_of_axis : type a. a Jkind_axis.Axis.t -> t =
 fun axis ->
  let open Jkind_axis.Axis in
  let open Mode.Axis in
  let open Mode.Crossing.Axis in
  let sample = sample_of_lattice bot in
  match axis with
  | Modal (Comonadic Areality) ->
    lattice_of_sample { sample with areality = Mode.Regionality.Const.Local }
  | Modal (Monadic Uniqueness) ->
    lattice_of_sample { sample with uniqueness = Mode.Uniqueness.Const.Unique }
  | Modal (Comonadic Linearity) ->
    lattice_of_sample { sample with linearity = Mode.Linearity.Const.Once }
  | Modal (Monadic Contention) ->
    lattice_of_sample
      { sample with contention = Mode.Contention.Const.Uncontended }
  | Modal (Comonadic Portability) ->
    lattice_of_sample
      { sample with portability = Mode.Portability.Const.Nonportable }
  | Modal (Comonadic Forkable) ->
    lattice_of_sample { sample with forkable = Mode.Forkable.Const.Unforkable }
  | Modal (Comonadic Yielding) ->
    lattice_of_sample { sample with yielding = Mode.Yielding.Const.Yielding }
  | Modal (Comonadic Statefulness) ->
    lattice_of_sample
      { sample with statefulness = Mode.Statefulness.Const.Stateful }
  | Modal (Monadic Visibility) ->
    lattice_of_sample
      { sample with visibility = Mode.Visibility.Const.Read_write }
  | Modal (Monadic Staticity) ->
    lattice_of_sample { sample with staticity = Mode.Staticity.Static }
  | Nonmodal Externality ->
    lattice_of_sample
      { sample with externality = Jkind_axis.Externality.Internal }
  | Nonmodal Nullability ->
    lattice_of_sample
      { sample with nullability = Jkind_axis.Nullability.Maybe_null }
  | Nonmodal Separability ->
    lattice_of_sample
      { sample with separability = Jkind_axis.Separability.Maybe_separable }

let of_axis_set' (set : Jkind_axis.Axis_set.t) : t =
  Jkind_axis.Axis_set.to_seq set
  |> Seq.fold_left
       (fun acc (Jkind_axis.Axis.Pack axis) -> join acc (mask_of_axis axis))
       bot

let check_of_axis_set () =
  List.iter
    (fun set ->
      let expected = of_axis_set' set in
      let actual = of_axis_set set in
      if expected <> actual
      then
        failwith
          (Format.asprintf
             "axis set conversion mismatch for %a: expected %s, got %s"
             Jkind_axis.Axis_set.print set (to_string expected)
             (to_string actual)))
    all_axis_sets

let () =
  check_axis
    (module Mode.Regionality.Const)
    "areality"
    (fun sample areality -> { sample with areality })
    areality
    [ Mode.Regionality.Const.Global;
      Mode.Regionality.Const.Regional;
      Mode.Regionality.Const.Local ];
  check_axis
    (module Mode.Linearity.Const)
    "linearity"
    (fun sample linearity -> { sample with linearity })
    linearity
    [Mode.Linearity.Const.Many; Mode.Linearity.Const.Once];
  check_axis
    (module Opposite (Mode.Uniqueness.Const))
    "uniqueness"
    (fun sample uniqueness -> { sample with uniqueness })
    uniqueness
    [Mode.Uniqueness.Const.Unique; Mode.Uniqueness.Const.Aliased];
  check_axis
    (module Mode.Portability.Const)
    "portability"
    (fun sample portability -> { sample with portability })
    portability
    [ Mode.Portability.Const.Portable;
      Mode.Portability.Const.Shareable;
      Mode.Portability.Const.Corruptible;
      Mode.Portability.Const.Nonportable ];
  check_axis
    (module Opposite (Mode.Contention.Const))
    "contention"
    (fun sample contention -> { sample with contention })
    contention
    [ Mode.Contention.Const.Uncontended;
      Mode.Contention.Const.Corrupted;
      Mode.Contention.Const.Shared;
      Mode.Contention.Const.Contended ];
  check_axis
    (module Mode.Forkable.Const)
    "forkable"
    (fun sample forkable -> { sample with forkable })
    forkable
    [Mode.Forkable.Const.Forkable; Mode.Forkable.Const.Unforkable];
  check_axis
    (module Mode.Yielding.Const)
    "yielding"
    (fun sample yielding -> { sample with yielding })
    yielding
    [Mode.Yielding.Const.Unyielding; Mode.Yielding.Const.Yielding];
  check_axis
    (module Mode.Statefulness.Const)
    "statefulness"
    (fun sample statefulness -> { sample with statefulness })
    statefulness
    [ Mode.Statefulness.Const.Stateless;
      Mode.Statefulness.Const.Writing;
      Mode.Statefulness.Const.Reading;
      Mode.Statefulness.Const.Stateful ];
  check_axis
    (module Opposite (Mode.Visibility.Const))
    "visibility"
    (fun sample visibility -> { sample with visibility })
    visibility
    [ Mode.Visibility.Const.Immutable;
      Mode.Visibility.Const.Read;
      Mode.Visibility.Const.Write;
      Mode.Visibility.Const.Read_write ];
  check_axis
    (module Opposite (Mode.Staticity.Const))
    "staticity"
    (fun sample staticity -> { sample with staticity })
    staticity
    [Mode.Staticity.Static; Mode.Staticity.Dynamic];
  check_axis
    (module Jkind_axis.Externality)
    "externality"
    (fun sample externality -> { sample with externality })
    externality
    [ Jkind_axis.Externality.External;
      Jkind_axis.Externality.External64;
      Jkind_axis.Externality.Internal ];
  check_axis
    (module Jkind_axis.Nullability)
    "nullability"
    (fun sample nullability -> { sample with nullability })
    nullability
    [Jkind_axis.Nullability.Non_null; Jkind_axis.Nullability.Maybe_null];
  check_axis
    (module Jkind_axis.Separability)
    "separability"
    (fun sample separability -> { sample with separability })
    separability
    [ Jkind_axis.Separability.Non_float;
      Jkind_axis.Separability.Separable;
      Jkind_axis.Separability.Maybe_separable ];
  check_of_axis_set ()
