open Axis_lattice

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

let base_bot_sample = sample_of_lattice bot

let check_roundtrip label sample =
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

let check_values label update values =
  List.iter
    (fun value ->
      List.iter
        (fun base_sample -> check_roundtrip label (update base_sample value))
        base_samples)
    values

let check_mod_bounds_roundtrip label update values =
  List.iter
    (fun value ->
      List.iter
        (fun base_sample ->
          let sample = update base_sample value in
          let bounds = mod_bounds_of_sample sample in
          let roundtripped =
            bounds |> Btype.Jkind0.Mod_bounds.to_axis_lattice
            |> Btype.Jkind0.Mod_bounds.of_axis_lattice
          in
          if not (Btype.Jkind0.Mod_bounds.equal bounds roundtripped)
          then
            failwith (Format.asprintf "mod_bounds roundtrip failed: %s" label))
        base_samples)
    values

let level_of_statefulness = function
  | Mode.Statefulness.Const.Stateless -> 0
  | Mode.Statefulness.Const.Writing -> 1
  | Mode.Statefulness.Const.Reading -> 2
  | Mode.Statefulness.Const.Stateful -> 3

let statefulness_of_level = function
  | 0 -> Mode.Statefulness.Const.Stateless
  | 1 -> Mode.Statefulness.Const.Writing
  | 2 -> Mode.Statefulness.Const.Reading
  | 3 -> Mode.Statefulness.Const.Stateful
  | _ -> invalid_arg "statefulness_of_level"

let level_of_visibility = function
  | Mode.Visibility.Const.Immutable -> 0
  | Mode.Visibility.Const.Read -> 1
  | Mode.Visibility.Const.Write -> 2
  | Mode.Visibility.Const.Read_write -> 3

let visibility_of_level = function
  | 0 -> Mode.Visibility.Const.Immutable
  | 1 -> Mode.Visibility.Const.Read
  | 2 -> Mode.Visibility.Const.Write
  | 3 -> Mode.Visibility.Const.Read_write
  | _ -> invalid_arg "visibility_of_level"

let statefulness_co_sub lhs rhs =
  statefulness_of_level
    (level_of_statefulness lhs land (lnot (level_of_statefulness rhs) land 0b11))

let visibility_co_sub lhs rhs =
  visibility_of_level
    (level_of_visibility lhs land (lnot (level_of_visibility rhs) land 0b11))

let check_statefulness_laws () =
  let values =
    [ Mode.Statefulness.Const.Stateless;
      Mode.Statefulness.Const.Writing;
      Mode.Statefulness.Const.Reading;
      Mode.Statefulness.Const.Stateful ]
  in
  List.iter
    (fun lhs_value ->
      List.iter
        (fun rhs_value ->
          let lhs =
            lattice_of_sample { base_bot_sample with statefulness = lhs_value }
          in
          let rhs =
            lattice_of_sample { base_bot_sample with statefulness = rhs_value }
          in
          let expect_value op_name actual expected =
            if actual <> expected
            then failwith (Format.asprintf "statefulness %s mismatch" op_name)
          in
          expect_value "join"
            (statefulness (join lhs rhs))
            (Mode.Statefulness.Const.join lhs_value rhs_value);
          expect_value "meet"
            (statefulness (meet lhs rhs))
            (Mode.Statefulness.Const.meet lhs_value rhs_value);
          expect_value "co_sub"
            (statefulness (co_sub lhs rhs))
            (statefulness_co_sub lhs_value rhs_value);
          let expected_leq = Mode.Statefulness.Const.le lhs_value rhs_value in
          if leq lhs rhs <> expected_leq
          then failwith "statefulness leq mismatch")
        values)
    values

let check_visibility_laws () =
  let values =
    [ Mode.Visibility.Const.Immutable;
      Mode.Visibility.Const.Read;
      Mode.Visibility.Const.Write;
      Mode.Visibility.Const.Read_write ]
  in
  List.iter
    (fun lhs_value ->
      List.iter
        (fun rhs_value ->
          let lhs =
            lattice_of_sample { base_bot_sample with visibility = lhs_value }
          in
          let rhs =
            lattice_of_sample { base_bot_sample with visibility = rhs_value }
          in
          let expect_value op_name actual expected =
            if actual <> expected
            then failwith (Format.asprintf "visibility %s mismatch" op_name)
          in
          expect_value "join"
            (visibility (join lhs rhs))
            (Mode.Visibility.Const.meet lhs_value rhs_value);
          expect_value "meet"
            (visibility (meet lhs rhs))
            (Mode.Visibility.Const.join lhs_value rhs_value);
          expect_value "co_sub"
            (visibility (co_sub lhs rhs))
            (visibility_co_sub lhs_value rhs_value);
          let expected_leq = Mode.Visibility.Const.le rhs_value lhs_value in
          if leq lhs rhs <> expected_leq then failwith "visibility leq mismatch")
        values)
    values

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
  check_values "areality"
    (fun sample areality -> { sample with areality })
    [ Mode.Regionality.Const.Global;
      Mode.Regionality.Const.Regional;
      Mode.Regionality.Const.Local ];
  check_values "linearity"
    (fun sample linearity -> { sample with linearity })
    [Mode.Linearity.Const.Many; Mode.Linearity.Const.Once];
  check_values "uniqueness"
    (fun sample uniqueness -> { sample with uniqueness })
    [Mode.Uniqueness.Const.Unique; Mode.Uniqueness.Const.Aliased];
  check_values "portability"
    (fun sample portability -> { sample with portability })
    [ Mode.Portability.Const.Portable;
      Mode.Portability.Const.Shareable;
      Mode.Portability.Const.Nonportable ];
  check_values "contention"
    (fun sample contention -> { sample with contention })
    [ Mode.Contention.Const.Uncontended;
      Mode.Contention.Const.Shared;
      Mode.Contention.Const.Contended ];
  check_values "forkable"
    (fun sample forkable -> { sample with forkable })
    [Mode.Forkable.Const.Forkable; Mode.Forkable.Const.Unforkable];
  check_values "yielding"
    (fun sample yielding -> { sample with yielding })
    [Mode.Yielding.Const.Unyielding; Mode.Yielding.Const.Yielding];
  check_values "statefulness"
    (fun sample statefulness -> { sample with statefulness })
    [ Mode.Statefulness.Const.Stateless;
      Mode.Statefulness.Const.Writing;
      Mode.Statefulness.Const.Reading;
      Mode.Statefulness.Const.Stateful ];
  check_values "visibility"
    (fun sample visibility -> { sample with visibility })
    [ Mode.Visibility.Const.Immutable;
      Mode.Visibility.Const.Read;
      Mode.Visibility.Const.Write;
      Mode.Visibility.Const.Read_write ];
  check_mod_bounds_roundtrip "statefulness"
    (fun sample statefulness -> { sample with statefulness })
    [ Mode.Statefulness.Const.Stateless;
      Mode.Statefulness.Const.Writing;
      Mode.Statefulness.Const.Reading;
      Mode.Statefulness.Const.Stateful ];
  check_mod_bounds_roundtrip "visibility"
    (fun sample visibility -> { sample with visibility })
    [ Mode.Visibility.Const.Immutable;
      Mode.Visibility.Const.Read;
      Mode.Visibility.Const.Write;
      Mode.Visibility.Const.Read_write ];
  check_statefulness_laws ();
  check_visibility_laws ();
  check_values "staticity"
    (fun sample staticity -> { sample with staticity })
    [Mode.Staticity.Static; Mode.Staticity.Dynamic];
  check_values "externality"
    (fun sample externality -> { sample with externality })
    [ Jkind_axis.Externality.External;
      Jkind_axis.Externality.External64;
      Jkind_axis.Externality.Internal ];
  check_values "nullability"
    (fun sample nullability -> { sample with nullability })
    [Jkind_axis.Nullability.Non_null; Jkind_axis.Nullability.Maybe_null];
  check_values "separability"
    (fun sample separability -> { sample with separability })
    [ Jkind_axis.Separability.Non_float;
      Jkind_axis.Separability.Separable;
      Jkind_axis.Separability.Maybe_separable ];
  check_of_axis_set ()
