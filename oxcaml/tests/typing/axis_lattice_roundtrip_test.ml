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

let check_roundtrip label sample =
  let roundtripped = sample_of_lattice (lattice_of_sample sample) in
  if roundtripped <> sample
  then failwith (Format.asprintf "axis roundtrip failed: %s" label)

let check_values label update values =
  List.iter
    (fun value ->
      List.iter
        (fun base_sample -> check_roundtrip label (update base_sample value))
        base_samples)
    values

let all_axis_sets =
  List.fold_right
    (fun (Jkind_axis.Axis.Pack axis) sets ->
      List.concat_map
        (fun set -> [set; Jkind_axis.Axis_set.add set axis])
        sets)
    Jkind_axis.Axis.all
    [Jkind_axis.Axis_set.empty]

let check_of_axis_set' () =
  List.iter
    (fun set ->
      let expected = of_axis_set set in
      let actual = of_axis_set' set in
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
      Mode.Statefulness.Const.Observing;
      Mode.Statefulness.Const.Stateful ];
  check_values "visibility"
    (fun sample visibility -> { sample with visibility })
    [ Mode.Visibility.Const.Immutable;
      Mode.Visibility.Const.Read;
      Mode.Visibility.Const.Read_write ];
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
  check_of_axis_set' ()
