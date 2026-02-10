open Axis_lattice

let base_boxeds = [to_boxed bot; to_boxed top]

let check_roundtrip label boxed =
  let roundtripped = to_boxed (of_boxed boxed) in
  if roundtripped <> boxed
  then
    failwith
      (Format.asprintf "axis roundtrip failed: %s" label)

let check_values label update values =
  List.iter
    (fun value ->
      List.iter
        (fun base_boxed -> check_roundtrip label (update base_boxed value))
        base_boxeds)
    values

let () =
  check_values "areality"
    (fun base_boxed areality -> { base_boxed with areality })
    [ Mode.Regionality.Const.Global;
      Mode.Regionality.Const.Regional;
      Mode.Regionality.Const.Local
    ];
  check_values "linearity"
    (fun base_boxed linearity -> { base_boxed with linearity })
    [ Mode.Linearity.Const.Many; Mode.Linearity.Const.Once ];
  check_values "uniqueness"
    (fun base_boxed uniqueness -> { base_boxed with uniqueness })
    [ Mode.Uniqueness.Const.Unique; Mode.Uniqueness.Const.Aliased ];
  check_values "portability"
    (fun base_boxed portability -> { base_boxed with portability })
    [ Mode.Portability.Const.Portable;
      Mode.Portability.Const.Shareable;
      Mode.Portability.Const.Nonportable
    ];
  check_values "contention"
    (fun base_boxed contention -> { base_boxed with contention })
    [ Mode.Contention.Const.Uncontended;
      Mode.Contention.Const.Shared;
      Mode.Contention.Const.Contended
    ];
  check_values "forkable"
    (fun base_boxed forkable -> { base_boxed with forkable })
    [ Mode.Forkable.Const.Forkable; Mode.Forkable.Const.Unforkable ];
  check_values "yielding"
    (fun base_boxed yielding -> { base_boxed with yielding })
    [ Mode.Yielding.Const.Unyielding; Mode.Yielding.Const.Yielding ];
  check_values "statefulness"
    (fun base_boxed statefulness -> { base_boxed with statefulness })
    [ Mode.Statefulness.Const.Stateless;
      Mode.Statefulness.Const.Observing;
      Mode.Statefulness.Const.Stateful
    ];
  check_values "visibility"
    (fun base_boxed visibility -> { base_boxed with visibility })
    [ Mode.Visibility.Const.Immutable;
      Mode.Visibility.Const.Read;
      Mode.Visibility.Const.Read_write
    ];
  check_values "staticity"
    (fun base_boxed staticity -> { base_boxed with staticity })
    [ Mode.Staticity.Static; Mode.Staticity.Dynamic ];
  check_values "externality"
    (fun base_boxed externality -> { base_boxed with externality })
    [ Jkind_axis.Externality.External;
      Jkind_axis.Externality.External64;
      Jkind_axis.Externality.Internal
    ];
  check_values "nullability"
    (fun base_boxed nullability -> { base_boxed with nullability })
    [ Jkind_axis.Nullability.Non_null; Jkind_axis.Nullability.Maybe_null ];
  check_values "separability"
    (fun base_boxed separability -> { base_boxed with separability })
    [ Jkind_axis.Separability.Non_float;
      Jkind_axis.Separability.Separable;
      Jkind_axis.Separability.Maybe_separable
    ]
