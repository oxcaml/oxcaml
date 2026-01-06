open Axis_lattice

let base_boxed : boxed = to_boxed bot

let check_roundtrip label boxed =
  let roundtripped = to_boxed (of_boxed boxed) in
  if roundtripped <> boxed
  then failwith (Format.asprintf "axis roundtrip failed: %s" label)

let check_values label update values =
  List.iter (fun value -> check_roundtrip label (update value)) values

let () =
  check_values "areality"
    (fun areality -> { base_boxed with areality })
    [ Mode.Regionality.Const.Global;
      Mode.Regionality.Const.Regional;
      Mode.Regionality.Const.Local
    ];
  check_values "linearity"
    (fun linearity -> { base_boxed with linearity })
    [ Mode.Linearity.Const.Many; Mode.Linearity.Const.Once ];
  check_values "uniqueness"
    (fun uniqueness -> { base_boxed with uniqueness })
    [ Mode.Uniqueness.Const.Unique; Mode.Uniqueness.Const.Aliased ];
  check_values "portability"
    (fun portability -> { base_boxed with portability })
    [ Mode.Portability.Const.Portable;
      Mode.Portability.Const.Shareable;
      Mode.Portability.Const.Nonportable
    ];
  check_values "contention"
    (fun contention -> { base_boxed with contention })
    [ Mode.Contention.Const.Uncontended;
      Mode.Contention.Const.Shared;
      Mode.Contention.Const.Contended
    ];
  check_values "forkable"
    (fun forkable -> { base_boxed with forkable })
    [ Mode.Forkable.Const.Forkable; Mode.Forkable.Const.Unforkable ];
  check_values "yielding"
    (fun yielding -> { base_boxed with yielding })
    [ Mode.Yielding.Const.Unyielding; Mode.Yielding.Const.Yielding ];
  check_values "statefulness"
    (fun statefulness -> { base_boxed with statefulness })
    [ Mode.Statefulness.Const.Stateless;
      Mode.Statefulness.Const.Observing;
      Mode.Statefulness.Const.Stateful
    ];
  check_values "visibility"
    (fun visibility -> { base_boxed with visibility })
    [ Mode.Visibility.Const.Immutable;
      Mode.Visibility.Const.Read;
      Mode.Visibility.Const.Read_write
    ];
  check_values "staticity"
    (fun staticity -> { base_boxed with staticity })
    [ Mode.Staticity.Static; Mode.Staticity.Dynamic ];
  check_values "externality"
    (fun externality -> { base_boxed with externality })
    [ Jkind_axis.Externality.External;
      Jkind_axis.Externality.External64;
      Jkind_axis.Externality.Internal
    ];
  check_values "nullability"
    (fun nullability -> { base_boxed with nullability })
    [ Jkind_axis.Nullability.Non_null; Jkind_axis.Nullability.Maybe_null ];
  check_values "separability"
    (fun separability -> { base_boxed with separability })
    [ Jkind_axis.Separability.Non_float;
      Jkind_axis.Separability.Separable;
      Jkind_axis.Separability.Maybe_separable
    ]
