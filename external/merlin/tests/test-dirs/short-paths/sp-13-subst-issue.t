This mocks the Async --include--> Async_kernel --exports--> Deferred

  $ mkdir async_kernel
  $ cd async_kernel


  $ cat >async_kernel__.ml <<'EOF'
  > module Deferred = Async_kernel__Deferred
  > module Deferred0 = Async_kernel__Deferred0
  > EOF

  $ $OCAMLC -c async_kernel__.ml -no-alias-deps 2>/dev/null


  $ cat >deferred0.ml <<'EOF'
  > type +'a t = 'a
  > let create : 'a -> 'a t = Fun.id
  > EOF

  $ cat >deferred0.mli <<'EOF'
  > type +'a t 
  > val create : 'a -> 'a t
  > EOF

  $ $OCAMLC -c deferred0.mli -open Async_kernel__ -o Async_kernel__Deferred0
  $ $OCAMLC -c deferred0.ml -open Async_kernel__ -o Async_kernel__Deferred0

  $ cat >deferred.ml <<'EOF'
  > type +'a t = 'a Deferred0.t
  > 
  > module Let_syntax = struct 
  >   module Let_syntax : sig val return : 'a -> 'a t end = struct let return x = Deferred0.create x end
  > end
  > EOF

  $ $OCAMLC -c deferred.ml -open Async_kernel__ -o Async_kernel__Deferred


  $ cat >async_kernel.ml <<'EOF'
  > module Deferred = Deferred
  > module Let_syntax = Deferred.Let_syntax.Let_syntax (* This the use the Deferred that should end in the discourse ? *)
  > EOF

  $ $OCAMLC -c async_kernel.ml -open Async_kernel__


  $ cd ..
  $ mkdir async
  $ cd async

  $ cat >async.ml <<'EOF'
  > module Deferred = Async_kernel.Deferred (* FIXME This should be used to rewrite Async_kernel.Deferred.t *)
  > module Let_syntax = Async_kernel.Let_syntax
  > EOF

  $ $OCAMLC -c async.ml -I ../async_kernel

  $ cd ..

  $ cat >test.ml <<'EOF'
  > open! Async
  > 
  > let foo = Let_syntax.return 5
  > EOF


  $ $OCAMLC -c test.ml  -I async -I async_kernel

  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > B async
  > B async_kernel
  > EOF

  $ $MERLIN single type-enclosing -nostdlib -position 3:5 \
  > -log-file - -log-section discourse-recap \
  > -filename test.ml 2>&1 <test.ml | sed -E 's/^# [0-9]+.[0-9]+/#/'
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [Async -> [{item = (module, Async!); env = with env}];
    Let_syntax -> [{item = (module, Async!.Let_syntax); env = with env}];
    Let_syntax.return ->
      [{item = (value, Async!.Let_syntax.return); env = with env}]];
    substs =
    [Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax];
    Async_kernel.Deferred -> [Deferred];
    Async_kernel.Let_syntax -> [Let_syntax]] }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Async_kernel -> [{item = (module, Async_kernel!); env = with env}];
    Let_syntax ->
      [{item = (module, Async_kernel__Deferred!.Let_syntax.Let_syntax);
        env = with env}];
    Async_kernel.Let_syntax ->
      [{item = (module, Async_kernel!.Let_syntax); env = with env}]];
    substs =
    [] }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Deferred -> [{item = (module, Deferred[1]); env = with env}];
    Async_kernel.Let_syntax ->
      [{item = (module, Async_kernel__Deferred!.Let_syntax.Let_syntax);
        env = with env}];
    Deferred.Let_syntax ->
      [{item = (module, Deferred[1].Let_syntax); env = with env}];
    Deferred.Let_syntax.Let_syntax ->
      [{item = (module, Deferred[1].Let_syntax.Let_syntax); env = with env}]];
    substs =
    [] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 156;
    paths =
    unit [unit!];
    unativeint_u [unativeint_u!];
    uint8_u [uint8_u!];
    uint64_u [uint64_u!];
    uint32_u [uint32_u!];
    uint16_u [uint16_u!];
    t [t[1]];
    string [string!];
    or_null [or_null!];
    option [option!];
    nativeint_u [nativeint_u!];
    nativeint [nativeint!];
    mask [mask!];
    list [list!];
    lexing_position [lexing_position!];
    lazy_t [lazy_t!];
    int8x64 [int8x64!];
    int8x32 [int8x32!];
    int8x16 [int8x16!];
    int8 [int8!];
    int64x8 [int64x8!];
    int64x4 [int64x4!];
    int64x2 [int64x2!];
    int64_u [int64_u!];
    int64 [int64!];
    int32x8 [int32x8!];
    int32x4 [int32x4!];
    int32x16 [int32x16!];
    int32_u [int32_u!];
    int32 [int32!];
    int16x8 [int16x8!];
    int16x32 [int16x32!];
    int16x16 [int16x16!];
    int16 [int16!];
    int [int!];
    idx_mut [idx_mut!];
    idx_imm [idx_imm!];
    idx_atomic [idx_atomic!];
    iarray [iarray!];
    floatarray [floatarray!];
    float64x8 [float64x8!];
    float64x4 [float64x4!];
    float64x2 [float64x2!];
    float32x8 [float32x8!];
    float32x4 [float32x4!];
    float32x16 [float32x16!];
    float32_u [float32_u!];
    float32 [float32!];
    float16x8 [float16x8!];
    float16x32 [float16x32!];
    float16x16 [float16x16!];
    float [float!];
    extension_constructor [extension_constructor!];
    expr [expr!];
    exn [exn!];
    eval [eval!];
    eff [eff!];
    continuation [continuation!];
    char [char!];
    bytes [bytes!];
    box [box!];
    bool [bool!];
    atomic_loc [atomic_loc!];
    array [array!];
    Let_syntax
      [Async!.Let_syntax; Async_kernel__Deferred!.Let_syntax.Let_syntax];
    Let_syntax.return
      [Async!.Let_syntax.return;
       Async_kernel__Deferred!.Let_syntax.Let_syntax.return];
    Deferred [Deferred[1]; Async!.Deferred];
    Deferred.Let_syntax [Deferred[1].Let_syntax];
    Deferred.Let_syntax.Let_syntax [Deferred[1].Let_syntax.Let_syntax];
    Async_kernel [Async_kernel!];
    Async_kernel.Let_syntax
      [Async_kernel!.Let_syntax; Async_kernel__Deferred!.Let_syntax.Let_syntax];
    Async_kernel.Let_syntax.return
      [Async_kernel__Deferred!.Let_syntax.Let_syntax.return];
    Async_kernel.Deferred [Async_kernel!.Deferred];
    Async [Async!];
    Async.Let_syntax [Async!.Let_syntax];
    Async.Deferred [Async!.Deferred];
    substs =
    [Async_kernel__Deferred -> [Async.Deferred; Async_kernel.Deferred];
    Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax; Async_kernel.Let_syntax];
    Async_kernel.Deferred -> [Deferred];
    Async_kernel.Let_syntax -> [Let_syntax; Deferred.Let_syntax.Let_syntax];
    Async_kernel__.Deferred -> [Async.Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax; Async_kernel.Let_syntax];
    Async_kernel.Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax]]
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [Async_kernel__Deferred -> [Async.Deferred; Async_kernel.Deferred];
    Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax; Async_kernel.Let_syntax];
    Async_kernel.Deferred -> [Deferred];
    Async_kernel.Let_syntax -> [Let_syntax; Deferred.Let_syntax.Let_syntax];
    Async_kernel__.Deferred -> [Async.Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax; Async_kernel.Let_syntax];
    Async_kernel.Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax]] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    [Async_kernel__Deferred -> [Async.Deferred; Async_kernel.Deferred];
    Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax; Async_kernel.Let_syntax];
    Async_kernel.Deferred -> [Deferred];
    Async_kernel.Let_syntax -> [Let_syntax; Deferred.Let_syntax.Let_syntax];
    Async_kernel__.Deferred -> [Async.Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax; Async_kernel.Let_syntax];
    Async_kernel.Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax]]
    }
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 7
        },
        "type": "int Deferred.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }

Dump the discourse so regressions show up as a diff in this test:

  $ $MERLIN single type-enclosing -nostdlib -position 3:5 \
  > -log-file - -log-section discourse-recap \
  > -filename test.ml < test.ml 2>&1| sed -E 's/^# [0-9]+.[0-9]+/#/'
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [Async -> [{item = (module, Async!); env = with env}];
    Let_syntax -> [{item = (module, Async!.Let_syntax); env = with env}];
    Let_syntax.return ->
      [{item = (value, Async!.Let_syntax.return); env = with env}]];
    substs =
    [Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax];
    Async_kernel.Deferred -> [Deferred];
    Async_kernel.Let_syntax -> [Let_syntax]] }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Async_kernel -> [{item = (module, Async_kernel!); env = with env}];
    Let_syntax ->
      [{item = (module, Async_kernel__Deferred!.Let_syntax.Let_syntax);
        env = with env}];
    Async_kernel.Let_syntax ->
      [{item = (module, Async_kernel!.Let_syntax); env = with env}]];
    substs =
    [] }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Deferred -> [{item = (module, Deferred[1]); env = with env}];
    Async_kernel.Let_syntax ->
      [{item = (module, Async_kernel__Deferred!.Let_syntax.Let_syntax);
        env = with env}];
    Deferred.Let_syntax ->
      [{item = (module, Deferred[1].Let_syntax); env = with env}];
    Deferred.Let_syntax.Let_syntax ->
      [{item = (module, Deferred[1].Let_syntax.Let_syntax); env = with env}]];
    substs =
    [] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 156;
    paths =
    unit [unit!];
    unativeint_u [unativeint_u!];
    uint8_u [uint8_u!];
    uint64_u [uint64_u!];
    uint32_u [uint32_u!];
    uint16_u [uint16_u!];
    t [t[1]];
    string [string!];
    or_null [or_null!];
    option [option!];
    nativeint_u [nativeint_u!];
    nativeint [nativeint!];
    mask [mask!];
    list [list!];
    lexing_position [lexing_position!];
    lazy_t [lazy_t!];
    int8x64 [int8x64!];
    int8x32 [int8x32!];
    int8x16 [int8x16!];
    int8 [int8!];
    int64x8 [int64x8!];
    int64x4 [int64x4!];
    int64x2 [int64x2!];
    int64_u [int64_u!];
    int64 [int64!];
    int32x8 [int32x8!];
    int32x4 [int32x4!];
    int32x16 [int32x16!];
    int32_u [int32_u!];
    int32 [int32!];
    int16x8 [int16x8!];
    int16x32 [int16x32!];
    int16x16 [int16x16!];
    int16 [int16!];
    int [int!];
    idx_mut [idx_mut!];
    idx_imm [idx_imm!];
    idx_atomic [idx_atomic!];
    iarray [iarray!];
    floatarray [floatarray!];
    float64x8 [float64x8!];
    float64x4 [float64x4!];
    float64x2 [float64x2!];
    float32x8 [float32x8!];
    float32x4 [float32x4!];
    float32x16 [float32x16!];
    float32_u [float32_u!];
    float32 [float32!];
    float16x8 [float16x8!];
    float16x32 [float16x32!];
    float16x16 [float16x16!];
    float [float!];
    extension_constructor [extension_constructor!];
    expr [expr!];
    exn [exn!];
    eval [eval!];
    eff [eff!];
    continuation [continuation!];
    char [char!];
    bytes [bytes!];
    box [box!];
    bool [bool!];
    atomic_loc [atomic_loc!];
    array [array!];
    Let_syntax
      [Async!.Let_syntax; Async_kernel__Deferred!.Let_syntax.Let_syntax];
    Let_syntax.return
      [Async!.Let_syntax.return;
       Async_kernel__Deferred!.Let_syntax.Let_syntax.return];
    Deferred [Deferred[1]; Async!.Deferred];
    Deferred.Let_syntax [Deferred[1].Let_syntax];
    Deferred.Let_syntax.Let_syntax [Deferred[1].Let_syntax.Let_syntax];
    Async_kernel [Async_kernel!];
    Async_kernel.Let_syntax
      [Async_kernel!.Let_syntax; Async_kernel__Deferred!.Let_syntax.Let_syntax];
    Async_kernel.Let_syntax.return
      [Async_kernel__Deferred!.Let_syntax.Let_syntax.return];
    Async_kernel.Deferred [Async_kernel!.Deferred];
    Async [Async!];
    Async.Let_syntax [Async!.Let_syntax];
    Async.Deferred [Async!.Deferred];
    substs =
    [Async_kernel__Deferred -> [Async.Deferred; Async_kernel.Deferred];
    Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax; Async_kernel.Let_syntax];
    Async_kernel.Deferred -> [Deferred];
    Async_kernel.Let_syntax -> [Let_syntax; Deferred.Let_syntax.Let_syntax];
    Async_kernel__.Deferred -> [Async.Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax; Async_kernel.Let_syntax];
    Async_kernel.Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax]]
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [Async_kernel__Deferred -> [Async.Deferred; Async_kernel.Deferred];
    Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax; Async_kernel.Let_syntax];
    Async_kernel.Deferred -> [Deferred];
    Async_kernel.Let_syntax -> [Let_syntax; Deferred.Let_syntax.Let_syntax];
    Async_kernel__.Deferred -> [Async.Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax; Async_kernel.Let_syntax];
    Async_kernel.Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax]] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    [Async_kernel__Deferred -> [Async.Deferred; Async_kernel.Deferred];
    Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax; Async_kernel.Let_syntax];
    Async_kernel.Deferred -> [Deferred];
    Async_kernel.Let_syntax -> [Let_syntax; Deferred.Let_syntax.Let_syntax];
    Async_kernel__.Deferred -> [Async.Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax; Async_kernel.Let_syntax];
    Async_kernel.Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax]]
    }
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 7
        },
        "type": "int Deferred.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }
