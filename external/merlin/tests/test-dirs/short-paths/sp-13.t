This mocks the Async --include--> Async_kernel --exports--> Deferred

  $ mkdir async_kernel
  $ cd async_kernel


  $ cat >async_kernel__.ml <<'EOF'
  > module Deferred = Async_kernel__Deferred
  > module Deferred0 = Async_kernel__Deferred0
  > EOF

  $ $OCAMLC -c async_kernel__.ml -no-alias-deps 2>/dev/null
  $ $MERLIN_TEST_OCAML_PATH/bin/ocamlobjinfo -quiet -discourse async_kernel__.cmi
  Discourse:
  Deferred: alias: Async_kernel__Deferred [Async_kernel__Deferred!]
    Async_kernel__Deferred!
  
  Deferred0: alias: Async_kernel__Deferred0 [Async_kernel__Deferred0!]
    Async_kernel__Deferred0!
  
  $ cat >deferred0.ml <<'EOF'
  > type +'a t = 'a
  > let create : 'a -> 'a t = fun x -> x
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
  >   module Let_syntax = struct let return x = Deferred0.create x end
  > end
  > EOF

  $ $OCAMLC -c deferred.ml -open Async_kernel__ -o Async_kernel__Deferred


  $ cat >async_kernel.ml <<'EOF'
  > module Deferred = Deferred
  > include Deferred.Let_syntax
  > EOF

  $ $OCAMLC -c async_kernel.ml -open Async_kernel__


  $ cd ..
  $ mkdir async
  $ cd async

  $ cat >async.ml <<'EOF'
  > include Async_kernel
  > EOF

  $ $OCAMLC -c async.ml -I ../async_kernel
  $ $MERLIN_TEST_OCAML_PATH/bin/ocamlobjinfo -quiet -discourse async.cmi
  Discourse:
  Deferred: alias: Deferred [Async_kernel__!.Deferred] Async_kernel__!.Deferred
  
  Let_syntax:
    alias: Deferred.Let_syntax.Let_syntax [Deferred/334[1].Let_syntax.Let_syntax]
    
  
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

  $ $MERLIN single type-enclosing -position 3:5 \
  > -log-file - -log-section discourse-recap \
  > -nostdlib \
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
    Async_kernel__.Deferred -> [Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax -> [Let_syntax]] }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Deferred -> [{item = (module, Deferred[1]); env = with env}];
    Let_syntax ->
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
    size = 145;
    paths =
    unit [unit!];
    unativeint_u [unativeint_u!];
    uint8_u [uint8_u!];
    uint64_u [uint64_u!];
    uint32_u [uint32_u!];
    uint16_u [uint16_u!];
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
    Async [Async!];
    Async.Let_syntax [Async!.Let_syntax];
    Async.Deferred [Async!.Deferred];
    substs =
    [Async_kernel__Deferred -> [Async.Deferred];
    Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax; Deferred.Let_syntax.Let_syntax];
    Async_kernel__.Deferred -> [Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax]]
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [Async_kernel__Deferred -> [Async.Deferred];
    Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax; Deferred.Let_syntax.Let_syntax];
    Async_kernel__.Deferred -> [Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax]] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    [Async_kernel__Deferred -> [Async.Deferred];
    Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax; Deferred.Let_syntax.Let_syntax];
    Async_kernel__.Deferred -> [Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax ->
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
        "type": "int Async_kernel__Deferred0.t",
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
    Async_kernel__.Deferred -> [Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax -> [Let_syntax]] }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Deferred -> [{item = (module, Deferred[1]); env = with env}];
    Let_syntax ->
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
    size = 145;
    paths =
    unit [unit!];
    unativeint_u [unativeint_u!];
    uint8_u [uint8_u!];
    uint64_u [uint64_u!];
    uint32_u [uint32_u!];
    uint16_u [uint16_u!];
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
    Async [Async!];
    Async.Let_syntax [Async!.Let_syntax];
    Async.Deferred [Async!.Deferred];
    substs =
    [Async_kernel__Deferred -> [Async.Deferred];
    Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax; Deferred.Let_syntax.Let_syntax];
    Async_kernel__.Deferred -> [Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax]]
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [Async_kernel__Deferred -> [Async.Deferred];
    Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax; Deferred.Let_syntax.Let_syntax];
    Async_kernel__.Deferred -> [Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax ->
      [Let_syntax; Async.Let_syntax]] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    [Async_kernel__Deferred -> [Async.Deferred];
    Async.Deferred -> [Deferred];
    Async.Let_syntax -> [Let_syntax; Deferred.Let_syntax.Let_syntax];
    Async_kernel__.Deferred -> [Deferred];
    Async_kernel__Deferred.Let_syntax.Let_syntax ->
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
        "type": "int Async_kernel__Deferred0.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }
