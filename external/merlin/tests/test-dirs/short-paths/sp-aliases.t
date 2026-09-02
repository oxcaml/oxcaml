  $ cat >test.ml <<'EOF'
  > type t = Foo
  > module X = struct
  >   type prev = t
  >   type t = Bar
  >   let err (_x : prev) = (Bar : t)
  > end
  > EOF

  $ echo "FLG -short-paths -nostdlib" > .merlin

  $ $MERLIN single type-enclosing -position 5:7 \
  > -log-file - -log-section discourse-verbose \
  > -filename test.ml < test.ml 2>&1| sed -E 's/^# [0-9]+.[0-9]+/#/'
  # discourse-verbose - U2
  U2: type t [t[1]] defined in current file
  # discourse-verbose - U1
  U1: path type used in file: t[1] (File "test.ml", line 3, characters 14-15)
  # discourse-verbose - U2
  U2: type prev [prev[3]] defined in current file
  # discourse-verbose - U2
  U2: type t [t[4]] defined in current file
  # discourse-verbose - U1
  U1: path type used in file: prev[3] (File "test.ml", line 5, characters 16-20)
  # discourse-verbose - U1
  U1: path type used in file: t[4] (File "test.ml", line 5, characters 31-32)
  # discourse-verbose - D7
  D7: constructor Bar used, merging its discourse
  # discourse-verbose - U2
  U2: module X [X[2]] defined in current file
  # discourse-verbose - U2
  U2: type X.prev [X[2].prev] defined in current file
  # discourse-verbose - U2
  U2: type X.t [X[2].t] defined in current file
  # discourse-verbose - U2
  U2: value X.err [X[2].err] defined in current file
  # discourse-verbose - D2
  D2: prev in U so in D (kind: type, path: prev[3])
  # discourse-verbose - D6
  D6: merging discourse of type prev
  # discourse-verbose - D2
  D2: t in U so in D (kind: type, path: t[1])
  # discourse-verbose - D6
  D6: merging discourse of type t
  # discourse-verbose - D2
  D2: t in U so in D (kind: type, path: t[4])
  # discourse-verbose - D6
  D6: merging discourse of type t
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 6
        },
        "end": {
          "line": 5,
          "col": 9
        },
        "type": "prev -> t",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 6,
          "col": 3
        },
        "type": "sig type prev = t type t = Bar val err : prev -> t end",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 6,
          "col": 3
        },
        "type": "sig type prev = t type t = Bar val err : prev -> t end",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 5:7 \
  > -log-file - -log-section discourse-recap \
  > -filename test.ml < test.ml 2>&1| sed -E 's/^# [0-9]+.[0-9]+/#/'
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [prev -> [{item = (type, prev[3]); env = with env}];
    t ->
      [{item = (type, t[1]); env = with env};
       {item = (type, t[4]); env = with env}]];
    substs =
    [] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 139;
    paths =
    unit [unit!];
    unativeint_u [unativeint_u!];
    uint8_u [uint8_u!];
    uint64_u [uint64_u!];
    uint32_u [uint32_u!];
    uint16_u [uint16_u!];
    t [t[1]; t[4]];
    string [string!];
    prev [prev[3]];
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
    X [X[2]];
    X.t [X[2].t];
    X.prev [X[2].prev];
    X.err [X[2].err];
    substs =
    []
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    []
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    []
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    []
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    []
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    []
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    []
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    []
    }
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 6
        },
        "end": {
          "line": 5,
          "col": 9
        },
        "type": "prev -> t",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 6,
          "col": 3
        },
        "type": "sig type prev = t type t = Bar val err : prev -> t end",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 6,
          "col": 3
        },
        "type": "sig type prev = t type t = Bar val err : prev -> t end",
        "tail": "no"
      }
    ],
    "notifications": []
  }


  $ $MERLIN single type-enclosing -position 2:0 \
  > -filename test.ml < test.ml | jq '.value[].type'
  "sig type prev = t type t = Bar val err : prev -> t end"
