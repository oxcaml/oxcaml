Reproduce a short-paths bug where a wrapped library's mangled name leaks
into the printed type. With dune-style wrapping, querying the type of
`Repro2_main.Topic_name.of_topic` in `usage/src/usage.ml` should print
`Repro2_main.Topic.t -> Repro2_main.Topic_name.t option`, but instead
prints `Repro2_types__.Topic.t -> Repro2_main.Topic_name.t option`.

Build the priv library (no dependencies).
  $ cat > priv/src/repro2_priv__.ml-gen << 'EOF'
  > module No_direct_access_to_repro2_priv = struct
  >   module Repro2_priv = No_such_module
  >   module Repro2_priv__Topic = No_such_module
  >   module Repro2_priv__Topic_name = No_such_module
  > end
  > 
  > (** @canonical Repro2_priv.Repro2_priv *)
  > module Repro2_priv = Repro2_priv
  > 
  > (** @canonical Repro2_priv.Topic *)
  > module Topic = Repro2_priv__Topic
  > 
  > (** @canonical Repro2_priv.Topic_name *)
  > module Topic_name = Repro2_priv__Topic_name
  > EOF

  $ $OCAMLC -w -49 -no-alias-deps -o priv/src/repro2_priv__.cmo \
  >   -c -impl priv/src/repro2_priv__.ml-gen
  $ $OCAMLC -I priv/src -open Repro2_priv__ \
  >   -o priv/src/repro2_priv__Topic.cmi -c -intf priv/src/topic.mli
  $ $OCAMLC -I priv/src -open Repro2_priv__ \
  >   -o priv/src/repro2_priv__Topic.cmo -c -impl priv/src/topic.ml
  $ $OCAMLC -I priv/src -open Repro2_priv__ \
  >   -o priv/src/repro2_priv__Topic_name.cmi -c -intf priv/src/topic_name.mli
  $ $OCAMLC -I priv/src -open Repro2_priv__ \
  >   -o priv/src/repro2_priv__Topic_name.cmo -c -impl priv/src/topic_name.ml
  $ $OCAMLC -I priv/src -open Repro2_priv__ \
  >   -o priv/src/repro2_priv.cmo -c -impl priv/src/repro2_priv.ml

Build the types library (depends on priv).
  $ cat > types/src/repro2_types__.ml-gen << 'EOF'
  > module No_direct_access_to_repro2_types = struct
  >   module Repro2_types = No_such_module
  >   module Repro2_types__Topic = No_such_module
  >   module Repro2_types__Topic_name = No_such_module
  > end
  > 
  > (** @canonical Repro2_types.Repro2_types *)
  > module Repro2_types = Repro2_types
  > 
  > (** @canonical Repro2_types.Topic *)
  > module Topic = Repro2_types__Topic
  > 
  > (** @canonical Repro2_types.Topic_name *)
  > module Topic_name = Repro2_types__Topic_name
  > EOF

  $ $OCAMLC -w -49 -no-alias-deps -o types/src/repro2_types__.cmo \
  >   -c -impl types/src/repro2_types__.ml-gen
  $ $OCAMLC -I types/src -I priv/src -open Repro2_types__ \
  >   -o types/src/repro2_types__Topic.cmi -c -intf types/src/topic.mli
  $ $OCAMLC -I types/src -I priv/src -open Repro2_types__ \
  >   -o types/src/repro2_types__Topic.cmo -c -impl types/src/topic.ml
  $ $OCAMLC -I types/src -I priv/src -open Repro2_types__ \
  >   -o types/src/repro2_types__Topic_name.cmi -c -intf types/src/topic_name.mli
  $ $OCAMLC -I types/src -I priv/src -open Repro2_types__ \
  >   -o types/src/repro2_types__Topic_name.cmo -c -impl types/src/topic_name.ml
  $ $OCAMLC -I types/src -I priv/src -open Repro2_types__ \
  >   -o types/src/repro2_types.cmo -c -impl types/src/repro2_types.ml

Build the main library (depends on types).
  $ cat > main/src/repro2_main__.ml-gen << 'EOF'
  > module No_direct_access_to_repro2_main = struct
  >   module Repro2_main = No_such_module
  > end
  > 
  > (** @canonical Repro2_main.Repro2_main *)
  > module Repro2_main = Repro2_main
  > EOF

  $ $OCAMLC -w -49 -no-alias-deps -o main/src/repro2_main__.cmo \
  >   -c -impl main/src/repro2_main__.ml-gen
  $ $OCAMLC -I main/src -I types/src -I priv/src -open Repro2_main__ \
  >   -o main/src/repro2_main.cmo -c -impl main/src/repro2_main.ml

Create a .merlin in usage/src mirroring what dune would generate.
  $ cat > usage/src/.merlin << 'EOF'
  > FLG -short-paths
  > FLG -log-file log -log-section discourse-recap -nostdlib
  > FLG -open Repro2_standalone__
  > S .
  > B .
  > SH ../../priv/src
  > BH ../../priv/src
  > S ../../types/src
  > B ../../types/src
  > S ../../main/src
  > B ../../main/src
  > EOF

TODO suspicions duplications in u_paths ?

Expected `Repro2_main.Topic.t -> Repro2_main.Topic_name.t option`.
  $ $MERLIN single type-enclosing -position 1:40 -index 0 \
  > -filename usage/src/usage.ml < usage/src/usage.ml \
  > | jq '.value[0].type'
  "Repro2_main.Topic.t -> Repro2_main.Topic_name.t option"


  $ cat log | sed -E 's/^# [0-9]+.[0-9]+/#/'
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [Repro2_main -> [{item = (module, Repro2_main!); env = with env}];
    Repro2_main.Topic_name ->
      [{item = (module, Repro2_main!.Topic_name); env = with env}];
    Repro2_main.Topic_name.of_topic ->
      [{item = (value, Repro2_main!.Topic_name.of_topic); env = with env}]];
    substs =
    [] }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Topic_name ->
       [{item = (module, Repro2_types!.Topic_name); env = with env}];
    Repro2_main.Topic_name ->
      [{item = (module, Repro2_types__Topic_name!); env = with env}]];
    substs =
    [] }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Topic_name ->
       [{item = (module, Repro2_types__Topic_name!); env = with env};
        {item = (module, Repro2_types__!.Topic_name); env = with env}]];
    substs =
    [] }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Repro2_types__Topic_name ->
       [{item = (module, Repro2_types__Topic_name!); env = with env}];
    Topic_name ->
      [{item = (module, Repro2_types__Topic_name!); env = with env}]];
    substs =
    [] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 163;
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
    Topic_name
      [Repro2_types__Topic_name!; Repro2_types!.Topic_name;
       Repro2_types__!.Topic_name];
    Topic_name.t [Repro2_types__Topic_name!.t];
    Topic_name.of_topic [Repro2_types__Topic_name!.of_topic];
    Repro2_types__Topic_name [Repro2_types__Topic_name!];
    Repro2_types__Topic_name.t [Repro2_types__Topic_name!.t];
    Repro2_types__Topic_name.of_topic [Repro2_types__Topic_name!.of_topic];
    Repro2_types__.Topic_name [Repro2_types__!.Topic_name];
    Repro2_types__.Topic.t [Repro2_types__!.Topic.t];
    Repro2_types.Topic_name [Repro2_types!.Topic_name];
    Repro2_main [Repro2_main!];
    Repro2_main.Topic_name [Repro2_types__Topic_name!; Repro2_main!.Topic_name];
    Repro2_main.Topic_name.t [Repro2_types__Topic_name!.t];
    Repro2_main.Topic_name.of_topic
      [Repro2_types__Topic_name!.of_topic; Repro2_main!.Topic_name.of_topic];
    Repro2_main.Topic [Repro2_main!.Topic];
    substs =
    [Repro2_types__Topic -> [Repro2_main.Topic];
    Repro2_types__Topic_name -> [Topic_name; Repro2_main.Topic_name];
    Repro2_main.Topic_name -> [Topic_name];
    Repro2_types.Topic_name -> [Topic_name];
    Repro2_types__.Topic -> [Repro2_main.Topic];
    Repro2_types__.Topic_name ->
      [Repro2_types__Topic_name; Repro2_main.Topic_name]]
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [Repro2_types__Topic -> [Repro2_main.Topic];
    Repro2_types__Topic_name -> [Topic_name; Repro2_main.Topic_name];
    Repro2_main.Topic_name -> [Topic_name];
    Repro2_types.Topic_name -> [Topic_name];
    Repro2_types__.Topic -> [Repro2_main.Topic];
    Repro2_types__.Topic_name ->
      [Repro2_types__Topic_name; Repro2_main.Topic_name]] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    [Repro2_types__Topic -> [Repro2_main.Topic];
    Repro2_types__Topic_name -> [Topic_name; Repro2_main.Topic_name];
    Repro2_main.Topic_name -> [Topic_name];
    Repro2_types.Topic_name -> [Topic_name];
    Repro2_types__.Topic -> [Repro2_main.Topic];
    Repro2_types__.Topic_name ->
      [Repro2_types__Topic_name; Repro2_main.Topic_name]]
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    [Repro2_types__Topic -> [Repro2_main.Topic];
    Repro2_types__Topic_name -> [Topic_name; Repro2_main.Topic_name];
    Repro2_main.Topic_name -> [Topic_name];
    Repro2_types.Topic_name -> [Topic_name];
    Repro2_types__.Topic -> [Repro2_main.Topic];
    Repro2_types__.Topic_name ->
      [Repro2_types__Topic_name; Repro2_main.Topic_name]] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    [Repro2_types__Topic -> [Repro2_main.Topic];
    Repro2_types__Topic_name -> [Topic_name; Repro2_main.Topic_name];
    Repro2_main.Topic_name -> [Topic_name];
    Repro2_types.Topic_name -> [Topic_name];
    Repro2_types__.Topic -> [Repro2_main.Topic];
    Repro2_types__.Topic_name ->
      [Repro2_types__Topic_name; Repro2_main.Topic_name]]
    }
