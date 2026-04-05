let fixtures =
  [ "example.lattice";
    "compiler_structure.lattice";
    "chains.lattice";
    "diamond.lattice";
    "sampled_product.lattice"
  ]

let run () =
  List.iter
    (fun fixture ->
      let source = Test_support.load_fixture fixture in
      Test_support.expect_generated_excludes
        ~name:fixture
        ~source
        [ "Solver_runtime";
          "Lattices_univ";
          "Solver_support";
          "type 'd t constraint 'd = 'l * 'r";
          "include-solver"
        ];
      if fixture = "compiler_structure.lattice"
      then (
        Test_support.expect_generated_ml_contains
          ~name:fixture
          ~source
          [ "module Value = struct";
            "module Alloc = struct";
            "module Kind_axes = struct";
            "alloc_as_value x =";
            "value_to_alloc_r2l x =";
            "let rec"
          ];
        Test_support.expect_generated_mli_contains
          ~name:fixture
          ~source
          [ "module Value : sig";
            "module Alloc : sig";
            "module Kind_axes : sig";
            "module Axis : sig";
            "val alloc_as_value : Alloc.t -> Value.t";
            "val value_to_alloc_r2g : Value.t -> Alloc.t"
          ]))
    fixtures;
  Test_support.compile_generated_case
    ~name:"compiler-compile"
    ~source:(Test_support.load_fixture "compiler_structure.lattice")
    ~main:
      {|
let () =
  let _ = Generated.Locality.global in
  let _ = Generated.Value.bottom in
  let _ = Generated.Kind_axes.bottom in
  let _ = Generated.alloc_as_value Generated.Alloc.bottom in
  let _ = Generated.value_to_alloc_r2l Generated.Value.bottom in
  ()
|}
