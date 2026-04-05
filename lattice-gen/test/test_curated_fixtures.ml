let fixtures =
  [ "example.lattice";
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
      ())
    fixtures;
