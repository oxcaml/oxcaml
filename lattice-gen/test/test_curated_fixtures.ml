let fixtures =
  [ "example.lattice";
    "compiler_structure.lattice";
    "compiler_structure_kind_subbed.lattice";
    "chains.lattice";
    "diamond.lattice";
    "sampled_product.lattice"
  ]

let run () =
  List.iter
    (fun fixture ->
      let source = Test_support.load_fixture fixture in
      if fixture = "compiler_structure.lattice"
         || fixture = "compiler_structure_kind_subbed.lattice"
      then
        (Test_support.expect_generated_ml_excludes
           ~name:fixture
           ~source
           [ "ignore (";
             "Repr.of_int_exn";
             "lsr 0";
             "let close_repr x = x";
             "let close_down_nat x = x";
             "let close_up_nat x = x";
             "let close_down_dual x = x";
             "let close_up_dual x = x";
             "Format.pp_print_string ppf (show t)"
           ];
         Test_support.compile_generated_case
           ~name:fixture
           ~source)
      else
        Test_support.run_generated_case
          ~name:fixture
          ~source)
    fixtures
