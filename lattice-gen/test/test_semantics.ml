let () =
  Test_negative_cases.run ();
  Test_bitwise.run ();
  Test_codegen_shapes.run ();
  Test_model_structure.run ();
  Test_observable.run ();
  Test_curated_fixtures.run ();
  Test_compiler_fixture.run ()
