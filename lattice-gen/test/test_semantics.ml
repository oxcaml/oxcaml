let () =
  Test_negative_cases.run ();
  Test_model_structure.run ();
  Test_mode_compatibility.run ();
  Test_observable.run ();
  Test_curated_fixtures.run ();
  Test_property_cases.run ()
