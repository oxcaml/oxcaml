let run () =
  let source =
    Test_support.load_fixture "compiler_structure_kind_subbed.lattice"
  in
  let compatibility_source =
    Test_support.load_fixture "mode_compatibility_source.ml"
  in
  Test_support.compile_generated_case_with_module
    ~name:"mode-compatibility"
    ~source
    ~module_name:"compatibility"
    ~module_source:compatibility_source
