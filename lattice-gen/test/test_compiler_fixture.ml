let run () =
  let source = Test_support.load_fixture "compiler_structure_html.lattice" in
  Observable.test_lattice_source
    ~root_module:"Compiler"
    ~input_name:"compiler_structure_html.lattice"
    source
