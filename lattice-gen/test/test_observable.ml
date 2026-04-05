let run () =
  Test_support.with_temp_dir "observable-" (fun dir ->
      let fixtures_dir = Filename.concat (Filename.concat (Sys.getcwd ()) "test") "observable-fixtures" in
      let basic_src = Filename.concat fixtures_dir "basic.html" in
      let basic_dst = Filename.concat dir "basic.html" in
      Test_support.copy_tree ~src:basic_src ~dst:basic_dst;
      Observable.process_paths ~mode:Observable.Update ~test:false [ basic_dst ];
      let basic = Test_support.read_file basic_dst in
      Test_support.ensure
        (Test_support.contains basic "lattice-gen-output:start")
        "basic observable page missing generated marker";
      Test_support.ensure
        (Test_support.contains basic "lattice-gen-scaffold:start")
        "basic observable page missing generated scaffold";
      Test_support.ensure
        (Test_support.contains basic "lattice-generated-tab")
        "basic observable page missing tab controls";
      Test_support.ensure
        (Test_support.contains basic "data-language=\"ocaml\"")
        "basic observable page missing ocaml language markers";
      Test_support.ensure
        (Test_support.contains basic "tok-keyword")
        "basic observable page missing syntax highlighting classes";
      Test_support.ensure
        (Test_support.contains basic "module Locality = struct")
        "basic observable page missing generated module";
      Observable.process_paths ~mode:Observable.Check ~test:false [ basic_dst ];
      Observable.process_paths ~mode:Observable.Check ~test:true [ basic_dst ];
      let bad_input =
        Filename.concat (Filename.concat (Sys.getcwd ()) "test") "observable_input.html"
      in
      match Observable.process_paths ~mode:Observable.Update ~test:true [ bad_input ] with
      | () -> Test_support.failf "expected observable rejection for <ocaml> block"
      | exception Observable.Error message ->
        Test_support.ensure
          (Test_support.contains message "<ocaml> blocks are not supported")
          "unexpected observable error: %s"
          message)
