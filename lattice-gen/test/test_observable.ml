let run () =
  Test_support.with_temp_dir "observable-" (fun dir ->
      let fixtures_src =
        Filename.concat (Filename.concat (Sys.getcwd ()) "test") "observable-fixtures"
      in
      let fixtures_dst = Filename.concat dir "observable-fixtures" in
      Test_support.copy_tree ~src:fixtures_src ~dst:fixtures_dst;
      Observable.process_paths ~mode:Observable.Update ~test:false [ fixtures_dst ];
      let basic = Test_support.read_file (Filename.concat fixtures_dst "basic.html") in
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
      Observable.process_paths ~mode:Observable.Check ~test:false [ fixtures_dst ];
      Observable.process_paths
        ~mode:Observable.Check
        ~test:true
        [ Filename.concat fixtures_dst "basic.html" ];
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
