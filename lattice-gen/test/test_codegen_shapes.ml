let run () =
  let source = Test_support.load_fixture "compiler_structure_html.lattice" in
  Test_support.expect_generated_ml_contains
    ~name:"compiler_structure_html.lattice"
    ~source
    [ "let[@inline] imply x y = y lor (x lxor 3)";
      "staticity = t lsr 5;";
      "statefulness = t lsr 5;";
      "with_staticity x t = (t land 31) lor (x lsl 5)";
      "let[@inline] make\n      ~uniqueness\n      ~contention\n      ~visibility\n      ~staticity\n    =\n    uniqueness lor (contention lsl 1) lor (visibility lsl 3) lor (staticity lsl 5)";
      "let[@inline] join x y = x lor y";
      "let[@inline] meet x y = x land y";
      "let[@inline] join x y =\n    (x land 511)";
      "lor ((x land 32256) land (y land 32256))"
    ];
  Test_support.expect_generated_excludes
    ~name:"compiler_structure_html.lattice"
    ~source
    [ "lsl 0";
      "lsr 0";
      "let o =";
      "let a =";
      "let tmp1 =";
      "let uniqueness = uniqueness in";
      "land 1) in";
      "land 3) in"
    ]
