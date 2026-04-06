let run () =
  let source =
    Test_support.load_fixture "compiler_structure_html.lattice"
    ^ {|

A = [ Lo < Hi ]

Pair = {
  left : A;
  right : A;
}

Single = {
  value : A;
}

join_pair : Pair -> Single = {
  value = join(left, right);
}

meet_pair : Pair -> Single = {
  value = meet(left, right);
}
|}
  in
  Test_support.expect_generated_ml_contains
    ~name:"compiler_structure_html.lattice"
    ~source
    [ "let[@inline] imply x y = (x lxor 0b11) lor y";
      "let[@inline] subtract x y = x land lnot y";
      "let[@inline] imply x y = y land lnot x";
      "staticity = t lsr 5;";
      "statefulness = t lsr 5;";
      "with_staticity x t = (x lsl 5) lor (t land 0b11111)";
      "let[@inline] make\n      ~uniqueness\n      ~contention\n      ~visibility\n      ~staticity\n    =\n    uniqueness lor (contention lsl 1) lor (visibility lsl 3) lor (staticity lsl 5)";
      "let[@inline] join x y = x lor y";
      "let[@inline] meet x y = x land y";
      "let[@inline] join x y =\n    ((x land y) land 0b111111000000000) lor ((x lor y) land 0b111111111)";
      "A.join (Pair.proj_left x) (Pair.proj_right x)";
      "A.meet (Pair.proj_left x) (Pair.proj_right x)"
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
    ];
  Test_support.expect_generated_ml_contains
    ~name:"unique-implies-uncontended.lattice"
    ~source:
      {|
U2 = [ Aliased > Unique ]

C2 = [ Contended > Shared > Uncontended ]

M2 = {
  uniqueness : U2;
  contention : C2;
}

K2 = {
  monadic : M2^op;
}

unique_cap2 : U2^op -> C2^op = [
  Aliased -> Contended;
  Unique -> Uncontended;
]

unique_implies_uncontended2 : M2^op -> M2^op = {
  uniqueness = uniqueness;
  contention = join(contention, unique_cap2(uniqueness));
}

kinds_unique_implies_uncontended2 : K2 -> K2 = {
  monadic = unique_implies_uncontended2(monadic);
}
|}
    [ "let[@inline] unique_cap2 x = x lor (x lsl 1)";
      "let unique_implies_uncontended2 x =";
      "~contention:(C2_op.join (M2_op.proj_contention x) (unique_cap2 (M2_op.proj_uniqueness x)))";
      "let kinds_unique_implies_uncontended2 x =";
      "~monadic:(unique_implies_uncontended2 (K2.proj_monadic x))"
    ]
