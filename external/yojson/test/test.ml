let () =
  Alcotest.run "Yojson"
    [
      ("equality", Test_monomorphic.equality);
      ("read", Test_read.single_json);
      ("write", Test_write.single_json);
      ("util", Test_util.tests);
    ]
