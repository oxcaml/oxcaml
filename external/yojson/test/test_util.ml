let path_empty () =
  Alcotest.(check (option Testable.yojson))
    __LOC__ (Some Fixtures.json_value)
    (Yojson.Safe.Util.path [] Fixtures.json_value)

let path_missing () =
  Alcotest.(check (option Testable.yojson))
    __LOC__ None
    (Yojson.Safe.Util.path [ "does not exist" ] Fixtures.json_value)

let path_traverse () =
  Alcotest.(check (option Testable.yojson))
    __LOC__
    (Some (`Int 42))
    (Yojson.Safe.Util.path [ "assoc"; "value" ] Fixtures.json_value)

let tests =
  [
    ("empty path", `Quick, path_empty);
    ("non-existing path", `Quick, path_missing);
    ("traversal", `Quick, path_traverse);
  ]
