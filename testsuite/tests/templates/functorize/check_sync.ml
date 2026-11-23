(* TEST
  set input = "${test_source_directory}/${case}/test_byte.ml";
  set case = "";
  output = "${case}_native.ml";
  reference = "${test_source_directory}/${case}/test_native.ml";
  script = "${test_source_directory}/gen-native.sh ${input} ${output}";

  case = "bad_input";
  script;
  check-program-output;

  case = "bad_deps";
  script;
  check-program-output;

  case = "crc_mismatch";
  script;
  check-program-output;

  case = "uses_plain";
  script;
  check-program-output;

  case = "module_alias";
  script;
  check-program-output;

  case = "dunelike";
  script;
  check-program-output;

  case = "partial_deps";
  script;
  check-program-output;

  case = "missing_impl";
  script;
  check-program-output;

  case = "cmifile";
  script;
  check-program-output;

  case = "multi_instances";
  script;
  check-program-output;

  case = "complete_arg";
  script;
  check-program-output;

  case = "nested_static";
  script;
  check-program-output;

  case = "shared_dep";
  script;
  check-program-output;

  case = "instance_arg";
  script;
  check-program-output;

  case = "archive_deps";
  script;
  check-program-output;
*)
