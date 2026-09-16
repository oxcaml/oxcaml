(* TEST
  setup-ocamlc.byte-build-env;

  (* Test that we've got [test_byte.ml] and [test_native.ml] in sync. If this fails, run
     [gen-native.sh] in this directory. *)

  script = "sh -c 'cd ${test_source_directory} && ./gen-native.sh --check'";
  script;
*)
