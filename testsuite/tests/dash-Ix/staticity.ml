(* TEST
(* This tests the interaction between the -Ix flag and staticity.

   Module S is declared static via a file-level [@@ static] modality, so its
   cmi records [cmi_staticity = Static].

   -Ix marks a directory as one where cmx files are guaranteed to be available,
   so a module loaded from such a directory keeps the staticity recorded in its
   cmi and can be used at [@ static].  A module loaded from a plain -I directory
   has no such guarantee, so it is forced to [dynamic].

   Each scenario below is compiled twice: via -Ix (a cmx is guaranteed, so the
   static use succeeds) and via plain -I (S is forced dynamic, so the static use
   fails with an error naming S).
*)

subdirectories = "slib";
readonly_files = "use_module.ml use_field.ml use_open.ml";
setup-ocamlc.byte-build-env;

(* Compile the static library module S. *)
flags = "-I slib -nocwd";
module = "slib/s.mli";
ocamlc.byte;
module = "slib/s.ml";
ocamlc.byte;

(* Scenario 1: rebind the whole module S at [@ static]. *)
{
  flags = "-Ix slib -nocwd";
  module = "use_module.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "0";
  ocamlc.byte;
}
{
  flags = "-I slib -nocwd";
  module = "use_module.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference = "${test_source_directory}/use_module.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Scenario 2: use the value [S.x] at [@ static]. *)
{
  flags = "-Ix slib -nocwd";
  module = "use_field.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "0";
  ocamlc.byte;
}
{
  flags = "-I slib -nocwd";
  module = "use_field.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference = "${test_source_directory}/use_field.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Scenario 3: [open S], with the [@ static] use occurring later. *)
{
  flags = "-Ix slib -nocwd";
  module = "use_open.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "0";
  ocamlc.byte;
}
{
  flags = "-I slib -nocwd";
  module = "use_open.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference = "${test_source_directory}/use_open.ocamlc.reference";
  check-ocamlc.byte-output;
}

*)

(* intentionally empty *)
