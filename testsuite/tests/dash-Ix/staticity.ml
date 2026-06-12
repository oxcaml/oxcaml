(* TEST
(* This tests the interaction between the -Ix flag and staticity.

   Module S is declared static via a file-level [@@ static] modality, so its
   cmi records [cmi_staticity = Static].

   -Ix marks a directory as one where cmx files are guaranteed to be available,
   so a module loaded from such a directory keeps the staticity recorded in its
   cmi and can be used at [@ static].  A module loaded from a plain -I directory
   has no such guarantee, so it is forced to [dynamic].

   We test that the same static module S can be rebound at [@ static] when
   loaded via -Ix, but not when loaded via plain -I.
*)

subdirectories = "slib";
readonly_files = "use_static.ml";
setup-ocamlc.byte-build-env;

(* Compile the static library module S. *)
flags = "-I slib -nocwd";
module = "slib/s.mli";
ocamlc.byte;
module = "slib/s.ml";
ocamlc.byte;

{
  (* -Ix guarantees a cmx, so S keeps its [Static] staticity and can be rebound
     at [@ static]. *)
  flags = "-Ix slib -nocwd";
  module = "use_static.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  (* Plain -I does not guarantee a cmx, so S is forced to [dynamic] and the
     rebind at [@ static] fails. *)
  flags = "-I slib -nocwd";
  module = "use_static.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference = "${test_source_directory}/use_static.ocamlc.reference";
  check-ocamlc.byte-output;
}

*)
