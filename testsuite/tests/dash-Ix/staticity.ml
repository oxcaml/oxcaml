(* TEST
(* This tests the interaction between the -Ix flag and staticity.

   Module S is declared static via a file-level [@@ static] modality, so its
   cmi records [cmi_staticity = Static].

   -Ix marks a directory as one where cmx files are guaranteed to be available,
   so a module loaded from such a directory keeps the staticity recorded in its
   cmi and can be used at [@ static].  A module loaded from a plain -I directory
   has no such guarantee, so it is forced to [dynamic].

   Below we rebind the static module S at [@ static]: this succeeds when S is
   loaded via -Ix, but fails when loaded via plain -I.
*)

subdirectories = "slib";
setup-ocamlc.byte-build-env;

(* Compile the static library module S. *)
flags = "-I slib -nocwd";
module = "slib/s.mli";
ocamlc.byte;
module = "slib/s.ml";
ocamlc.byte;

{
  (* -Ix guarantees a cmx, so S keeps its [Static] staticity and can be rebound
     at [@ static]: compilation succeeds. *)
  flags = "-Ix slib -nocwd";
  module = "staticity.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "0";
  ocamlc.byte;
}
{
  (* Plain -I does not guarantee a cmx, so S is forced to [dynamic] and the
     rebind at [@ static] fails. *)
  flags = "-I slib -nocwd";
  module = "staticity.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference = "${test_source_directory}/staticity.ocamlc.reference";
  check-ocamlc.byte-output;
}

*)
module (M @ static) = S
