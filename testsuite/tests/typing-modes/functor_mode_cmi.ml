(* TEST
 readonly_files = "functor_mode_cmi_lib.ml";
 setup-ocamlc.byte-build-env;
 module = "functor_mode_cmi_lib.ml";
 ocamlc.byte;
 module = "functor_mode_cmi.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

module G_impl (X : sig type t end) : sig type t = X.t list end @ local = struct
  type t = X.t list
end

module G : module type of Functor_mode_cmi_lib.F = G_impl

module N = Functor_mode_cmi_lib.F (struct type t = int end)
