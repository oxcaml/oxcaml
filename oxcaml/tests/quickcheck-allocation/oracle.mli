module Verdict : sig
  type t =
    | Accept (* the check succeeded (exit 0) *)
    | Reject of
        string (* the check failed as expected; payload = captured stderr *)
    | Gen_error of string (* compile failed for an unrelated reason *)

  val cause : t -> string
end

module Quadrant : sig
  type t =
    | Agree_noalloc (* FE accept, BE pass *)
    | Agree_alloc (* FE reject, BE reject *)
    | Soundness_suspect (* FE accept, BE reject -- ambiguous, triage *)
    | Precision_gap (* FE reject, BE pass -- trustworthy frontend gap *)
end

(* [compiler] must be an absolute path to the built [ocamlopt.opt]; it is
   invoked via the shell, so a bare name would be resolved by the shell's
   PATH. *)
val run_frontend : compiler:string -> file:string -> Verdict.t

val run_backend : compiler:string -> file:string -> Verdict.t

val classify :
  frontend:Verdict.t -> backend:Verdict.t -> (Quadrant.t, string) result
