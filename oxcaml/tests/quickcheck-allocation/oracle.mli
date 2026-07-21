(* Oracle: run the frontend Allocation-mode check and the backend zero_alloc check on a
   single doubly-annotated program, and classify the outcome. The same file is used for
   both checks so that both analyses see the same typed program (the [@ noalloc_strict]
   mode annotation changes inference by forcing the function's allocations local, so a
   source without it would be a different program). *)

module Verdict : sig
  type t =
    | Accept (* the check succeeded (exit 0) *)
    | Reject of
        string (* the check failed as expected; payload = captured stderr *)
    | Gen_error of string (* compile failed for an unrelated reason *)

  val cause : t -> string
end

module Outcome : sig
  (* Because the program carries both annotations, a frontend rejection is a type
     error: the program does not compile, so the backend check cannot run on it. There
     is no FE-reject & BE-pass quadrant anymore; frontend rejections are completeness
     candidates, bucketed by cause. *)
  type t =
    | Agree_noalloc (* FE accepts, BE passes -- agree *)
    | Soundness_suspect of { backend_error : string }
        (* FE accepts, BE rejects -- ambiguous (backend may be imprecise), triage *)
    | Fe_reject of { cause : string }
        (* FE rejects -- possible precision gap; not confirmable by the backend *)
end

(* [compiler] must be an absolute path to the built [ocamlopt.opt]; it is
   invoked via the shell, so a bare name would be resolved by the shell's
   PATH. *)
val run_frontend : compiler:string -> file:string -> Verdict.t

val run_backend : compiler:string -> file:string -> Verdict.t

(* The full oracle pipeline on one source file: the frontend typing check first, then
   the backend zero_alloc check only if the frontend accepts. [Error] means a check
   failed for a reason unrelated to the property under test, i.e. a generator bug. *)
val check : compiler:string -> file:string -> (Outcome.t, string) result
