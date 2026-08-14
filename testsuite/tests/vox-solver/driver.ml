(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

(* Driver policy: [none] is not a backend, and an unusable configuration
   fails once, at selection, not once per obligation. *)

open Vox_logic
open Vox_backend

let obligation =
  { Obligation.signature =
      { Signature.empty with variables = ["n", Sort.Int] }
  ; hypotheses =
      [ { id = 0
        ; term = App (Ge, [Var "n"; Const (Int "0")])
        ; origin = { label = "n is a length"; location = Location.none }
        }
      ]
  ; goal = App (Ge, [App (Add, [Var "n"; Const (Int "1")]); Const (Int "1")])
  ; location = Location.none
  }

let describe = function
  | Ok (Proved { unused_hypotheses = _ }) -> "proved"
  | Ok (Refuted _) -> "refuted"
  | Ok (Unknown Timeout) -> "unknown (timeout)"
  | Ok (Unknown (Incomplete reason)) -> "unknown (" ^ reason ^ ")"
  | Result.Error (Unavailable message) -> "unavailable: " ^ message
  | Result.Error (Error { cause; raw = _ }) -> "error: " ^ cause

let run backend_name config =
  match plan ~backend_name ~config with
  | Result.Error message -> Format.printf "selection failed: %s@." message
  | Ok No_discharge -> Format.printf "not discharged@."
  | Ok (Discharge (module Backend)) ->
    Format.printf "discharging with %s:@." Backend.name;
    Format.printf "%s@." (describe (Backend.discharge ~config obligation))

[%%expect{|
val obligation : Vox_logic.Obligation.t =
  {Obligation.signature =
    {Signature.sorts = []; datatypes = []; variables = [("n", Sort.Int)];
     functions = []};
   hypotheses =
    [{Obligation.id = 0;
      term = Term.App (Op.Ge, [Term.Var "n"; Term.Const (Literal.Int "0")]);
      origin =
       {Origin.label = "n is a length";
        location =
         {Location.loc_start =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true}}}];
   goal =
    Term.App (Op.Ge,
     [Term.App (Op.Add, [Term.Var "n"; Term.Const (Literal.Int "1")]);
      Term.Const (Literal.Int "1")]);
   location =
    {Location.loc_start =
      {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1};
     loc_end =
      {Lexing.pos_fname = "_none_"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1};
     loc_ghost = true}}
val describe : (Vox_backend.verdict, Vox_backend.failure) Result.t -> string =
  <fun>
val run : string -> Vox_backend.Config.t -> unit = <fun>
|}]

(* [none] short-circuits before any backend is consulted: obligations are
   reported as not discharged even with no solver on the machine. *)

let () = run "none" Config.default

[%%expect{|
not discharged
|}]

(* The printing backend emits the prove query and discharges nothing. *)

let () = run "printing" { Config.default with timeout_seconds = None }

[%%expect{|
discharging with printing:
(set-option :produce-unsat-cores true)
(declare-const n Int)
(assert (! (>= n 0) :named h0))
(assert (not (>= (+ n 1) 1)))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
unknown (printing backend discharges nothing)
|}]

(* z3 without a configured command fails at selection, with one message
   that names the way out. *)

let () = run "z3" Config.default

[%%expect{|
selection failed: z3 backend: no solver configured: give the z3 backend a command, or pass -vox-backend none to typecheck only
|}]

(* An unknown name fails with the valid names, derived from the backend
   list. *)

let () = run "lean" Config.default

[%%expect{|
selection failed: unknown vox backend lean (valid backends: printing, z3; or none to typecheck only)
|}]
