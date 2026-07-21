(* The fuzz loop: generate programs, run the oracle pipeline, and aggregate the
   results. *)

module Suspect : sig
  (* A soundness suspect (FE-accept & BE-reject): ambiguous, needs manual
     triage. *)
  type t =
    { seed : int;
      sample : Gen.Sample.t;
      backend_error : string
    }

  val to_string : t -> string
end

module Gap : sig
  (* A frontend rejection: a completeness (precision-gap) candidate, bucketed by
     cause. The backend cannot confirm it, since the rejected program does not
     compile. *)
  type t =
    { seed : int;
      cause : string; (* canonical frontend rejection cause, for bucketing *)
      sample : Gen.Sample.t
    }
end

module Stats : sig
  type t

  val agree_noalloc : t -> int

  val gen_errors : t -> int

  (* Both lists are in generation order. *)
  val suspects : t -> Suspect.t list

  val gaps : t -> Gap.t list
end

(* Run [count] rounds starting from PRNG seed [seed0] (round [k] uses seed
   [seed0 + k]), all in generation mode [mode]. [compiler] is the path to the
   built [ocamlopt.opt]. *)
val run :
  compiler:string -> count:int -> seed0:int -> mode:Gen.Mode.t -> Stats.t

val report : Stats.t -> unit
