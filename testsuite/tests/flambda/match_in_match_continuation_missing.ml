(* TEST
 flambda;
 ocamlopt_flags = "-w -20 -O3 -flambda2-match-in-match -flambda2-expert-cont-specialization-threshold 200";
 native;
*)

(* This test exercises a situation where, due to over-application (and other things),
   we get into a situation where we do continuation specialization on a handler which
   contains at least:
   - one wrapper continuation (over-application wrappers)
   - one non-wrapper continuation, which happens to reference the wrapper continuation

   In such a situation, because we currently cannot lift the wrapper continuation
   (because of potential symbols within them, but also because it's unclear how correct the
   replay history would be for such continuations), we also cannot lift the non-wrapper
   continuation, because it refers to a non-lifted continuation. *)

external __dummy2__ : unit -> 'a = "%opaque"

let errorf ?(sub = 0) = __dummy2__ ()

let[@local never][@inline] id x = x

(* This is to prevent lambda_to_flambda from identifying the over-application. *)
let errorf = id errorf

let f s x =
  match s with
  | "ocaml.error" | "error" -> errorf x
  | _ -> ()

