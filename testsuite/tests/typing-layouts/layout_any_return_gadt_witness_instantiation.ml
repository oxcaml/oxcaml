(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 {
   native;
 }
*)

(* A return-any function may be called, in the same unit, at an instantiation
   whose result convention it never produces at a direct return site: the
   concrete-returning arm is unreachable at that instantiation by GADT
   refinement, so every such call raises before a result is returned.  The
   convention agreement guaranteed by typing is per-execution, so no static
   per-callsite comparison against the callee's direct returns applies. *)

type ('a : any) witness =
  | Int : int witness
  | Float : float# witness

let[@inline never] f : type (a : any). a witness -> a = function
  | Int -> 42
  | Float -> assert false

let[@inline never] use_int () = f Int

let[@inline never] use_float () : float# = f Float

(* A dead callsite may even request an instantiation for which no witness is
   constructible at all. *)
type ('a : any) int_only = Int_only : int int_only

let[@inline never] g : type (a : any). a int_only -> a = function
  | Int_only -> 17

let[@inline never] dead (w : float# int_only) : float# = g w

let () =
  assert (use_int () = 42);
  match use_float () with
  | (_ : float#) -> assert false
  | exception Assert_failure _ -> ()
