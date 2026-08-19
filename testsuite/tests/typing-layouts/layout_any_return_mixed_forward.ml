(* TEST
 flags = "-extension layouts_beta";
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 }
*)

(* A return-any function may mix a direct concrete return with a residual
   tail-forward to another unknown-result function.  The caller's result
   convention then comes from the forwarded callee on one path and from the
   direct return on the other. *)

module F = Stdlib_upstream_compatible.Float_u

type (_ : any) fwd_w = F_float : float# fwd_w

let[@inline never] forward : type (a : any). a fwd_w -> a =
  fun F_float -> #3.14

type (_ : any) w =
  | W_int : int w
  | W_fwd : ('a : any). 'a fwd_w -> 'a w

let[@inline never] f : type (a : any). a w -> a =
  fun w ->
    match w with
    | W_int -> 42
    | W_fwd fw -> forward fw

let[@inline never] use_int () = f W_int

let[@inline never] use_float () : float# = f (W_fwd F_float)

let () =
  assert (use_int () = 42);
  assert (Float.equal (F.to_float (use_float ())) 3.14)
