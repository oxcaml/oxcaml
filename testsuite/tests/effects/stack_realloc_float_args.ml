(* TEST
 flags += " -no-cfg-stack-checks";
 stack-checks;
 native;
*)

(* With [-no-cfg-stack-checks] the stack check is at function entry, where the
   unboxed float arguments are live in registers: growing the stack there must
   preserve them. [sum] is entered at every depth of a fresh fiber, so that for
   some depths its entry check is the one that grows the stack. *)

external to_float : float# -> float = "%box_float"

let[@inline never] nop () = ()

let[@inline never] sum (a : float#) (b : float#) (c : float#) (d : float#) =
  nop ();
  to_float a +. to_float b +. to_float c +. to_float d

let rec deep n = if n = 0 then sum #1. #2. #4. #8. else deep (n - 1) +. 16.

let () =
  for depth = 0 to 200 do
    let r =
      Effect.Deep.match_with deep depth
        { retc = Fun.id; exnc = raise; effc = (fun _ -> None) }
    in
    assert (r = 15. +. (16. *. float_of_int depth))
  done
