(* A [%probe] inside a [@@tail_mod_cons] function silently breaks the
   constant-stack guarantee of TMC.

   The probe handler is applied in a local-returning mode (see [Texp_probe] in
   translcore.ml, "we conservatively assume that all arguments are local"), so
   the enclosing function keeps a region.  TMC still fires -- a [map_dps]
   variant is generated -- but its recursive call sits inside that region with
   [Rc_normal], so the region has to be closed after the call returns and the
   call stops being a tail call.  Stack usage becomes O(n) instead of O(1).

   The same region shows up on both the naive and the optimized probe path,
   and no warning is emitted at [-w +a].

   BUG: with the stack limit set in the dune rule, this test should print
   30000 and exit 0.  It currently overflows the stack instead, so tmc.expected
   is empty and the rule expects exit code 2.  When TMC and probes are fixed,
   both the expected output and the accepted exit code must be updated. *)

module X = struct
  let side_effect = ()
end

let[@tail_mod_cons] rec map t ~f =
  match [%probe "x" (X.side_effect)]; t with
  | [] -> ([%probe "x" (X.side_effect)]; [])
  | hd :: tl -> f ([%probe "x" (X.side_effect)]; hd) :: map tl ~f

(* Tail-recursive, so building the input does not itself need much stack. *)
let rec build acc n = if n = 0 then acc else build (n :: acc) (n - 1)

(* Large with respect to the stack limit set in the dune rule. *)
let large = 30_000

let () = Printf.printf "%d\n" (List.length (map (build [] large) ~f:succ))
