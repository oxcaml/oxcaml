let rec fib_general ~a0 ~a1 i =
  match i with
  | 0 -> a0
  | 1 -> a1
  | n -> fib_general ~a0:a1 ~a1:(a0 + a1) (i - 1)

let rec collatz_odd n cnt =
  if n = 1
  then cnt
  else if n mod 2 = 1
  then collatz_odd ((3 * n) + 1) (cnt + 1)
  else collatz_even n cnt

and collatz_even n cnt =
  if n = 1
  then cnt
  else if n mod 2 = 0
  then collatz_even (n / 2) (cnt + 1)
  else collatz_odd n cnt

(* This makes sure that caml_apply is treated correctly. LLVM complains when
   parameter counts mismatch for tail calls, but it should be fine in our case.
   We skip that check in LLVM if the calling conventions are OCaml's. *)
let tail_call_outside n x = Tailcall2.tail_call_me (n + 1) (2 * n) x

let () =
  Format.printf "fib_general: %d\n" (fib_general ~a0:3 ~a1:7 5);
  Format.printf "collatz_odd: %d\n" (collatz_odd 27 0);
  tail_call_outside 37 41.3
