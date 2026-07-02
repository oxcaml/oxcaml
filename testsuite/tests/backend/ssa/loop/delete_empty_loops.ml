(* TEST
 flags = "-O3 -ssa-delete-empty-loops";
 native;
*)

(* Exercises empty-loop deletion. A terminating loop that does no observable
   work may be removed, but a loop whose body has *any* side effect -- even one
   that does not flow through a header parameter -- must be preserved, as must a
   loop whose termination cannot be established. *)

(* Genuinely empty: spins a dead counter, no observable effect. Safe to delete;
   observably a no-op either way. *)
let[@inline never] spin (n : int) =
  for _i = 0 to Sys.opaque_identity n - 1 do
    ()
  done

(* Side effect (print) not flowing through a header parameter: must NOT be
   deleted, so all [n] characters are printed. *)
let[@inline never] noisy (n : int) =
  for _i = 0 to n - 1 do
    print_char 'x'
  done

(* Side effect (store) not flowing through a header parameter: must NOT be
   deleted, so the store happens [n] times. *)
let[@inline never] store_loop (a : int array) (n : int) =
  for _i = 0 to n - 1 do
    a.(0) <- a.(0) + 1
  done

(* Two induction variables compared against each other: the comparison operand
   is loop-variant, so termination is genuinely unknown and the loop (which here
   does terminate because [j] outruns [i]) must run to completion rather than be
   deleted. *)
let[@inline never] two_counter (n : int) =
  let i = ref 0 and j = ref (Sys.opaque_identity n) and hits = ref 0 in
  while !i < !j do
    incr hits;
    i := !i + 2;
    j := !j + 1
  done;
  !hits

let () =
  spin 1000;
  noisy 5;
  print_newline ();
  let a = [| 0 |] in
  store_loop a 100;
  Printf.printf "store=%d\n" a.(0);
  (* i: 0,2,4,...  j: n,n+1,...  gap n closes at rate 1 per step -> n steps *)
  Printf.printf "two_counter=%d\n" (two_counter 10)
