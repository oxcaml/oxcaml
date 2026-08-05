(* TEST
 include testing;
*)

let rec @fib n =
  if n < 2 then n else @(fib (n - 1)) + fib (n - 2)

let @empty = []
let ints : int list = empty
let strings : string list = empty
let appended = ints @ ([1; 2])

let @caught =
  try @(raise (Failure "observed")) with Failure _ -> 7

let () =
  assert (fib 6 = 8);
  assert (strings = []);
  assert (appended = [1; 2]);
  assert (caught = 7)
