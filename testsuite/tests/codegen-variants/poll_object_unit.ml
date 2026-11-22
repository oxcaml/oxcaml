let rec sum acc n = if n = 0 then acc else sum (acc + n) (n - 1)

(* Counts down from [n] and stops early when [stop] is set: the number left
   is the result. Nothing in the loop lets a signal handler run but a
   poll. *)
let rec spin stop n = if n = 0 || !stop then n else spin stop (n - 1)
