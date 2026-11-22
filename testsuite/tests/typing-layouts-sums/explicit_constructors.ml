(* TEST
 { bytecode; } { native; }
*)

module Choice = struct
  type t = Stop | Count of int

  let count n = #Count n
  let read = function #Stop -> 0 | #Count n -> n
end

let () =
  assert (Choice.read Choice.#Stop = 0);
  assert (Choice.read (Choice.count 37) = 37);
  assert (match Choice.#Count 19 with Choice.#Count n -> n = 19
                                    | Choice.#Stop -> false)
