(* TEST
 include stdlib_stable;
 flambda2;
 native;
*)

(* A test comparing allocations when using untagged [int16#]es to allocations
   when using tagged [int16s]. *)

(* Hide polymorphic equality *)
let ( = ) = Int.equal

module Int16 = struct
  include Stdlib_stable.Int16

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( // ) = unsigned_div
  let ( % ) = rem
  let ( %% ) = unsigned_rem
  let ( = ) = equal
end

module Int16_u = struct
  include Stdlib_stable.Int16_u

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( // ) = unsigned_div
  let ( % ) = rem
  let ( %% ) = unsigned_rem
  let ( = ) = equal
end

let baseline_allocation =
  let first  = Gc.allocated_bytes () in
  let second = Gc.allocated_bytes () in
  second -. first

let measure_alloc fmt f =
  let before = Gc.allocated_bytes () in
  let result = (f[@inlined never]) () in
  let after = Gc.allocated_bytes () in
  let alloc = (after -. before) -. baseline_allocation in
  Printf.printf (fmt ^^ "; %s\n")
    (Int16_u.to_int result)
    (if alloc > 0.0 then "allocated" else "did not allocate")

(* We mark key functions [[@inline never]].  Without this, flambda2 might be
   able to eliminate all allocations in the tagged case, and it's important to
   have neither inlined for a fair comparison.  (This was the case in the
   [float64] version of this test.) *)

module Collatz_untagged = struct
  open Int16_u

  let[@inline never] rec collatz_count' count n =
    if n = of_int16 (Int16.of_int (1)) then
      count
    else
      collatz_count'
        (succ count)
        (if n %% of_int16 (Int16.of_int (2)) = of_int16 (Int16.of_int (0)) then
           n // of_int16 (Int16.of_int (2))
         else
           of_int16 (Int16.of_int (3)) * n + of_int16 (Int16.of_int (1)))

  let collatz_count n = collatz_count' (of_int16 (Int16.of_int (0))) n

  let go () =
    measure_alloc "Untagged: Collatz took %d steps to reach 1"
      (fun () -> collatz_count (of_int16 (Int16.of_int (27))))
end

module Collatz_tagged = struct
  open Int16

  let[@inline never] rec collatz_count' count n =
    if n = (Int16.of_int (1)) then
      count
    else
      collatz_count'
        (succ count)
        (if n %% (Int16.of_int (2)) = (Int16.of_int (0)) then
           n // (Int16.of_int (2))
         else
           (Int16.of_int (3))*n + (Int16.of_int (1)))

  let collatz_count n = Int16_u.of_int16 (collatz_count' (Int16.of_int (0)) n)

  let go () =
    measure_alloc "Tagged: Collatz took %d steps to reach 1"
      (fun () -> collatz_count (Int16.of_int (27)))
end

let () = Collatz_untagged.go ()
let () = Collatz_tagged.go ()
