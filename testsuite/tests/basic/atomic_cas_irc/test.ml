(* r holds 5, so both compare-and-sets expect 1 and must fail *)
let[@inline never] cas_twice (seen : int) r (v : int) =
  ignore (Atomic.compare_and_set r seen v);
  Atomic.compare_and_set r seen v

let () =
  let r = Atomic.make 5 in
  let swapped = cas_twice 1 r 7 in
  Printf.printf "%b %d\n" swapped (Atomic.get r)
