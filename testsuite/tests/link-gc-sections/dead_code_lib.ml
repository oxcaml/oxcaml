exception Deep of int

(* Never referenced: should disappear from executables linked with
   -no-export-dynamic and survive otherwise. *)
let[@inline never] dead x = x * 3 + 1

let[@inline never] frame3 n =
  if n >= 0 then raise (Deep n) else n

let[@inline never] frame2 n = frame3 (n + 1) + 1

let[@inline never] frame1 n = frame2 (n + 1) + 1

let[@inline never] live n = ignore (frame1 n)

(* Allocation across a major GC so that frame descriptors are consulted. *)
let[@inline never] churn n =
  let acc = ref [] in
  for i = 1 to n do
    acc := (i, string_of_int i) :: !acc;
    if i mod 5 = 0 then Gc.full_major ()
  done;
  List.length !acc
