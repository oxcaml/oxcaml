(* Closures: tests various closure patterns *)
[@@@ocaml.flambda_o3]

let () = Printf.printf "Closures: initializing\n%!"

(* Multiple non-constant toplevel values *)
let r1 = ref 0
let r2 = ref 1

(* Mutually referencing closures *)
let rec even n =
  if n = 0 then true
  else odd (n - 1)
and odd n =
  if n = 0 then false
  else even (n - 1)

(* Closure capturing multiple values *)
let make_adder () =
  let captured = !r1 + !r2 in
  fun x -> x + captured

let adder = make_adder ()

(* Test the closures *)
let () =
  r1 := 10;
  r2 := 20;
  Printf.printf "Closures: even 10 = %b, odd 10 = %b\n%!" (even 10) (odd 10);
  Printf.printf "Closures: adder 5 = %d\n%!" (adder 5)

(* Minor GC after closure creation - safe during callback *)
let () =
  Gc.minor ();
  Printf.printf "Closures: after Gc.minor, adder 100 = %d\n%!" (adder 100)

let closures_value = adder (Gc_test.final_value mod 100)

let () = Printf.printf "Closures: closures_value = %d\n%!" closures_value
