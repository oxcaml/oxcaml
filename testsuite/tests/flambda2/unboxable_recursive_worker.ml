(* TEST
 flambda2;
 {
   ocamlopt_flags = "-O2";
   native;
 }{
   ocamlopt_flags = "-O3";
   native;
 }{
   ocamlopt_flags = "-O2 -flambda2-reaper";
   native;
 }{
   ocamlopt_flags = "-O4";
   native;
 }{
   ocamlopt_flags = "-O2 -stubs-forward-inlining";
   native;
 }{
   ocamlopt_flags = "-O3 -stubs-forward-inlining";
   native;
 }
*)

(* Keep the boxed entry available, but eliminate the saturated recursive
   worker's argument/result boxing. Inputs are opaque so the recurrence
   cannot be folded; quarter-integer results have an independent exact oracle. *)
let[@inline never] sum n x =
  let[@unboxable] rec loop i (acc[@unboxable]) =
    if i <= 0 then acc else loop (i - 1) (acc +. x)
  in
  loop n 0.0

let[@inline never] escaped n x =
  let[@unboxable] rec loop i (acc[@unboxable]) =
    if i <= 0 then acc else loop (i - 1) (acc +. x)
  in
  (Sys.opaque_identity loop) n 0.0

let[@inline never] two_accumulators n x =
  let[@unboxable] rec loop i (a[@unboxable]) (b[@unboxable]) =
    if i <= 0 then a +. b
    else loop (i - 1) (a +. x) (b +. x)
  in
  loop n 0.0 0.0

let[@inline never] non_tail n x =
  let[@unboxable] rec loop i (acc[@unboxable]) =
    if i <= 0 then acc
    else 1.0 +. loop (i - 1) (acc +. x) -. 1.0
  in
  loop n 0.0

(* The explicit recursive policies are not requests to route around the
   user's inlining limit. Check their semantics with forwarding enabled too. *)
let[@inline never] never n x =
  let[@unboxable] rec loop i (acc[@unboxable]) =
    if i <= 0 then acc
    else (loop [@inlined never]) (i - 1) (acc +. x)
  in
  loop n 0.0

let[@inline never] unrolled n x =
  let[@unboxable] rec loop i (acc[@unboxable]) =
    if i <= 0 then acc
    else (loop [@unrolled 1]) (i - 1) (acc +. x)
  in
  loop n 0.0

let measure f n x scale =
  Gc.full_major ();
  let before = Gc.minor_words () in
  let result = f n x in
  let allocated = Gc.minor_words () -. before in
  assert (result = float_of_int (max n 0) *. x *. scale);
  allocated

let check f scale =
  let x = Sys.opaque_identity 0.25 in
  ignore (measure f (Sys.opaque_identity (-1)) x scale);
  ignore (measure f (Sys.opaque_identity 0) x scale);
  let small = measure f (Sys.opaque_identity 128) x scale in
  let large = measure f (Sys.opaque_identity 100003) x scale in
  assert (abs_float (large -. small) < 32.0)

let () =
  check sum 1.0;
  check escaped 1.0;
  check two_accumulators 2.0;
  check non_tail 1.0;
  let n = Sys.opaque_identity 128 in
  let x = Sys.opaque_identity 0.25 in
  assert (never n x = 32.0);
  assert (unrolled n x = 32.0);
  let n = Sys.opaque_identity 1000000 in
  let x = Sys.opaque_identity 0.25 in
  assert (sum n x = 250000.0);
  assert (escaped n x = 250000.0)
