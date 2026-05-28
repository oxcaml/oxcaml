(* TEST
 flags = "-g -gno-upstream-dwarf -dvariable-availability";
 native;
*)

(* Exercises the source-variable availability diagnostic. The
   [Sys.opaque_identity] calls keep the bindings alive past Flambda 2
   simplification so the Cmm and debug-info checkpoints have something to
   observe. The variables below cover parameters, let-bindings, pattern
   destructuring, [for] loops, captures, and conditional branches. *)

let arith x y =
  let z = Sys.opaque_identity (x + y) in
  let w = Sys.opaque_identity (z * 2) in
  Sys.opaque_identity (w + x)

let branchy a b =
  let c = Sys.opaque_identity (a + b) in
  if Sys.opaque_identity (c > 0)
  then
    let d = Sys.opaque_identity (c * c) in
    Sys.opaque_identity (d - a)
  else
    let e = Sys.opaque_identity (b - c) in
    Sys.opaque_identity (e + a)

let counted n =
  let acc = ref 0 in
  for i = 1 to n do
    let step = Sys.opaque_identity (i * i) in
    acc := Sys.opaque_identity (!acc + step)
  done;
  Sys.opaque_identity !acc

let destructure pair =
  let x, y = Sys.opaque_identity pair in
  Sys.opaque_identity (x + y)

let capturing init =
  let base = Sys.opaque_identity init in
  let bumped =
    let bump n = Sys.opaque_identity (base + n) in
    Sys.opaque_identity (bump 1, bump 2)
  in
  Sys.opaque_identity bumped
