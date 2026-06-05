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

(* An or-pattern binds [x] in both arms; typing unifies the two binders, so
   one is an [ok] "merged" drop and the other survives as the named
   binding. *)
type either =
  | Left of int
  | Right of int

let merged e =
  match e with
  | Left x | Right x -> Sys.opaque_identity x

(* [g] is a local function that is only ever applied, so
   [simplify_local_functions] turns it into a static-catch handler and its
   name becomes an [ok] "became-static-catch" drop. *)
let local_fun cond x =
  let g y = Sys.opaque_identity (x + y) in
  if Sys.opaque_identity cond then g 1 else g 2

(* [_unused] is never read, so [simplify_lets] removes it as an [ok]
   "ignored" drop. *)
let ignored pair =
  let kept, _unused = Sys.opaque_identity pair in
  Sys.opaque_identity (kept + 1)
