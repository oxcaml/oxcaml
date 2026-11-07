(* TEST
 flags += " -O3 -regalloc irc -regalloc-validate";
 flags += " -regalloc-param STACK_SLOTS_THRESHOLD:1";
 flags += " -regalloc-param SPLIT_REMATERIALIZE:on";
 {
   native;
 }
 {
   flags += " -regalloc-param IRC_INTERF_THRESHOLD:0";
   native;
 }
 {
   flags += " -regalloc ls";
   native;
 }
 {
   flags += " -regalloc gi";
   native;
 }
*)

(* [r] is live across the call to [print_int] and rematerialized (as a load
   from [p]) after it, rather than reloaded; the spill of [r] inserted by the
   split pass is then dead and deleted, which leaves its stack slot unused.
   Lowering [STACK_SLOTS_THRESHOLD] disables the stack-slot packing that would
   otherwise hide the unused slot; the validator then requires it to have been
   removed. The second run forces IRC to give up after splitting, so the
   prepared CFG and its metadata must reach the linear-scan fallback. *)

type t =
  { x : int;
    y : int
  }

let[@inline never] mk n = { x = n; y = n + 1 }

let[@inline never] g n =
  let p = mk n in
  let r = p.x in
  print_int r;
  r + p.y

let () = Printf.printf "\n%d\n" (g 5)

type nested = { a : t; z : int; other : int }

let[@inline never] consume x z = Printf.printf "%d %d\n" x z

(* Rematerializing [x] and [z] between the calls introduces uses of [a] and
   [p]. Splitting must retain [p]'s reload and rematerialize [a] before [x],
   without relying on a second prelude during fallback. *)
let[@inline never] cascade p =
  let a = p.a in
  let x = a.x in
  let z = p.z in
  consume x z;
  consume x z;
  a.y + p.other

let () =
  let p = Sys.opaque_identity { a = { x = 3; y = 5 }; z = 7; other = 11 } in
  Printf.printf "%d\n" (cascade p)
