(* TEST
 flags += " -x86-peephole-optimize";
 native;
*)

(* Regression test for an x86 peephole bug: [remove_mov_to_dead_register]
   rewrote [mov A, x; mov x, B] into [mov A, B] even when [B] was a memory
   operand whose address uses [x]; the rewritten store then computed its
   address from the stale value of [x].

   [Sys.opaque_identity] forces a reg-to-reg move (the source stays live, so
   the copy needs its own register), and [q.id <- Obj.magic q] compiles to a
   store of the register holding [q] through an address based on that same
   register (the field is immediate, so there is no write barrier). The next
   [Sys.opaque_identity] copy then overwrites that register, making it appear
   dead to the rewrite rule. *)

type t = { mutable id : int }

let[@inline never] stamp2 (p : t) (r : t) =
  let q = Sys.opaque_identity p in
  q.id <- (Obj.magic q : int);
  let s = Sys.opaque_identity r in
  s.id <- (Obj.magic s : int);
  ignore (Sys.opaque_identity p : t);
  ignore (Sys.opaque_identity r : t)

let[@inline never] stamp3 (a : t) (b : t) (c : t) =
  let q = Sys.opaque_identity a in
  q.id <- (Obj.magic q : int);
  let s = Sys.opaque_identity b in
  s.id <- (Obj.magic s : int);
  let u = Sys.opaque_identity c in
  u.id <- (Obj.magic u : int);
  ignore (Sys.opaque_identity a : t);
  ignore (Sys.opaque_identity b : t);
  ignore (Sys.opaque_identity c : t)

let () =
  let p = { id = 0 } and r = { id = 0 } in
  stamp2 p r;
  assert (p.id = (Obj.magic p : int));
  assert (r.id = (Obj.magic r : int));
  let a = { id = 0 } and b = { id = 0 } and c = { id = 0 } in
  stamp3 a b c;
  assert (a.id = (Obj.magic a : int));
  assert (b.id = (Obj.magic b : int));
  assert (c.id = (Obj.magic c : int))
