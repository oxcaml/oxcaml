(* TEST
 reference = "${test_source_directory}/deref_alloc.reference";
 flambda2;
 include stdlib_stable;
 {
   native;
 } {
   flags = "-Oclassic";
   native;
 } {
   flags = "-O3";
   native;
 }
*)

(* Dereference patterns on elements of syntactic tuples need the tuple in
   memory. Check that it is allocated at most once. *)

open Stdlib_stable

let words f =
  let before = Gc.minor_words () in
  for _ = 1 to 1000 do f () done;
  let after = Gc.minor_words () in
  int_of_float ((after -. before) /. 1000.)

let check name ~max f =
  let n = words f in
  if n <= max then Printf.printf "%s: ok\n" name
  else Printf.printf "%s: %d words, expected at most %d\n" name n max

type r = { mutable f : int; g : string }

let[@inline never] tupled (deref_ a, b) = Addr_imm.get a + b

let[@inline never] literal_match x y =
  match x, y with deref_ a, _ -> Addr_imm.get a

let[@inline never] literal_match_alias x y =
  match x, y with ((deref_ a, _) as t) -> Addr_imm.get a + snd t

let[@inline never] literal_match_rows x y =
  match x, y with
  | (deref_ a, 0) as t -> Addr_imm.get a + fst t
  | (deref_ a, _) as t -> Addr_imm.get a + snd t

let[@inline never] literal_match_exn x y =
  match x, y with deref_ a, _ -> Addr_imm.get a | exception _ -> 0

let[@inline never] let_constant () =
  let (deref_ a, _) = (1, 2) in Addr_imm.get a

let[@inline never] record r = match r with { f = deref_ a; _ } -> Addr.get a

let () =
  let x = Sys.opaque_identity 1 and y = Sys.opaque_identity 2 in
  let t = Sys.opaque_identity (x, y) in
  let r = Sys.opaque_identity { f = 3; g = "" } in
  (* A tuple is 3 words *)
  check "tupled function, allocated tuple" ~max:0
    (fun () -> ignore (Sys.opaque_identity (tupled t)));
  check "tupled function, syntactic tuple" ~max:3
    (fun () -> ignore (Sys.opaque_identity (tupled (x, y))));
  check "match on a syntactic tuple" ~max:3
    (fun () -> ignore (Sys.opaque_identity (literal_match x y)));
  check "match on a syntactic tuple, with an alias" ~max:3
    (fun () -> ignore (Sys.opaque_identity (literal_match_alias x y)));
  check "match on a syntactic tuple, with several rows" ~max:3
    (fun () -> ignore (Sys.opaque_identity (literal_match_rows x y)));
  check "match on a syntactic tuple, with an exception case" ~max:3
    (fun () -> ignore (Sys.opaque_identity (literal_match_exn x y)));
  check "let-bound constant tuple" ~max:0
    (fun () -> ignore (Sys.opaque_identity (let_constant ())));
  check "record" ~max:0
    (fun () -> ignore (Sys.opaque_identity (record r)))
