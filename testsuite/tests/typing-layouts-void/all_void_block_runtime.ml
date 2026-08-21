(* TEST
 reference = "${test_source_directory}/all_void_block_runtime.reference";
 flambda2;
 {
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta -Oclassic";
   native;
 }{
   flags = "-extension layouts_beta -O3";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

let describe name x =
  let o = Obj.repr x in
  if Obj.is_int o
  then Printf.printf "%s: imm %d\n" name (Obj.obj o : int)
  else Printf.printf "%s: block tag %d\n" name (Obj.tag o)

(* All-void constructors with the attribute are immediates. *)

type imm =
  | A of unit# [@immediate_all_void_constructor]
  | B of #(unit# * unit#) [@immediate_all_void_constructor]
  | C
  | D of int

let () =
  describe "imm A" (A #());
  describe "imm B" (B #(#(), #()));
  describe "imm C" C;
  describe "imm D" (D 3);
  (match A #() with
   | A v -> let #() = v in print_endline "imm match: A"
   | B _ | C | D _ -> assert false);
  (match B #(#(), #()) with
   | B #(v, _) -> let #() = v in print_endline "imm match: B"
   | A _ | C | D _ -> assert false);
  let eff s = print_endline s; #() in
  let a = A (eff "imm effect: A arg") in
  let _ : imm = a in
  let b = B #(eff "imm effect: B arg 1", eff "imm effect: B arg 2") in
  let _ : imm = b in
  Printf.printf "imm equal: %b\n" (A #() = A #());
  Printf.printf "imm hash equal: %b\n"
    (Hashtbl.hash (A #()) = Hashtbl.hash (A #()));
  let round_tripped : imm =
    Marshal.from_string (Marshal.to_string (A #()) []) 0
  in
  describe "imm A marshalled" round_tripped

(* All-void constructors without the attribute are blocks. *)

type blk =
  | A2 of unit#
  | B2 of #(unit# * unit#)
  | C2
  | D2 of int

let () =
  describe "blk A2" (A2 #());
  describe "blk B2" (B2 #(#(), #()));
  describe "blk C2" C2;
  describe "blk D2" (D2 3);
  (match A2 #() with
   | A2 v -> let #() = v in print_endline "blk match: A2"
   | B2 _ | C2 | D2 _ -> assert false);
  (match B2 #(#(), #()) with
   | B2 #(v, _) -> let #() = v in print_endline "blk match: B2"
   | A2 _ | C2 | D2 _ -> assert false);
  let eff s = print_endline s; #() in
  let a = A2 (eff "blk effect: A2 arg") in
  let _ : blk = a in
  let b = B2 #(eff "blk effect: B2 arg 1", eff "blk effect: B2 arg 2") in
  let _ : blk = b in
  Printf.printf "blk equal: %b\n" (A2 #() = A2 #());
  Printf.printf "blk hash equal: %b\n"
    (Hashtbl.hash (A2 #()) = Hashtbl.hash (A2 #()));
  let round_tripped : blk =
    Marshal.from_string (Marshal.to_string (A2 #()) []) 0
  in
  describe "blk A2 marshalled" round_tripped

(* An any-arg constructor refined to void is a block. *)

type ('a : any) refined = R of 'a | S

let () =
  let r : unit# refined = R #() in
  describe "refined R" r;
  describe "refined S" S;
  (match r with
   | R v -> let #() = v in print_endline "refined match: R"
   | S -> assert false)
