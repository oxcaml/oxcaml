(* TEST
 flambda2;
 {
   flags = "-extension layouts_beta";
   { expect; expect.opt; }
 }{
   flags = "-extension layouts_beta -Oclassic";
   expect.opt;
 }{
   flags = "-extension layouts_beta -O3";
   expect.opt;
 }
*)

let describe x =
  let o = Obj.repr x in
  if Obj.is_int o
  then Printf.sprintf "imm %d" (Obj.obj o : int)
  else Printf.sprintf "block tag %d" (Obj.tag o)
[%%expect{|
val describe : 'a -> string = <fun>
|}]

(* All-void constructors with the attribute are immediates. *)

type imm =
  | A of unit# [@immediate_all_void_constructor]
  | B of #(unit# * unit#) [@immediate_all_void_constructor]
  | C
  | D of int
[%%expect{|
type imm =
    A of unit# [@immediate_all_void_constructor]
  | B of #(unit# * unit#) [@immediate_all_void_constructor]
  | C
  | D of int
|}]

let imm_reprs =
  [describe (A #()); describe (B #(#(), #())); describe C; describe (D 3)]
[%%expect{|
val imm_reprs : string list = ["imm 0"; "imm 1"; "imm 2"; "block tag 0"]
|}]

let imm_match_a =
  match A #() with
  | A v -> let #() = v in "matched A"
  | B _ | C | D _ -> assert false
[%%expect{|
val imm_match_a : string = "matched A"
|}]

let imm_match_b =
  match B #(#(), #()) with
  | B #(v, _) -> let #() = v in "matched B"
  | A _ | C | D _ -> assert false
[%%expect{|
val imm_match_b : string = "matched B"
|}]

let imm_arg_effects =
  let log = ref [] in
  let eff s = log := s :: !log; #() in
  let a = A (eff "A arg") in
  let _ : imm = a in
  let b = B #(eff "B arg 1", eff "B arg 2") in
  let _ : imm = b in
  List.rev !log
[%%expect{|
val imm_arg_effects : string list = ["A arg"; "B arg 2"; "B arg 1"]
|}]

let imm_structural_equal = (A #() = A #())
[%%expect{|
val imm_structural_equal : bool = true
|}]

let imm_hash_equal = Hashtbl.hash (A #()) = Hashtbl.hash (A #())
[%%expect{|
val imm_hash_equal : bool = true
|}]

let imm_marshal_round_trip =
  describe (Marshal.from_string (Marshal.to_string (A #()) []) 0 : imm)
[%%expect{|
val imm_marshal_round_trip : string = "imm 0"
|}]

(* All-void constructors without the attribute are blocks. *)

type blk =
  | A2 of unit#
  | B2 of #(unit# * unit#)
  | C2
  | D2 of int
[%%expect{|
type blk = A2 of unit# | B2 of #(unit# * unit#) | C2 | D2 of int
|}]

let blk_reprs =
  [describe (A2 #()); describe (B2 #(#(), #())); describe C2; describe (D2 3)]
[%%expect{|
val blk_reprs : string list =
  ["block tag 0"; "block tag 1"; "imm 0"; "block tag 2"]
|}]

let blk_match_a =
  match A2 #() with
  | A2 v -> let #() = v in "matched A2"
  | B2 _ | C2 | D2 _ -> assert false
[%%expect{|
val blk_match_a : string = "matched A2"
|}]

let blk_match_b =
  match B2 #(#(), #()) with
  | B2 #(v, _) -> let #() = v in "matched B2"
  | A2 _ | C2 | D2 _ -> assert false
[%%expect{|
val blk_match_b : string = "matched B2"
|}]

let blk_arg_effects =
  let log = ref [] in
  let eff s = log := s :: !log; #() in
  let a = A2 (eff "A2 arg") in
  let _ : blk = a in
  let b = B2 #(eff "B2 arg 1", eff "B2 arg 2") in
  let _ : blk = b in
  List.rev !log
[%%expect{|
val blk_arg_effects : string list = ["A2 arg"; "B2 arg 2"; "B2 arg 1"]
|}]

let blk_structural_equal = (A2 #() = A2 #())
[%%expect{|
val blk_structural_equal : bool = true
|}]

let blk_hash_equal = Hashtbl.hash (A2 #()) = Hashtbl.hash (A2 #())
[%%expect{|
val blk_hash_equal : bool = true
|}]

let blk_marshal_round_trip =
  describe (Marshal.from_string (Marshal.to_string (A2 #()) []) 0 : blk)
[%%expect{|
val blk_marshal_round_trip : string = "block tag 0"
|}]

(* An any-arg constructor refined to void is a block. *)

type ('a : any) refined = R of 'a | S
[%%expect{|
type ('a : any) refined = R of 'a | S
|}]

let refined_reprs =
  let r : unit# refined = R #() in
  [describe r; describe (S : unit# refined)]
[%%expect{|
val refined_reprs : string list = ["block tag 0"; "imm 0"]
|}]

let refined_match =
  match (R #() : unit# refined) with
  | R v -> let #() = v in "matched R"
  | S -> assert false
[%%expect{|
val refined_match : string = "matched R"
|}]
