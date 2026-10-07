(* TEST
 include stdlib_stable;
 expect;
*)

open Stdlib_stable

type r = { f : string }
let use_unyielding : 'a @ unyielding -> unit = fun _ -> ()
[%%expect{|
type r = { f : string; }
val use_unyielding : 'a -> unit = <fun>
|}]

let via_pat (r : r @ local) : string =
  match Addr.of_imm (Addr_imm.of_idx_local r (.f)) with
  | addr_ x -> x
[%%expect{|
Line 2, characters 20-50:
2 |   match Addr.of_imm (Addr_imm.of_idx_local r (.f)) with
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

let via_pat_yielding (r : r @ local) =
  match Addr.of_imm (Addr_imm.of_idx_local r (.f)) with
  | addr_ x -> use_unyielding x
[%%expect{|
Line 2, characters 20-50:
2 |   match Addr.of_imm (Addr_imm.of_idx_local r (.f)) with
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

type g = { g : unit -> unit }
let use_forkable : 'a @ forkable -> unit = fun _ -> ()
[%%expect{|
type g = { g : unit -> unit; }
val use_forkable : 'a -> unit = <fun>
|}]

let via_pat_unforkable (r : g @ local) =
  match Addr.of_imm (Addr_imm.of_idx_local r (.g)) with
  | addr_ x -> use_forkable x
[%%expect{|
Line 2, characters 20-50:
2 |   match Addr.of_imm (Addr_imm.of_idx_local r (.g)) with
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]
