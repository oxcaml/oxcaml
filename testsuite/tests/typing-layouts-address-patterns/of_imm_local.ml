(* TEST
 include stdlib_stable;
 expect;
*)

open Stdlib_stable

type t
type t_global : value mod global
let use_unyielding : t @ unyielding read -> unit = fun _ -> ()
let use_forkable : t @ forkable read -> unit = fun _ -> ()
[%%expect{|
type t
type t_global : value mod global
val use_unyielding : t @ read -> unit = <fun>
val use_forkable : t @ read -> unit = <fun>
|}]

let via_pat (a : t Addr_imm.t @ local) : t @ read =
  match Addr.of_imm a with
  | addr_ x -> x
[%%expect{|
Line 2, characters 20-21:
2 |   match Addr.of_imm a with
                        ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let via_pat_yielding (a : t Addr_imm.t @ yielding) =
  match Addr.of_imm a with
  | addr_ x -> use_unyielding x
[%%expect{|
Line 2, characters 20-21:
2 |   match Addr.of_imm a with
                        ^
Error: This value is "yielding" but is expected to be "unyielding".
|}]

let via_pat_unforkable (a : t Addr_imm.t @ unforkable) =
  match Addr.of_imm a with
  | addr_ x -> use_forkable x
[%%expect{|
Line 2, characters 20-21:
2 |   match Addr.of_imm a with
                        ^
Error: This value is "unforkable" but is expected to be "forkable".
|}]

let of_imm_local_needs_mod_global (a : t Addr_imm.t @ local) =
  Addr.of_imm_local a
[%%expect{|
Line 2, characters 20-21:
2 |   Addr.of_imm_local a
                        ^
Error: The value "a" has type "t Stdlib_stable.Addr_imm.t" = "t addr_imm"
       but an expression was expected of type
         "'a Stdlib_stable__.Addr_imm.t" = "'a addr_imm"
       The kind of t is value
         because of the definition of t at line 3, characters 0-6.
       But the kind of t must be a subkind of any mod global.
|}]
