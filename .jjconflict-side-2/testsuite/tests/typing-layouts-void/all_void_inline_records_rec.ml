(* TEST
 flambda2;
 flags = "-extension layouts_beta";
 { expect; expect.opt; }
 { flags += " -Oclassic"; expect.opt; }
 { flags += " -O3"; expect.opt; }
*)

(* Test that `rec` definitions work like ordinary definitions. *)

type t =
  | A of { mutable a : unit# }
  | B of { mutable b : unit# }
  | C of int
[%%expect{|
type t = A of { mutable a : unit#; } | B of { mutable b : unit#; } | C of int
|}]

(* Test the behavior of side effects as a proxy for evaluation:
   increment a counter, then optionally raise an exception. *)
let run ~should_raise =
  let calls = ref 0 in
  let tick () = incr calls; if should_raise then raise Exit in
  let result =
    try
      let rec x = B { b = (tick (); #()) }
      and get () = x in
      match (Sys.opaque_identity get) () with
      | A _ -> "A"
      | B _ -> "B"
      | C _ -> "C"
    with Exit -> "raised"
  in
  !calls, result
[%%expect{|
val run : should_raise:bool -> int * string = <fun>
|}]

let returned = run ~should_raise:false
[%%expect{|
val returned : int * string = (1, "B")
|}]

let raised = run ~should_raise:true
[%%expect{|
val raised : int * string = (1, "raised")
|}]
