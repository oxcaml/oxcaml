(* TEST
 flambda2;
 flags = "-extension layouts_beta";
 { expect; expect.opt; }
 { flags += " -Oclassic"; expect.opt; }
 { flags += " -O3"; expect.opt; }
*)

(* Recursive initialization preserves a nonzero constructor tag even when
   its payload is empty. Its initializer runs exactly once, including when
   it raises. No physical equality is required. *)
type t =
  | A of { mutable a : unit# }
  | B of { mutable b : unit# }
  | C of int
let recursive =
  let run should_raise =
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
  in
  run false, run true
[%%expect{|
type t = A of { mutable a : unit#; } | B of { mutable b : unit#; } | C of int
val recursive : (int * string) * (int * string) = ((1, "B"), (1, "raised"))
|}]
