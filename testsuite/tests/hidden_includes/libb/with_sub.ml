(* Used by [-open-cmi] tests:
   - [A] is a sub-module whose [x] is a [float], distinguishing it from
     [liba/a.cmi]'s [int] [x] (used to test ordering and shadowing).
   - The top-level [x : string] lets the same cmi be used to test command-line
     order against an [-open A] without a cmi name collision. *)
module A = struct
  type t = float

  let x = 3.14
end

let x = "from With_sub"
