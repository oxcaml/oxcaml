(* Used by [-open-cmi] tests: exposes a sub-module [A] whose [x] is a
   [float], distinguishing it from [liba/a.cmi]'s [int] [x]. *)
module A = struct
  type t = float

  let x = 3.14
end
