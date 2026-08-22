let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

(* A three-deep [@inline always] chain [func1] -> [func2] -> [func3], all
   inlined into [f_caller]. The backtrace from a breakpoint inside [func3]'s
   inlined body checks the DW_AT_call_file/line/column attributes of the
   DW_TAG_inlined_subroutine DIEs: each caller frame must be shown at the line
   containing the corresponding call (func2 at the [func3 ...] call, func1 at
   the [func2 ...] call, f_caller at the [func1 ...] call), not at a line inside
   the callee's own body. A regression that emits a frame's own position instead
   of its call site (as was once the case) shifts every frame's location one
   level too deep and loses the [f_caller] call site entirely. *)

let[@inline always] func3 x =
  let y = Sys.opaque_identity (x * 3) in
  y + 1

let[@inline always] func2 x =
  let y = func3 (x + 2) in
  y * Sys.opaque_identity 5

let[@inline always] func1 x =
  let y = func2 (x lxor 1) in
  y + Sys.opaque_identity 7

let[@inline never] [@local never] f_caller x =
  let result = func1 (Sys.opaque_identity x) in
  Sys.opaque_identity result

let _ = f_caller 11
