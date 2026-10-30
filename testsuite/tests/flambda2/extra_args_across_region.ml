(* TEST *)

(* The following code creates a situation where the result of [Char.chr],
   in tail position relative to [f], goes through two different continuation
   wrappers: one for closing the region associated with [k] (which is inlined
   in Simplif), and one for adding the current value of [r] as an extra
   parameter. A bug in the conversion from Lambda to Flambda used to
   compute extra args on the wrong continuation in this case, causing
   compilation failures later.
*)

let f () =
  let k x = if Some 0 = Some (Sys.opaque_identity 0) then Char.chr x else 'x' in
  let r = ref 65 (* 'A' *) in k !r
let () = print_char (f ())
