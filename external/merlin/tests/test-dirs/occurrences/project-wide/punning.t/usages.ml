include Definitions

(* Let punning *)
let _ =
  let (let*) = Option.bind in
  let* a in
  Some a

(* Record field punning *)
let _ = { value }
