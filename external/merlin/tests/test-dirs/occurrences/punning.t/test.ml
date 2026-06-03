(* Let punning *)
let _ =
  let (let*) = Option.bind in
  let a = Some 1 in
  let* a in
  Some a

(* Record field punning *)
type t = { value : string }
let value = "hello"
let _ = { value }
