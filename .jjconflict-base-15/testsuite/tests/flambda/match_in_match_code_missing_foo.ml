
type t =
  | Tagged of string * string

type error =
  | Lazy of (unit -> t)
  | Foo of t

let create ?strict tag x msg_of_x =
  match strict with
  | None -> Lazy (fun () -> Tagged (tag, msg_of_x x))
  | Some () -> Foo (Tagged (tag, (msg_of_x x)))


