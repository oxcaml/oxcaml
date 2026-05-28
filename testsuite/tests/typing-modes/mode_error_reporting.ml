(* TEST
   expect;
*)

type cont : value mod many

[%%expect{|
type cont : value mod many
|}]

type trigger_state = Awaiting of (unit -> unit) @@ portable

[%%expect{|
type trigger_state = Awaiting of (unit -> unit) @@ portable
|}]

let push (_ : _ @ once unique) = ()

[%%expect{|
val push : 'a @ unique once -> unit = <fun>
|}]

let continue : unit -> (trigger_state ref * cont) @ once unique =
  fun () -> assert false

[%%expect{|
val continue : unit -> trigger_state ref * cont @ unique once = <fun>
|}]

let run () =
  let trigger, k = continue () in
  trigger := Awaiting (fun () -> push k)

[%%expect{|
Uncaught exception: Mode.Submode_error_simple_context(_, _)

|}]
