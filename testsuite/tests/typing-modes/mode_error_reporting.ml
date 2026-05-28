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
Line 3, characters 33-37:
3 |   trigger := Awaiting (fun () -> push k)
                                     ^^^^
Error: The value "push" is "nonportable"
       but is expected to be "portable"
         because it is used inside the function at line 3, characters 22-40
         which is expected to be "portable"
         because it is contained (via constructor "Awaiting") (with some modality) in the value at line 3, characters 13-40.
|}]
