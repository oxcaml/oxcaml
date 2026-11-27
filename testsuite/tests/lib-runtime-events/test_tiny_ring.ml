(* TEST
 {
   include runtime_events;
   ocamlrunparam += ",e=2";
   { bytecode; }
   { native; }
 }
*)

(* A ring of four words: an event of three words only fits once older
   events, and the padding at the end of the ring, have been released. *)
open Runtime_events

type User.tag += Ev

let ev = User.register "tiny_ring.event" Ev Type.int

let received = ref 0

let handler _domain_id _ts e v =
  match User.tag e with
  | Ev -> received := !received + v
  | _ -> ()

let () =
  start ();
  let cursor = create_cursor None in
  let callbacks =
    Callbacks.create () |> Callbacks.add_user_event Type.int handler
  in
  for _ = 1 to 100 do
    User.write ev 1;
    ignore (read_poll cursor callbacks None)
  done;
  assert (!received > 0)
