(* TEST
 {
   include runtime_events;
   arch64;
   ocamlrunparam += ",d=1,e=28";
   { bytecode; }
   { native; }
 }
*)

(* A single ring of 2^28 words makes the ring buffers file a little larger
   than 2^31 bytes. The file is sparse and only the pages that are written
   are backed by memory. *)
open Runtime_events

type User.tag += Ev

let ev = User.register "large_ring.event" Ev Type.int

let got = ref 0

let handler _domain_id _ts e v =
  match User.tag e with
  | Ev -> got := v
  | _ -> ()

let () =
  start ();
  let ring_file = Option.get (path ()) in
  let size = In_channel.with_open_bin ring_file In_channel.length in
  assert (size > Int64.shift_left 1L 31);
  User.write ev 42;
  let cursor = create_cursor None in
  let callbacks =
    Callbacks.create () |> Callbacks.add_user_event Type.int handler
  in
  ignore (read_poll cursor callbacks None);
  (* The names of user events are stored after the rings. *)
  assert (!got = 42)
