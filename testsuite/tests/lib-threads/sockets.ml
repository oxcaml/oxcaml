(* TEST
 include systhreads;
 hassysthreads;
<<<<<<< HEAD
 not-macos;
 libunix;
||||||| 23e84b8c4d
 libunix; (* Broken on Windows (missing join?), needs to be fixed *)
=======
 not-target-windows; (* Broken on Windows (missing join?), needs to be fixed *)
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
 {
   bytecode;
 }{
   native;
 }
*)

open Printf

(* Threads and sockets *)

let serve_connection s =
  let buf = Bytes.make 1024 '>' in
  let n = Unix.read s buf 2 (Bytes.length buf - 2) in
<<<<<<< HEAD
  Thread.delay 0.1;
||||||| 23e84b8c4d
  Thread.delay 1.0;
=======
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  ignore (Unix.write s buf 0 (n + 2));
  Unix.close s

let server sock =
  while true do
    let (s, _) = Unix.accept sock in
    ignore(Thread.create serve_connection s)
  done

<<<<<<< HEAD
let mutex = Mutex.create ()
let lines = ref []

let client (addr, msg) =
||||||| 23e84b8c4d
let client (addr, msg) =
=======
let client1_done = Event.new_channel ()

let wait_for_turn id =
  if id = 2 then
    Event.receive client1_done |> Event.sync |> ignore

let signal_turn id =
  if id = 1 then
    Event.send client1_done 2 |> Event.sync

let client (id, addr) =
  let msg = "Client #" ^ Int.to_string id ^ "\n" in
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  let sock =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.connect sock addr;
  let buf = Bytes.make 1024 ' ' in
  ignore (Unix.write_substring sock msg 0 (String.length msg));
  let n = Unix.read sock buf 0 (Bytes.length buf) in
<<<<<<< HEAD
  Mutex.lock mutex;
  lines := (Bytes.sub buf 0 n) :: !lines;
  Mutex.unlock mutex
||||||| 23e84b8c4d
  print_bytes (Bytes.sub buf 0 n); flush stdout
=======
  wait_for_turn id;
  print_bytes (Bytes.sub buf 0 n); flush stdout;
  signal_turn id
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a

let () =
  let addr = Unix.ADDR_INET(Unix.inet_addr_loopback, 0) in
  let sock =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock addr;
  let addr = Unix.getsockname sock in
  Unix.listen sock 5;
  ignore (Thread.create server sock);
<<<<<<< HEAD
  let client1 = Thread.create client (addr, "Client #1\n") in
  Thread.delay 0.05;
  client (addr, "Client #2\n");
  Thread.join client1;
  List.iter print_bytes (List.sort Bytes.compare !lines);
  flush stdout
||||||| 23e84b8c4d
  ignore (Thread.create client (addr, "Client #1\n"));
  Thread.delay 0.5;
  client (addr, "Client #2\n")
=======
  let c = Thread.create client (2, addr) in
  client (1, addr);
  Thread.join c
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
