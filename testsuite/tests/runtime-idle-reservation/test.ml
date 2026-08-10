(* TEST
   modules = "stubs.c";
   multicore;
   native;
*)

external reset : int -> int -> unit = "caml_test_idle_reservation_reset"
external reserve : int -> int -> int * int = "caml_test_idle_reservation_reserve"
external completed : unit -> int = "caml_test_idle_reservation_completed"

let rec wait_until predicate =
  if not (predicate ()) then begin
    Domain.cpu_relax ();
    wait_until predicate
  end

let () =
  reset 10 110;
  let ready = Atomic.make 0 in
  let start = Atomic.make false in
  let worker () =
    let observed = completed () in
    Atomic.incr ready;
    wait_until (fun () -> Atomic.get start);
    reserve 110 observed
  in
  let first = Domain.spawn worker in
  let second = Domain.spawn worker in
  wait_until (fun () -> Atomic.get ready = 2);
  Atomic.set start true;
  let first_reserved, _ = Domain.join first in
  let second_reserved, _ = Domain.join second in
  assert (first_reserved + second_reserved = 100);
  assert (completed () = 110);

  reset 20 10;
  let reserved, refreshed = reserve 20 10 in
  assert (reserved = 0);
  assert (refreshed = 20)
