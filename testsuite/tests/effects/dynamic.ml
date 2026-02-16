(* TEST
   include systhreads;
   runtime5;
   { bytecode; }
   { native; }
*)

(* Tests the runtime support for dynamic bindings. Dynamic.t isn't yet implemented in the
   OxCaml stdlib, only the runtime support for it, so testing that requires faking a bit
   more infrastructure here in the test case. *)

module Dynamic : sig
  type 'a t

  val make : 'a -> 'a t
  val get : 'a t -> 'a

  val with_temporarily : 'a t -> 'a -> f: (unit -> 'b) -> 'b

end = struct
  type last_fiber : immediate
  type (-'a, +'b) cont
  type 'a t

  external reperform :
    'a Effect.t -> ('a, 'b) cont -> last_fiber -> 'b = "%reperform"

  external with_stack_bind :
    ('a -> 'b) ->
    (exn -> 'b) ->
    ('c Effect.t -> ('c, 'b) cont -> last_fiber -> 'b) ->
    'd t ->
    'd ->
    ('e -> 'a) ->
    'e ->
    'b = "%with_stack_bind"

  external make : 'a -> 'a t = "caml_dynamic_make"
  external get : 'a t -> 'a = "caml_dynamic_get"

  let with_temporarily d v ~f =
    let effc eff k last_fiber = reperform eff k last_fiber in
    with_stack_bind (fun x -> x) (fun e -> raise e) effc d v f ()
end

type _ Effect.t += E : unit -> unit Effect.t

(* `trigger ()` is `(wait, go)`, where `wait()` will wait until `go()` is called.
    and resets the trigger (so go() can be meaningfully called more than once). *)

let trigger () =
  let t = Atomic.make false in
  ((fun () -> (while not (Atomic.get t) do Thread.yield () done; Atomic.set t false)),
   (fun () -> Atomic.set t true))

(* `on_my_mark f` makes a thread and returns a function `go`. When `go()` is called, the
   thread will call `f` and return; `go()` joins the thread before returning. *)

let on_my_mark f =
  let wait, go = trigger () in
  let t = Thread.create (fun () -> (wait(); f())) () in
  fun () -> (go (); Thread.join t)

(* `sync_thread f` make a thread and returns a pair of functions `(go, stop)`.
   Whenever `go()` is called, the other thread runs `f`, synchronously. When
   `stop()` is called, the other thread stops and is joined. *)

let sync_thread f =
  let wait1, go1 = trigger () in
  let wait2, go2 = trigger () in
  let stop = Atomic.make false in
  let t = Thread.create (fun () ->
    let rec loop () =
      wait1 ();
      f ();
      go2 ();
      if not (Atomic.get stop)
      then (loop ())
    in loop ()) () in
  ((fun () -> (go1(); wait2())),
   (fun () -> (Atomic.set stop true; go1(); Thread.join t)))

(* Actual Dynamic.t tests from here on *)

(* `get` should return the `original` value:
   - on the same thread;
   - on another thread;
   - on a thread created before the dynamic itself.
*)

let _ =
  print_endline "\n# Test 1";
  let r = ref (Dynamic.make 57) in
  let go = on_my_mark (fun () -> (Printf.printf "get on earlier thread [expect 1]: %d\n" (Dynamic.get (!r)))) in
  let d = Dynamic.make 1 in
  (Printf.printf "get [expect 1]: %d\n" (Dynamic.get d);
   Thread.join (Thread.create (fun () -> Printf.printf "get on other thread [expect 1]: %d\n" (Dynamic.get d)) ());
   r := d;
   go ())

(* `with_temporarily` should change the value seen:
   - within its dynamic extent;
   - but not on some other thread while it is running;
   - or after it returns;
*)

let test_with_temp d ~root outside n =
  let (wait, go) = trigger () in
  let t = Thread.create (fun () ->
    (wait ();
     Printf.printf "In other thread during with_temporarily [expect %d]: %d\n"
       root
       (Dynamic.get d))) () in
  (Dynamic.with_temporarily d n
     ~f:(fun () -> (Printf.printf "with_temporarily [expect %d]: %d\n" n (Dynamic.get d);
                    go (); Thread.join t;
                    Printf.printf "with_temporarily still [expect %d]: %d\n" n (Dynamic.get d))));
  Printf.printf "after with_temporarily [expect %d]: %d\n" outside (Dynamic.get d)

let _ =
  print_endline "\n# Test 2";
  let d = Dynamic.make 7 in
  (test_with_temp d ~root:7 7 8)

(* Does with_temporarily work correctly in effect handlers? *)

let _ =
  print_endline "\n# Test 3";
  let n = 20 in
  let root = n+10 in
  let d = Dynamic.make root in
  let check_other_thread, finish =
    sync_thread (fun () ->
      Printf.printf "In pre-existing thread [expect %d]: %d\n"
        root
        (Dynamic.get d)) in

  let f () =
    (Printf.printf "In fiber [expect %d]: %d\n" n (Dynamic.get d);
     check_other_thread();
     Dynamic.with_temporarily d (n+1) ~f:(fun () ->
       Effect.perform (E ());
       Printf.printf "In continuation [expect %d]: %d\n" (n+1) (Dynamic.get d))) in

  let h : type a. a Effect.t -> ((a, 'b) Effect.Deep.continuation -> 'b) option = function
    | E () -> Some (fun k ->
      Printf.printf "in handler [expect %d]: %d\n" n (Dynamic.get d);
      test_with_temp d ~root n (n+2);
      check_other_thread();
      Dynamic.with_temporarily d (n+3) ~f:(fun () ->
        Effect.Deep.continue k ();
        Printf.printf "after continuation [expect %d]: %d\n" (n+3) (Dynamic.get d)))
    | e -> None in

  (Dynamic.with_temporarily d n ~f:(fun () ->
   Effect.Deep.match_with f ()
     { retc = (fun () ->
         (Printf.printf "after fiber [expect %d]: %d\n" (n+3) (Dynamic.get d);
          check_other_thread()));
       exnc = (fun e -> raise e);
       effc = h };
   finish()))

(* Does with_temporarily work inside effect contexts? This usefully tests that effects are
   passed up through the with_temporarily context to the outer effect context (testing the
   `reperform` in the implementation of `with_temporarily`. *)

let _ =
  print_endline "\n# Test 4";
  let n = 40 in
  let root = n+10 in
  let d = Dynamic.make root in
  let check_other_thread, finish =
    sync_thread (fun () -> Printf.printf "In pre-existing thread [expect %d]: %d\n" root (Dynamic.get d)) in

  let f () =
    (Printf.printf "In fiber [expect %d]: %d\n" n (Dynamic.get d);
     Dynamic.with_temporarily d (n+1)
     ~f:(fun () -> (check_other_thread();
                    Printf.printf "with_temporarily in fiber [expect %d]: %d\n" (n+1) (Dynamic.get d);
                    Effect.perform (E ());
                    Printf.printf "continuing in with_temporarily [expect %d]: %d\n" (n+1) (Dynamic.get d)));
     Printf.printf "still in continuation, after with_temporarily [expect %d]: %d\n" (n+3) (Dynamic.get d)) in

  let h : type a. a Effect.t -> ((a, 'b) Effect.Deep.continuation -> 'b) option = function
    | E () -> Some (fun k ->
      Printf.printf "in handler [expect %d]: %d\n" n (Dynamic.get d);
      Dynamic.with_temporarily d (n+3) ~f:(fun () ->
        Effect.Deep.continue k ();
        Printf.printf "after continuation returns [expect %d]: %d\n" (n+3) (Dynamic.get d)))
    | e -> None in

  (Dynamic.with_temporarily d n ~f:(fun () ->
   Effect.Deep.match_with f ()
     { retc = (fun () -> (Printf.printf "after fiber [expect %d]: %d\n" (n+3) (Dynamic.get d);
                          check_other_thread();
                          test_with_temp d ~root (n+3) (n+5)));
       exnc = (fun e -> raise e);
       effc = h };
   finish()))
