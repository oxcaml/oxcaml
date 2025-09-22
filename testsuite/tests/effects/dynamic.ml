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
  val set_root : 'a t -> 'a -> unit

  val with_temporarily : 'a t -> 'a -> f: (unit -> 'b) -> 'b

end = struct
  type (-'a, +'b) stack : immediate
  type last_fiber : immediate
  type (-'a, +'b) cont
  type 'a t

  external reperform :
    'a Effect.t -> ('a, 'b) cont -> last_fiber -> 'b = "%reperform"

  module Must_not_enter_gc = struct
    external alloc_stack_dyn :
      ('a -> 'b)
      -> (exn -> 'b)
      -> ('c Effect.t -> ('c, 'b) cont -> last_fiber -> 'b)
      -> 'd t
      -> 'd
      -> ('a, 'b) stack = "caml_alloc_stack_bind"

    external runstack : ('a, 'b) stack -> ('c -> 'a) -> 'c -> 'b = "%runstack"

    let[@inline never] with_stack_dyn valuec exnc effc d v f x =
      runstack (alloc_stack_dyn valuec exnc effc d v) f x
  end

  external make : 'a -> 'a t = "caml_dynamic_make"
  external get : 'a t -> 'a = "caml_dynamic_get"
  external set_root : 'a t -> 'a -> unit = "caml_dynamic_set_root"

  let with_temporarily d v ~f =
    let effc eff k last_fiber = reperform eff k last_fiber in
    Must_not_enter_gc.with_stack_dyn (fun x -> x) (fun e -> raise e) effc d v f ()
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
  let t = Thread.create (fun () -> (while not (Atomic.get stop) do
                                      wait1 ();
                                      f ();
                                      go2()
                                    done)) () in
  ((fun () -> (go1(); wait2())),
   (fun () -> (Atomic.set stop true; go1(); Thread.join t)))

(* Actual Dynamic.t tests from here on *)

(* `get` should return the `original` value:
   - on the same thread;
   - on another thread;
   - on a thread created before the dynamic itself.
*)

let _ =
  let r = ref (Dynamic.make 57) in
  let go = on_my_mark (fun () -> (Printf.printf "get on earlier thread [expect 1]: %d\n" (Dynamic.get (!r)))) in
  let d = Dynamic.make 1 in
  (Printf.printf "get [expect 1]: %d\n" (Dynamic.get d);
   Thread.join (Thread.create (fun () -> Printf.printf "get on other thread [expect 1]: %d\n" (Dynamic.get d)) ());
   r := d;
   go ())

(* `set_root` should change the value seen:
   - on this thread;
   - on a child thread created after `set_root`;
   - but not on a child thread created previously.
*)

let _ =
  let r = ref (Dynamic.make 57) in
  let go = on_my_mark (fun () -> (Printf.printf "get on pre-existing thread after set_root [expect 3]: %d\n" (Dynamic.get (!r)))) in
  let d = Dynamic.make 3 in
  (Dynamic.set_root d 2;
   Printf.printf "get after set_root [expect 2]: %d\n" (Dynamic.get d);
   Thread.join (Thread.create (fun () -> Printf.printf "get on child thread created after set_root [expect 2]: %d\n" (Dynamic.get d)) ());
   r := d;
   go ();
   Thread.join (Thread.create (fun () -> Dynamic.set_root d 4) ());
   Printf.printf "get after set_root on child thread [expect 2]: %d\n" (Dynamic.get d))

(* `with_temporarily` should change the value seen:
   - within its dynamic extent;
   - but not on some other thread while it is running;
   - or after it returns;
*)

let test_with_temp d n =
  let (wait, go) = trigger () in
  let outside = Dynamic.get d in
  let t = Thread.create (fun () -> (wait (); Printf.printf "In other thread during with_temporarily [expect %d]: %d\n" outside (Dynamic.get d))) () in
  (Dynamic.with_temporarily d n
     ~f:(fun () -> (Printf.printf "with_temporarily [expect %d]: %d\n" n (Dynamic.get d);
                    go (); Thread.join t;
                    Printf.printf "with_temporarily still [expect %d]: %d\n" n (Dynamic.get d)));
   Printf.printf "after with_temporarily [expect %d]: %d\n" outside (Dynamic.get d))

let _ = let d = Dynamic.make 42 in
  (Dynamic.set_root d 7; test_with_temp d 6)

(* In the presence of effects, check that set_root values are visible in other fibers,
   without contaminating the value on a pre-existing thread. *)

let _ =

  let n = 10 in
  let outside = n+10 in
  let d = Dynamic.make outside in
  let check_other_thread, finish =
    sync_thread (fun () -> Printf.printf "In pre-existing thread [expect %d]: %d\n" outside (Dynamic.get d)) in

  let f () =
    (Printf.printf "In fiber [expect %d]: %d\n" n (Dynamic.get d);
     check_other_thread();
     Dynamic.set_root d (n+1);
     Effect.perform (E ());
     Printf.printf "In continuation [expect %d]: %d\n" (n+2) (Dynamic.get d);
     Dynamic.set_root d (n+3)) in

  let h : type a. a Effect.t -> ((a, 'b) Effect.Deep.continuation -> 'b) option = function
    | E () -> Some (fun k ->
      Printf.printf "in handler [expect %d]: %d\n" (n+1) (Dynamic.get d);
      check_other_thread();
      Dynamic.set_root d (n+2);
      Effect.Deep.continue k ();
      Printf.printf "after continuation [expect %d]: %d\n" (n+4) (Dynamic.get d))
    | e -> None in

  Dynamic.set_root d n;
  Effect.Deep.match_with f ()
    { retc = (fun () -> (Printf.printf "after fiber [expect %d]: %d\n" (n+3) (Dynamic.get d);
                         check_other_thread();
                         Dynamic.set_root d (n+4)));
      exnc = (fun e -> raise e);
      effc = h };
  finish()

(* Does with_temporarily work correctly in effect handlers? *)

let _ =

  let n = 20 in
  let outside = n+10 in
  let d = Dynamic.make outside in
  let check_other_thread, finish =
    sync_thread (fun () -> Printf.printf "In pre-existing thread [expect %d]: %d\n" outside (Dynamic.get d)) in

  let f () =
    (Printf.printf "In fiber [expect %d]: %d\n" n (Dynamic.get d);
     check_other_thread();
     Dynamic.set_root d (n+1);
     Effect.perform (E ());
     Printf.printf "In continuation [expect %d]: %d\n" (n+3) (Dynamic.get d);
     Dynamic.set_root d (n+4)) in

  let h : type a. a Effect.t -> ((a, 'b) Effect.Deep.continuation -> 'b) option = function
    | E () -> Some (fun k ->
      Printf.printf "in handler [expect %d]: %d\n" (n+1) (Dynamic.get d);
      test_with_temp d (n+2);
      check_other_thread();
      Dynamic.set_root d (n+3);
      Effect.Deep.continue k ();
      Printf.printf "after continuation [expect %d]: %d\n" (n+5) (Dynamic.get d))
    | e -> None in

  (Dynamic.set_root d n;
   Effect.Deep.match_with f ()
     { retc = (fun () -> (Printf.printf "after fiber [expect %d]: %d\n" (n+4) (Dynamic.get d);
                          check_other_thread();
                          Dynamic.set_root d (n+5)));
       exnc = (fun e -> raise e);
       effc = h };
   finish())

(* Does with_temporarily work inside effect contexts? This usefully tests that effects are
   passed up through the with_temporarily context to the outer effect context (testing the
   `reperform` in the implementation of `with_temporarily`. *)

let _ =
  let n = 40 in
  let outside = n+10 in
  let d = Dynamic.make outside in
  let check_other_thread, finish =
    sync_thread (fun () -> Printf.printf "In pre-existing thread [expect %d]: %d\n" outside (Dynamic.get d)) in

  let f () =
    (Printf.printf "In fiber [expect %d]: %d\n" n (Dynamic.get d);
     Dynamic.with_temporarily d (n+1)
     ~f:(fun () -> (check_other_thread();
                    Dynamic.set_root d (n+2);
                    (* set_root shouldn't affect the value because we're in a with_temp *)
                    Printf.printf "with_temporarily in fiber [expect %d]: %d\n" (n+1) (Dynamic.get d);
                    Effect.perform (E ());
                    Printf.printf "continuing in with_temporarily [expect %d]: %d\n" (n+1) (Dynamic.get d)));
     Printf.printf "still in continuation, after with_temporarily [expect %d]: %d\n" (n+3) (Dynamic.get d);
     Dynamic.set_root d (n+4)) in

  let h : type a. a Effect.t -> ((a, 'b) Effect.Deep.continuation -> 'b) option = function
    | E () -> Some (fun k ->
      Printf.printf "in handler [expect %d): %d\n" (n+2) (Dynamic.get d);
      Dynamic.set_root d (n+3);
      Effect.Deep.continue k ();
      Printf.printf "after continuation returns [expect %d]: %d\n" (n+6) (Dynamic.get d))
    | e -> None in

  (Dynamic.set_root d n;
   Effect.Deep.match_with f ()
     { retc = (fun () -> (Printf.printf "after fiber [expect %d]: %d\n" (n+4) (Dynamic.get d);
                          check_other_thread();
                          test_with_temp d (n+5);
                          Dynamic.set_root d (n+6)));
       exnc = (fun e -> raise e);
       effc = h };
   finish())
