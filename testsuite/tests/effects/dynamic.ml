(* TEST
   include systhreads;
   { bytecode; }
   { native; }
*)

(* Tests the runtime support for dynamic bindings. Dynamic.t isn't yet implemented in the
   OxCaml stdlib, only the runtime support for it, so testing that requires faking a bit
   more infrastructure here in the test case. *)

module type Dynamic_S = sig
  type 'a t

  val make : inherit_:bool -> 'a t
  val get : 'a t -> 'a or_null

  val with_temporarily : 'a t -> 'a -> f: (unit -> 'b) -> 'b
end

module Dynamic : Dynamic_S = struct
  type 'a t

  external make : inherit_:bool -> 'a t = "caml_dynamic_make"
  external get : 'a t -> 'a or_null = "caml_dynamic_get"
  external push : 'a t -> 'a -> unit  = "caml_dynamic_push"
  external pop : 'a t -> unit = "caml_dynamic_pop"

  let with_temporarily d v ~f =
    push d v;
    Fun.protect f ~finally:(fun () -> pop d)
end

module Dynamic_inside_fiber : Dynamic_S = struct
  type 'a t

  external make : inherit_:bool -> 'a t = "caml_dynamic_make"
  external get : 'a t -> 'a or_null = "caml_dynamic_get"
  external push : 'a t -> 'a -> unit  = "caml_dynamic_push"
  external pop : 'a t -> unit = "caml_dynamic_pop"

  let with_temporarily d v ~f =
    Effect.Deep.match_with
      (fun () ->
        push d v;
        Fun.protect f ~finally:(fun () -> pop d)) ()
      { retc = (fun v -> v);
        exnc = (fun e -> raise e);
        effc = (fun (type a) (_ : a Effect.t) -> None) }
end

module Dynamic_outside_fiber : Dynamic_S = struct
  type 'a t

  external make : inherit_:bool -> 'a t = "caml_dynamic_make"
  external get : 'a t -> 'a or_null = "caml_dynamic_get"
  external push : 'a t -> 'a -> unit  = "caml_dynamic_push"
  external pop : 'a t -> unit = "caml_dynamic_pop"

  let with_temporarily d v ~f =
    push d v;
    Fun.protect (fun () ->
      Effect.Deep.match_with f ()
        { retc = (fun v -> v);
          exnc = (fun e -> raise e);
          effc = (fun (type a) (_ : a Effect.t) -> None) })
      ~finally:(fun () -> pop d)
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

let print_null = function
  | This x -> Int.to_string x
  | Null -> "null"

(* Actual Dynamic.t tests from here on *)

module Test_with_temp (Dynamic : Dynamic_S) = struct

  let print_dyn d = print_null (Dynamic.get d)

  (* `get` should return null
    - on the same thread;
    - on another thread;
  *)

  let _ =
    print_endline "\n# Test 1";
    let d = Dynamic.make ~inherit_:false in
    Printf.printf "get [expect null]: %s\n" (print_dyn d);
    Thread.create (fun () -> Printf.printf "get on other thread [expect null]: %s\n" (print_dyn d)) ()
    |> Thread.join

  (* `with_temporarily` should change the value seen:
    - within its dynamic extent;
    - but not on some other thread while it is running;
    - or after it returns;
  *)

  let test_with_temp d outside n =
    let (wait, go) = trigger () in
    let t = Thread.create (fun () ->
      (wait ();
      Printf.printf "In other thread during with_temporarily [expect null]: %s\n"
        (print_dyn d))) () in
    (Dynamic.with_temporarily d n
      ~f:(fun () -> (Printf.printf "with_temporarily [expect %d]: %s\n" n (print_dyn d);
                      go (); Thread.join t;
                      Printf.printf "with_temporarily still [expect %d]: %s\n" n (print_dyn d))));
    Printf.printf "after with_temporarily [expect %s]: %s\n" (print_null outside) (print_dyn d)

  let _ =
    print_endline "\n# Test 2";
    let d = Dynamic.make ~inherit_:false in
    (test_with_temp d Null 8)

  (* Does with_temporarily work correctly in effect handlers? *)

  let _ =
    print_endline "\n# Test 3";
    let n = 20 in
    let d = Dynamic.make ~inherit_:false in
    let check_other_thread, finish =
      sync_thread (fun () ->
        Printf.printf "In pre-existing thread [expect null]: %s\n"
          (print_dyn d)) in

    let f () =
      (Printf.printf "In fiber [expect %d]: %s\n" n (print_dyn d);
      check_other_thread();
      Dynamic.with_temporarily d (n+1) ~f:(fun () ->
        Effect.perform (E ());
        Printf.printf "In continuation [expect %d]: %s\n" (n+1) (print_dyn d))) in

    let h : type a. a Effect.t -> ((a, 'b) Effect.Deep.continuation -> 'b) option = function
      | E () -> Some (fun k ->
        Printf.printf "in handler [expect %d]: %s\n" n (print_dyn d);
        test_with_temp d (This n) (n+2);
        check_other_thread();
        Dynamic.with_temporarily d (n+3) ~f:(fun () ->
          Effect.Deep.continue k ();
          Printf.printf "after continuation [expect %d]: %s\n" (n+3) (print_dyn d)))
      | e -> None in

    (Dynamic.with_temporarily d n ~f:(fun () ->
    Effect.Deep.match_with f ()
      { retc = (fun () ->
          (Printf.printf "after fiber [expect %d]: %s\n" (n+3) (print_dyn d);
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
    let d = Dynamic.make ~inherit_:false in
    let check_other_thread, finish =
      sync_thread (fun () -> Printf.printf "In pre-existing thread [expect null]: %s\n" (print_dyn d)) in

    let f () =
      (Printf.printf "In fiber [expect %d]: %s\n" n (print_dyn d);
      Dynamic.with_temporarily d (n+1)
      ~f:(fun () -> (check_other_thread();
                      Printf.printf "with_temporarily in fiber [expect %d]: %s\n" (n+1) (print_dyn d);
                      Effect.perform (E ());
                      Printf.printf "continuing in with_temporarily [expect %d]: %s\n" (n+1) (print_dyn d)));
      Printf.printf "still in continuation, after with_temporarily [expect %d]: %s\n" (n+3) (print_dyn d)) in

    let h : type a. a Effect.t -> ((a, 'b) Effect.Deep.continuation -> 'b) option = function
      | E () -> Some (fun k ->
        Printf.printf "in handler [expect %d]: %s\n" n (print_dyn d);
        Dynamic.with_temporarily d (n+3) ~f:(fun () ->
          Effect.Deep.continue k ();
          Printf.printf "after continuation returns [expect %d]: %s\n" (n+3) (print_dyn d)))
      | e -> None in

    (Dynamic.with_temporarily d n ~f:(fun () ->
    Effect.Deep.match_with f ()
      { retc = (fun () -> (Printf.printf "after fiber [expect %d]: %s\n" (n+3) (print_dyn d);
                            check_other_thread();
                            test_with_temp d (This (n+3)) (n+5)));
        exnc = (fun e -> raise e);
        effc = h };
    finish()))
end

let () =
  Printf.printf "\n[Dynamic]\n";
  let module _ = Test_with_temp (Dynamic) in
  Printf.printf "\n[Dynamic_outside_fiber]\n";
  let module _ = Test_with_temp (Dynamic_outside_fiber) in
  Printf.printf "\n[Dynamic_inside_fiber]\n";
  let module _ = Test_with_temp (Dynamic_inside_fiber) in
  ()
