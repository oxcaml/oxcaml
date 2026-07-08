(* TEST
   modules = "dynamic_inherit_stubs.c";
   include systhreads;
   multicore;
   { bytecode; }
   { native; }
*)

module Dynamic = struct
  type 'a t

  external make : inherit_:bool -> 'a t = "caml_dynamic_make"
  external get : 'a t -> 'a or_null = "caml_dynamic_get"
  external push : 'a t -> 'a -> unit = "caml_dynamic_push"
  external pop : 'a t -> unit = "caml_dynamic_pop"

  let with_temporarily d v ~f =
    push d v;
    Fun.protect f ~finally:(fun () -> pop d)
end

let str = function This n -> Int.to_string n | Null -> "null"

let show inh non =
  Printf.sprintf "inheritable=%s non-inheritable=%s"
    (str (Dynamic.get inh)) (str (Dynamic.get non))

external run_in_c_thread : (unit -> unit) -> unit =
  "dynamic_inherit_run_in_c_thread"

external is_inherit : _ Dynamic.t -> bool = "caml_dynamic_is_inherited"

let test_is_inherit () =
  let inh = Dynamic.make ~inherit_:true and non = Dynamic.make ~inherit_:false in
  Printf.printf "  is_inherit  [expect true/false]: %b/%b\n\n" (is_inherit inh) (is_inherit non)

(* [trigger ()] is [(wait, go)]: [wait ()] blocks until [go ()] is called. *)
let trigger () =
  let t = Atomic.make false in
  ( (fun () ->
      while not (Atomic.get t) do Thread.yield () done;
      Atomic.set t false),
    (fun () -> Atomic.set t true) )

(* Domains: a spawned domain runs [domain_create] on its own fresh main stack,
   with no enclosing binding to capture, so neither kind is inherited. *)
let test_domain () =
  print_endline "# Domains";
  let inh = Dynamic.make ~inherit_:true and non = Dynamic.make ~inherit_:false in
  Dynamic.with_temporarily inh 1 ~f:(fun () ->
    Dynamic.with_temporarily non 1 ~f:(fun () ->
      Printf.printf "  spawning domain [expect 1/1]:       %s\n" (show inh non);
      let observed = Domain.join (Domain.spawn (fun () -> show inh non)) in
      Printf.printf "  spawned domain  [expect null/null]: %s\n" observed))

(* Threads: a new thread's main stack is allocated by the creating thread, so
   it captures the inheritable binding current at [Thread.create] but never the
   non-inheritable one. *)
let test_thread () =
  print_endline "\n# Threads";
  let inh = Dynamic.make ~inherit_:true and non = Dynamic.make ~inherit_:false in
  Dynamic.with_temporarily inh 2 ~f:(fun () ->
    Dynamic.with_temporarily non 2 ~f:(fun () ->
      Printf.printf "  spawning thread [expect 2/2]:    %s\n" (show inh non);
      let observed = ref "" in
      let t = Thread.create (fun () -> observed := show inh non) () in
      Thread.join t;
      Printf.printf "  new thread      [expect 2/null]: %s\n" !observed))

(* The inherited value is a snapshot taken at thread-creation time, not a live
   view of the parent's binding. *)
let test_thread_snapshot () =
  print_endline "\n# Threads (snapshot at creation)";
  let inh = Dynamic.make ~inherit_:true in
  let wait, go = trigger () in
  Dynamic.push inh 3;
  let observed = ref "" in
  let t =
    Thread.create (fun () -> wait (); observed := str (Dynamic.get inh)) ()
  in
  (* Rebind in the parent after the thread has captured its snapshot. *)
  Dynamic.push inh 99;
  go ();
  Thread.join t;
  Printf.printf "  parent now [expect 99]: %s\n" (str (Dynamic.get inh));
  Printf.printf "  thread     [expect 3]:  %s\n" !observed;
  Dynamic.pop inh;
  Dynamic.pop inh

(* C-registered threads allocate their stack during [caml_c_thread_register].
   Even if they register while an OCaml thread has released the runtime lock
   inside a binding, they should start with an empty dynamic environment. *)
let test_c_thread () =
  print_endline "\n# C-registered threads";
  let inh = Dynamic.make ~inherit_:true and non = Dynamic.make ~inherit_:false in
  Dynamic.with_temporarily inh 5 ~f:(fun () ->
    Dynamic.with_temporarily non 5 ~f:(fun () ->
      Printf.printf "  registering C thread [expect 5/5]:       %s\n"
        (show inh non);
      let observed = ref "" in
      run_in_c_thread (fun () -> observed := show inh non);
      Printf.printf "  C thread             [expect null/null]: %s\n"
        !observed))

(* Fibers: a fiber is created with its creator as parent, so a directly-run
   fiber sees both kinds (the non-inheritable one through the parent chain).
   The difference appears once the fiber's continuation is resumed in a context
   that no longer has the bindings: the inheritable snapshot survives, the
   non-inheritable one does not. *)
type _ Effect.t += Suspend : unit Effect.t

let test_fiber () =
  print_endline "\n# Fibers";
  let inh = Dynamic.make ~inherit_:true and non = Dynamic.make ~inherit_:false in
  let k : (unit, unit) Effect.Deep.continuation option ref = ref None in
  let f () =
    Printf.printf "  first run         [expect 4/4]:    %s\n" (show inh non);
    Effect.perform Suspend;
    Printf.printf "  resumed elsewhere [expect 4/null]: %s\n" (show inh non)
  in
  Dynamic.with_temporarily inh 4 ~f:(fun () ->
    Dynamic.with_temporarily non 4 ~f:(fun () ->
      Effect.Deep.match_with f ()
        { retc = (fun () -> ());
          exnc = (fun e -> raise e);
          effc =
            (fun (type a) (e : a Effect.t) ->
              match e with
              | Suspend ->
                Some (fun (c : (a, unit) Effect.Deep.continuation) ->
                  k := Some c)
              | _ -> None) }));
  (* Both bindings are now unwound; resume the fiber in this context. *)
  match !k with Some c -> Effect.Deep.continue c () | None -> ()

let () =
  test_is_inherit ();
  test_domain ();
  test_thread ();
  test_thread_snapshot ();
  test_c_thread ();
  test_fiber ()
