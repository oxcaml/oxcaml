(* TEST
   { bytecode; }
   { native; }
*)

module Dynamic = struct
  include Dynamic

  (* Expose the implementation of dynamic for testing. *)
  external push : 'a t -> 'a @ contended portable -> unit = "caml_dynamic_push"
  external pop : 'a t -> unit = "caml_dynamic_pop"
  external hash : 'a t -> int = "%identity"
end

let str_or_null = function This s -> s | Null -> "null"
let int_or_null = function This n -> string_of_int n | Null -> "null"
let get_str d = str_or_null (Dynamic.get d)
let get_int d = int_or_null (Dynamic.get d)

(* Run [f] on a brand new fiber, hence with a freshly-initialized (empty)
   binding table. *)
let in_fresh_fiber (f : unit -> unit) =
  Effect.Deep.match_with f ()
    { retc = (fun () -> ());
      exnc = (fun e -> raise e);
      effc = (fun (type a) (_ : a Effect.t) -> None) }

(* Deeply nest [with_temporarily] on a single variable. Each level pushes onto
   the *same* binding stack, forcing it to grow (init capacity 4, doubling).
   We record the value seen on the way down and on the way back up; the latter
   exercises repeated pop and the eventual free of the emptied stack. *)
let test_stack_growth () =
  print_endline "# stack growth: deep nesting on a single variable";
  let d = Dynamic.make () in
  let depth = 20 in
  let down = Buffer.create 64 and up = Buffer.create 64 in
  let rec go i =
    if i <= depth then
      Dynamic.with_temporarily d i ~f:(fun () ->
        Buffer.add_string down (get_int d ^ " ");
        go (i + 1);
        Buffer.add_string up (get_int d ^ " "))
  in
  go 1;
  Printf.printf "down: %s\n" (String.trim (Buffer.contents down));
  Printf.printf "up:   %s\n" (String.trim (Buffer.contents up));
  Printf.printf "after all pops [expect null]: %s\n" (get_int d)

(* Bind many distinct variables simultaneously (nested), forcing the hash table
   to grow repeatedly (init capacity 8, doubling at half-full) and to rehash
   every key on each grow. Then read them all back, exercising find+probing. *)
let test_table_growth () =
  print_endline "\n# table growth: many simultaneous distinct variables";
  let n = 25 in
  let ds = Array.init n (fun _ -> Dynamic.make ()) in
  let rec bind i =
    if i = n then begin
      let visible = ref 0 and correct = ref true in
      Array.iteri
        (fun j (d : int Dynamic.t) ->
          match Dynamic.get d with
          | This v ->
            incr visible;
            if v <> j then correct := false
          | Null -> ())
        ds;
      Printf.printf "bound %d, visible %d, all correct: %b\n" n !visible
        !correct
    end
    else Dynamic.with_temporarily ds.(i) i ~f:(fun () -> bind (i + 1))
  in
  bind 0;
  let leftover =
    Array.fold_left
      (fun acc d ->
        acc + match Dynamic.get d with This _ -> 1 | Null -> 0)
      0 ds
  in
  Printf.printf "visible after unwind [expect 0]: %d\n" leftover

(* The interesting hash-table case: two keys that collide. The second is placed
   by linear probing at [slot+1]. Removing the first must rehash the probe
   chain so the second remains reachable from its home slot. *)
let test_collision () =
  print_endline "\n# hash collision: linear probing and rehash on pop";
  in_fresh_fiber (fun () ->
    (* Fresh fiber => fresh table. The first push grows it to capacity 8, and it
       stays at 8 while at most 4 keys are bound, so the table slot of a key is
       [hash land 7]. Find two keys sharing the same NON-zero slot. *)
    let slot d = Dynamic.hash d land 7 in
    let pool = Array.init 128 (fun _ -> Dynamic.make ()) in
    let a, b =
      let found = ref None in
      Array.iter
        (fun x ->
          if Option.is_none !found && slot x <> 0 then
            Array.iter
              (fun y ->
                if Option.is_none !found && (not (x == y)) && slot x = slot y
                then found := Some (x, y))
              pool)
        pool;
      match !found with
      | Some p -> p
      | None -> failwith "no suitable collision found"
    in
    Dynamic.push a 111;
    Dynamic.push b 222;
    (* b was placed at a's slot + 1 by linear probing. *)
    Printf.printf "both bound: a=%s b=%s\n" (get_int a) (get_int b);
    Dynamic.pop a;
    (* Reading the removed key first also evicts the (shared) per-thread cache
       slot, so the read of b below genuinely consults the hash table. *)
    Printf.printf "after pop a: a=%s [expect null]\n" (get_int a);
    Printf.printf "after pop a: b=%s [expect 222]\n" (get_int b);
    Dynamic.pop b)

(* Heap-allocated bindings must be scanned as GC roots. We bust the per-thread
   cache so the live strings are reachable only through the binding stack in the
   hash table, then move the heap underneath them and read them back. The final
   fiber leaves bindings live at exit, exercising table teardown. *)
let test_gc () =
  print_endline "\n# GC root scanning of live bindings";
  let flush_gc () =
    for _ = 1 to 64 do
      let _ = Dynamic.get (Dynamic.make ()) in
      ()
    done;
    Gc.full_major ();
    Gc.compact ()
  in
  let d = Dynamic.make () in
  Dynamic.push d (Bytes.unsafe_to_string (Bytes.make 4 'x'));
  Dynamic.push d (Bytes.unsafe_to_string (Bytes.make 6 'y'));
  flush_gc ();
  Printf.printf "top after GC [expect yyyyyy]: %s\n" (get_str d);
  Dynamic.pop d;
  flush_gc ();
  Printf.printf "below after GC [expect xxxx]: %s\n" (get_str d);
  Dynamic.pop d;
  in_fresh_fiber (fun () ->
    let e = Dynamic.make () and g = Dynamic.make () in
    Dynamic.push e (Bytes.unsafe_to_string (Bytes.make 3 'p'));
    Dynamic.push g (Bytes.unsafe_to_string (Bytes.make 3 'q'));
    flush_gc ();
    Printf.printf "live in fiber: e=%s g=%s\n" (get_str e) (get_str g))

let () =
  test_stack_growth ();
  test_table_growth ();
  test_collision ();
  test_gc ()
