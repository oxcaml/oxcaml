(* TEST
 ocamlopt_flags = "-S -g -no-frametables-in-rodata";
 native;
*)

(* Exercises runtime features that depend on the GC frametable, with
   [-no-frametables-in-rodata] active.  The frametable is consulted by the
   runtime to:
     - locate live stack roots during GC;
     - resolve return addresses to source locations for backtraces;
     - dispatch user finalisation callbacks during a collection. *)

let[@cold] f x = x +. 1.

let () = assert (f 27. = 28.)

(* -- GC root scanning ---------------------------------------------------- *)

(* Each frame keeps [pair] live across an explicit minor collection, the
   recursive call (which itself allocates and may collect), and an
   occasional major collection.  If the frametable describes the layout
   incorrectly, [pair] will be corrupted by GC and [check_pair] will see
   stale data. *)

let[@inline never] check_pair n (a, b) =
  assert (a = n);
  assert (b = n * 2)

let rec gc_roots_recursive n =
  if n = 0
  then []
  else
    let pair = n, n * 2 in
    Gc.minor ();
    let rest = gc_roots_recursive (n - 1) in
    if n mod 10 = 0 then Gc.full_major ();
    check_pair n pair;
    pair :: rest

let () =
  let pairs = gc_roots_recursive 200 in
  assert (List.length pairs = 200);
  List.iteri
    (fun i (a, b) ->
      let expected = 200 - i in
      assert (a = expected);
      assert (b = expected * 2))
    pairs

(* -- Exception handling -------------------------------------------------- *)

exception Test_exn of int

let rec deep_raise n =
  if n = 0 then raise (Test_exn 42) else 1 + deep_raise (n - 1)

let () =
  match deep_raise 200 with
  | _ -> assert false
  | exception Test_exn n -> assert (n = 42)

(* Catch and re-raise an exception at every level of the stack, allocating
   in between to interleave the exception machinery with the GC. *)

let rec rethrow n =
  if n = 0
  then raise (Test_exn 0)
  else
    try rethrow (n - 1) with
    | Test_exn k ->
      Gc.minor ();
      raise (Test_exn (k + 1))

let () =
  match rethrow 50 with
  | _ -> assert false
  | exception Test_exn n -> assert (n = 50)

(* -- Backtraces ---------------------------------------------------------- *)

let () = Printexc.record_backtrace true

let contains_substring s sub =
  let n = String.length s and m = String.length sub in
  let rec loop i =
    if i + m > n
    then false
    else if String.equal sub (String.sub s i m)
    then true
    else loop (i + 1)
  in
  m = 0 || loop 0

let rec frame_with_backtrace n =
  if n = 0 then raise (Test_exn 0) else 1 + frame_with_backtrace (n - 1)

let () =
  match frame_with_backtrace 20 with
  | _ -> assert false
  | exception Test_exn _ ->
    let bt = Printexc.get_backtrace () in
    (* With [-g], frametable debuginfo should resolve return addresses back
       to this source file. *)
    assert (contains_substring bt "frametables_in_rodata.ml")

(* -- Finalizers ---------------------------------------------------------- *)

(* Finalisation callbacks execute OCaml code from inside the GC; the runtime
   must walk and resume each stack via its frametable. *)

let finalizer_runs = ref 0

let[@inline never] register_finalizer () =
  let buf = Bytes.create 16 in
  Gc.finalise (fun _ -> incr finalizer_runs) buf

let () =
  for _ = 1 to 200 do
    register_finalizer ()
  done;
  Gc.full_major ();
  Gc.full_major ();
  assert (!finalizer_runs = 200)
