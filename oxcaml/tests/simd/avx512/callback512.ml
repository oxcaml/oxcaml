open Stdlib

external run_callback : (unit -> unit) -> unit = "" "vec512_run_callback"

external run_callback_stack_args :
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  (int -> int -> int -> int -> int -> int -> int -> int -> unit) ->
  unit = "" "vec512_run_callback_stack_args"

external int64x8_of_int64s :
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64x8 = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external int64x8_w0 : int64x8 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]

external int64x8_w1 : int64x8 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]

external int64x8_w2 : int64x8 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]

external int64x8_w3 : int64x8 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]

external int64x8_w4 : int64x8 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]

external int64x8_w5 : int64x8 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]

external int64x8_w6 : int64x8 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]

external int64x8_w7 : int64x8 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

external lots_of_vectors :
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 ->
  int64x8 = "" "lots_of_vectors512"
[@@noalloc] [@@unboxed]

let eq l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v a b c d e f g h =
  eq (int64x8_w0 v) a;
  eq (int64x8_w1 v) b;
  eq (int64x8_w2 v) c;
  eq (int64x8_w3 v) d;
  eq (int64x8_w4 v) e;
  eq (int64x8_w5 v) f;
  eq (int64x8_w6 v) g;
  eq (int64x8_w7 v) h

let mk n =
  int64x8_of_int64s n (Int64.add n 1L) (Int64.add n 2L) (Int64.add n 3L)
    (Int64.add n 4L) (Int64.add n 5L) (Int64.add n 6L) (Int64.add n 7L)

let callback () =
  let v0 = mk 1L in
  let v1 = mk 9L in
  let v2 = mk 17L in
  let v3 = mk 25L in
  let v4 = mk 33L in
  let v5 = mk 41L in
  let v6 = mk 49L in
  let v7 = mk 57L in
  let v8 = mk 65L in
  let v9 = mk 73L in
  let v10 = mk 81L in
  let v11 = mk 89L in
  let v12 = mk 97L in
  let v13 = mk 105L in
  let v14 = mk 113L in
  let v15 = mk 121L in
  let sum =
    lots_of_vectors v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  in
  check sum 976L 992L 1008L 1024L 1040L 1056L 1072L 1088L

let callback_n i0 i1 i2 i3 i4 i5 i6 i7 =
  assert (
    i0 = 0 && i1 = 1 && i2 = 2 && i3 = 3 && i4 = 4 && i5 = 5 && i6 = 6 && i7 = 7);
  callback ()

(* Previously failing tests *)

let () = run_callback callback

let () = run_callback_stack_args 0 1 2 3 4 5 6 7 callback_n

let () =
  let[@inline never] finalizer () =
    let x = ref () in
    Gc.finalise (fun _ -> callback ()) x
  in
  finalizer ();
  Gc.full_major ()

let () = Sys.with_async_exns callback

(* Additional checks *)

let () = callback ()

let () =
  try Sys.with_async_exns (fun () -> raise Sys.Break) with
  | Sys.Break -> callback ()
  | _ -> assert false

let[@loop never] rec stack_overflow () = stack_overflow () [@nontail]

let () =
  try Sys.with_async_exns stack_overflow with
  | Stack_overflow -> callback ()
  | _ -> assert false

(* Effects *)

module Effect = Stdlib__Effect

type _ Effect.t += E : unit Effect.t

let eff0 () =
  try
    Effect.Deep.try_with
      (fun () -> Effect.perform E)
      ()
      { effc = (fun (type a) (_ : a Effect.t) -> None) }
  with Effect.Unhandled E -> callback ()

let eff1 () =
  Effect.Deep.try_with
    (fun () -> callback ())
    ()
    { effc = (fun (type a) (_ : a Effect.t) -> None) };
  callback ()

let eff2 () =
  Effect.Deep.try_with
    (fun () -> Effect.perform E)
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E ->
            callback ();
            Some (fun (_ : (a, unit) Effect.Deep.continuation) -> callback ())
          | _ -> None)
    };
  callback ()

let eff3 () =
  Effect.Deep.try_with
    (fun () ->
      callback ();
      Effect.perform E;
      callback ())
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E ->
            Some
              (fun (k : (a, unit) Effect.Deep.continuation) ->
                callback ();
                Effect.Deep.continue k ())
          | _ -> None)
    };
  callback ()

let eff4 () =
  Effect.Deep.match_with
    (fun () -> ())
    ()
    { retc = (fun () -> callback ());
      exnc = (fun _ -> ());
      effc = (fun (type a) (_ : a Effect.t) -> None)
    };
  callback ()

let eff5 () =
  Effect.Deep.match_with
    (fun () -> assert false)
    ()
    { retc = (fun () -> assert false);
      exnc = (fun _ -> callback ());
      effc = (fun (type a) (_ : a Effect.t) -> None)
    };
  callback ()

type _ Effect.t += E2 : unit Effect.t

let eff6 () =
  try
    Effect.Deep.match_with
      (fun () ->
        callback ();
        Effect.Deep.try_with
          (fun () ->
            callback ();
            Effect.perform E2;
            assert false)
          ()
          { effc =
              (fun (type a) (_ : a Effect.t) ->
                callback ();
                Effect.perform E;
                callback ();
                None)
          })
      ()
      { retc = (fun () -> assert false);
        exnc =
          (function
          | Effect.Unhandled E2 ->
            callback ();
            Effect.perform E
          | _ -> assert false);
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | E ->
              Some
                (fun (k : (a, unit) Effect.Deep.continuation) ->
                  callback ();
                  Effect.Deep.continue k ())
            | _ -> None)
      }
  with Effect.Unhandled E ->
    callback ();
    callback ()

let () =
  eff0 ();
  eff1 ();
  eff2 ();
  eff3 ();
  eff4 ();
  eff5 ();
  eff6 ();
  run_callback eff0;
  run_callback eff1;
  run_callback eff2;
  run_callback eff3;
  run_callback eff4;
  run_callback eff5;
  run_callback eff6
