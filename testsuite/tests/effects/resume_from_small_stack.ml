(* TEST
 { bytecode; }
 { native; }
*)

(* Resuming a continuation that is in the major heap, while the major GC is
   marking and has not reached it yet, makes the runtime scan the stacks of
   the continuation before it switches to them. That scan needs far more
   stack than a fiber is guaranteed to have left when stack checks are
   enabled, so it must not run on the stack of the fiber that resumes. On
   arm64 it used to, and overwrote the memory below that stack.

   Each continuation below is in that state when it is resumed. It is resumed
   from a new fiber, whose stack starts small, under a number of frames that
   goes from 0 to 31, so that some of these fibers are as close to the limit
   of their stack as a fiber can get. *)

open Effect
open Effect.Deep

type _ Effect.t += Suspend : unit Effect.t

let count = 32

let suspended : (unit, int) continuation option array = Array.make count None

let suspend i =
  match_with
    (fun () -> perform Suspend; i)
    ()
    { retc = Fun.id;
      exnc = raise;
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Suspend ->
            Some
              (fun (k : (a, int) continuation) ->
                suspended.(i) <- Some k;
                -1)
          | _ -> None)
    }

let[@inline never] rec under_frames n f =
  if n = 0 then f () else 1 + under_frames (n - 1) f

let resume i ~frames =
  match suspended.(i) with
  | None -> failwith "resumed twice"
  | Some k ->
    suspended.(i) <- None;
    match_with
      (fun () -> under_frames frames (fun () -> continue k ()))
      ()
      { retc = Fun.id; exnc = raise; effc = (fun _ -> None) }

let () =
  for i = 0 to count - 1 do
    let r = suspend i in
    if r <> -1 then failwith (Printf.sprintf "suspend %d returned %d" i r)
  done;
  (* This moves the continuations to the major heap and starts a major cycle,
     which has not marked them. *)
  Gc.full_major ();
  (* The slice has the largest budget, to go through the sweeping and idle
     phases of that cycle whatever their length. It requests the marking
     phase, which the minor collection starts. Marking has then begun, from
     the roots, and has not reached the suspended continuations. *)
  ignore (Gc.major_slice max_int : int);
  Gc.minor ();
  for i = 0 to count - 1 do
    let r = resume i ~frames:i in
    if r <> 2 * i then failwith (Printf.sprintf "resume %d returned %d" i r)
  done;
  print_endline "ok"
