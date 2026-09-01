(* TEST
   { bytecode; }
   { native; }
*)

(* Regression test for the dynamic-binding cache not being flushed when an
   asynchronous exception unwinds across fibers.

   A child fiber binds [d] to a child value (via [with_temporarily], which uses
   the [%with_stack_bind] primitive) and reads it, so the per-thread direct
   mapped cache holds [d -> child].  An async exception (here [Sys.Break],
   raised from a finaliser) then fires inside that fiber and unwinds, via
   [caml_raise_async], back to an ancestor [Sys.with_async_exns] handler,
   freeing the bound fiber's stack.  After unwinding, [Dynamic.get d] must
   return the root value again; if the cache is not flushed it wrongly returns
   the now-out-of-scope child value. *)

let () = Sys.catch_break true

let[@inline never] allocate_bytes finished =
  let b = Bytes.create 42 in
  Gc.finalise_last (fun () ->
      finished := true;
      raise Sys.Break)
    b;
  ref (Some b)

let print_null = function
  | This x -> x
  | Null -> "null"

let print_dyn d = print_null (Dynamic.get d)

let () =
  let d = Dynamic.make () in
  let parent = (Bytes.unsafe_to_string (Bytes.make 4 'x')) in
  Dynamic.with_temporarily d parent ~f:(fun () -> exclave_
    let finished = ref false in
    let r = allocate_bytes finished in
    (try
      Sys.with_async_exns (fun () ->
        r := None;
        (* Bind [d] to [child] and read it so the cache holds
          [d -> child], then allocate until the finaliser raises [Sys.Break]
          asynchronously, unwinding out of this async exn handler. *)
        let child = (Bytes.unsafe_to_string (Bytes.make 4 'y')) in
        Dynamic.with_temporarily d child ~f:(fun () -> exclave_
          Printf.printf "in fiber [expect %s]: %s\n%!" child (print_dyn d);
          while true do
            let _ @ global = Sys.opaque_identity (42, Random.int 42) in
            ()
          done;
          ()) [@nontail])
    with
    | Sys.Break -> assert !finished
    | _ -> assert false);
    (* The bound fiber is gone; [d] must read as [parent] again. *)
    Printf.printf "after async unwind [expect %s]: %s\n%!" parent (print_dyn d))
