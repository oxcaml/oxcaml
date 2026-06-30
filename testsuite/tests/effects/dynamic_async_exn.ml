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

(* Dynamic.t isn't yet implemented in the OxCaml stdlib, only the runtime
   support for it, so testing requires faking the infrastructure here. *)

module Dynamic : sig
  type 'a t

  val make : unit -> 'a t
  val get : 'a t -> 'a or_null

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

  external make : unit -> 'a t = "caml_dynamic_make"
  external get : 'a t -> 'a or_null = "caml_dynamic_get"

  let with_temporarily d v ~f =
    let effc eff k last_fiber = reperform eff k last_fiber in
    with_stack_bind (fun x -> x) (fun e -> raise e) effc d v f ()
end

let () = Sys.catch_break true

let[@inline never] allocate_bytes finished =
  let b = Bytes.create 42 in
  Gc.finalise_last (fun () ->
      finished := true;
      raise Sys.Break)
    b;
  ref (Some b)

let print_null = function
  | This x -> Int.to_string x
  | Null -> "null"

let print_dyn d = print_null (Dynamic.get d)

let child = 100

let () =
  let d = Dynamic.make () in
  let finished = ref false in
  let r = allocate_bytes finished in
  (try
    Sys.with_async_exns (fun () ->
      r := None;
      (* Bind [d] to [child] in a fiber and read it so the cache holds
         [d -> child], then allocate until the finaliser raises [Sys.Break]
         asynchronously, unwinding out of this fiber. *)
      Dynamic.with_temporarily d child ~f:(fun () ->
        Printf.printf "in fiber [expect %d]: %s\n%!" child (print_dyn d);
        while true do
          let _ @ global = Sys.opaque_identity (42, Random.int 42) in
          ()
        done))
  with
  | Sys.Break -> assert !finished
  | _ -> assert false);
  (* The bound fiber is gone; [d] must read as [root] again. *)
  Printf.printf "after async unwind [expect null]: %s\n%!" (print_dyn d)
