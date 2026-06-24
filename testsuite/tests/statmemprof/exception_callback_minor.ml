(* TEST *)

(* Tests that an exception in the alloc_minor callback propagates
   correctly to the top level. *)

module MP = Gc.Memprof

let _ =

try
 Sys.with_async_exns (fun () ->
   let _:MP.t = MP.start ~callstack_size:10 ~sampling_rate:1.
                  { MP.null_tracker with
                      alloc_minor =
                        fun _ -> raise Sys.Break } in
     (ignore (Sys.opaque_identity (ref (ref 42)));
      MP.stop ())
 )
with
  Sys.Break -> (MP.stop();
                Printf.printf "Exception from alloc_minor callback.\n")

(* The upstream test uses a user-defined exception which carries a string "alloc_minor
   callback" to the printf call. We can't do that here as we only allow the async
   callbacks, so we fake the string to keep the reference output files identical. *)
