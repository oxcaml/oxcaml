(* TEST *)

(* Tests that an exception in the alloc_minor callback propagates
   correctly to the top level. *)

<<<<<<< oxcaml
||||||| upstream-base
(* We don't want to print the backtrace. We just want to make sure the
   exception is printed.
   This also makes sure [Printexc] is loaded, otherwise we don't use
   its uncaught exception handler. *)
let _ = Printexc.record_backtrace false
=======
exception MyExc of string

>>>>>>> upstream-incoming
module MP = Gc.Memprof

let _ =

try
<<<<<<< oxcaml
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
                Printf.printf "Exception from memprof.\n")
||||||| upstream-base
  start ~callstack_size:10 ~sampling_rate:1.
    { null_tracker with
      alloc_minor = (fun _ -> assert false);
      alloc_major = (fun _ -> assert false);
    };
  ignore (Sys.opaque_identity (ref (ref 42)));
  stop ()
=======
   let _:MP.t = MP.start ~callstack_size:10 ~sampling_rate:1.
                  { MP.null_tracker with
                      alloc_minor =
                        fun _ -> raise (MyExc "alloc_minor callback") } in
     (ignore (Sys.opaque_identity (ref (ref 42)));
      MP.stop ())
with
  MyExc s -> (MP.stop();
              Printf.printf "Exception from %s.\n" s)
>>>>>>> upstream-incoming
