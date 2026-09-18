(* TEST *)

(* Tests that an exception in the alloc_major callback propagates
   correctly to the top level. *)

module MP = Gc.Memprof

let alloc_major_tracker on_alloc =
  { MP.null_tracker with
    alloc_major = (fun info -> on_alloc info; None);
  }

(* Run without exception, as the null test *)

let () =
  ignore (MP.start ~callstack_size:10 ~sampling_rate:1.
                   (alloc_major_tracker (fun _ -> ())));
  ignore (Sys.opaque_identity (Array.make 500 0));
  MP.stop();
  print_endline "Run without exception."


(* Run with an exception *)

let _ =
try
 Sys.with_async_exns (fun () ->
  let _:MP.t = MP.start ~callstack_size:10 ~sampling_rate:1.
                   (alloc_major_tracker
                     (fun _ -> raise Sys.Break)) in
   (ignore (Sys.opaque_identity (Array.make 500 0));
    MP.stop ())
 )
with
  Sys.Break -> (MP.stop();
                Printf.printf "Exception from major allocation callback.\n")

(* The upstream test uses a user-defined exception which carries a string "major
   allocation callback" to the printf call. We can't do that here as we only allow the
   async callbacks, so we fake the string to keep the reference output files identical. *)
