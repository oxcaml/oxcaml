(* Member 1 of the batch: forces Stdlib's cmx (and its zero_alloc info) into
   the caches shared across the batch. *)
let g () = print_int 0
