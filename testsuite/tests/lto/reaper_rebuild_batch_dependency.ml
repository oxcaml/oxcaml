(* An independent second member for reaper_rebuild_batch.ml, and the imported
   unit whose position in the batch reaper_rebuild_batch_bad_order.ml gets
   wrong. *)
let[@inline never] used x = x + 1

let unused x = x * 2
