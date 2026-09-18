let use_static (_ @ static) = ()

(* Using the value [S.x] at [@ static] requires [S] itself to be static.

   CR-soon zqian: the error below is a bare "this value is dynamic" that drops
   the [Cmx_not_guaranteed] hint, so it neither names [S] nor explains why it is
   dynamic. This should be fixed to carry the hint through value uses. *)
let () = use_static S.x
