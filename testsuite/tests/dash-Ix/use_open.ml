open S

let use_static (_ @ static) = ()

(* The [@ static] use of [x] (bound by [open S] above) occurs far from the
   [open].

   CR-soon zqian: the error below is a bare "this value is dynamic" that drops
   the [Cmx_not_guaranteed] hint, so it does not name [S]. This should be fixed
   to carry the hint through value uses. *)
let () = use_static x
