let use_static (_ @ static) = ()

(* [x] is bound by [-open-cmi slib/s.cmi] or [-open-cmi-x slib/s.cmi] on the
   command line.  Only the latter guarantees a cmx, so only it keeps [S] static.
   (As in use_open.ml, the error for the former does not name [S].) *)
let () = use_static x
