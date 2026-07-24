module R = Bundle2.Make(P_int)()

let () =
  (* Type sharing: Basic_transparent and Util_transparent have transparent
     type t = P.t.  After instantiation with P_int,
     R.Basic_transparent.t = R.Util_transparent.t = P_int.t = int.  This
     means a Util_transparent.t value can be used directly as a
     Basic_transparent.t value. *)
  let u : R.Util_transparent.t = R.Util_transparent.create () in
  let b : R.Basic_transparent.t = u in
  print_endline (R.Basic_transparent.to_string b)

module R1 = Bundle2.Make(P_int)()
module R2 = Bundle2.Make(P_int)()

let () =
  (* With transparent types, two bundle applications give the same type
     (both = P_int.t = int), so values can be mixed freely. *)
  let v : R1.Basic_transparent.t = 42 in
  let _ : R2.Basic_transparent.t = v in
  ()
