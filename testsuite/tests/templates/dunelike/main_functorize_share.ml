module R = Bundle2.Func(P_int)()

let () =
  (* Type sharing: Basic2 and Util2 have transparent type t = P.t.
     After instantiation with P_int, R.Basic2.t = R.Util2.t = P_int.t = int.
     This means a Util2.t value can be used directly as a Basic2.t value. *)
  let u : R.Util2.t = R.Util2.create () in
  let b : R.Basic2.t = u in
  print_endline (R.Basic2.to_string b)

module R1 = Bundle2.Func(P_int)()
module R2 = Bundle2.Func(P_int)()

let () =
  (* With transparent types, two bundle applications give the same type
     (both = P_int.t = int), so values can be mixed freely. *)
  let v : R1.Basic2.t = 42 in
  let _ : R2.Basic2.t = v in
  ()
