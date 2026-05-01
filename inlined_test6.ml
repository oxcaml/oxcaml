(* Use a more complex parameter usage to keep `a` as a real variable *)
let[@inline always] inner a =
  (* Use a in a way that's harder to substitute *)
  Sys.opaque_identity a + Sys.opaque_identity a + 1

let[@inline never] caller x =
  let result = inner (Sys.opaque_identity x) in
  result + 1

let () =
  let r = caller 41 in
  (Printf.printf [@inlined never]) "%d\n" r
