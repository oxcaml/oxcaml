(* The float32 store clobbers xmm15, but not ymm15 when AVX is disabled. *)

let a = Bigarray.(Array1.create float32 c_layout 1)

let[@inline never] f x =
  a.{0} <- x;
  x +. 1.

let () =
  let y = f 2.5 in
  Printf.printf "%g %g\n" y a.{0}
