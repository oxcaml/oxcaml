let try_gcd x y =
  Gcd_data.x := x;
  Gcd_data.y := y;
  Gcd.gcd ();
  Format.printf "gcd(%d, %d) = %d\n" x y !Gcd_data.res

let () =
  try_gcd 216 250 (* 2 *);
  try_gcd 37 37 (* 37 *);
  try_gcd 987 1597 (* 1 *);
  try_gcd 1665 1035 (* 45 *);
  try_gcd 23 0 (* 23 *)
