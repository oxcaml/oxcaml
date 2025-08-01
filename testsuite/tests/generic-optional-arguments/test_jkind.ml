(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

(* Some concrete type parameter *)
type (_, _) gadt =
  | None
  | Some : 'a -> (int, 'a) gadt
[@@option_like]
[%%expect {|
type (_, _) gadt = None | Some : 'a -> (int, 'a) gadt
|}]


let f (?(x = 4.2) : (int, float) gadt) () = x
[%%expect{|
Line 1, characters 20-37:
1 | let f (?(x = 4.2) : (int, float) gadt) () = x
                        ^^^^^^^^^^^^^^^^^
Error: Unknown generic optional argument type: (int, float) gadt
|}]
