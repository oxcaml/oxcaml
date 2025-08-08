(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

(* Some concrete type parameter *)
(* CR generic-optional: Thoroughly test gadts *)
type (_, _) gadt =
  | None
  | Some : 'a -> (int, 'a) gadt
[@@option_like]
[%%expect {|
type (_, _) gadt = None | Some : 'a -> (int, 'a) gadt
|}]


let f (?(x = 4.2) : (int, float) gadt) () = x
[%%expect{|
val f : (?x):(int, float) gadt -> unit -> float = <fun>
|}]

(* CR generic-optional: This test case should work once we have semantic
   interpretations. *)
type (_, _) gadt =
  | A : ('a, 'b) gadt
  | B : 'b -> (int, 'b) gadt
[@@option_like]
[%%expect {|
type (_, _) gadt = A : ('a, 'b) gadt | B : 'b -> (int, 'b) gadt
|}]


let f (?(x = 4.2) : (int, float) gadt) () = x
[%%expect{|
val f : (?x):(int, float) gadt -> unit -> float = <fun>
|}]
