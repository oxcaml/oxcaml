(* TEST
   flags = "-rectypes";
   expect;
*)

type 'a t constraint 'a = 'a * 'a
[%%expect{|
type 'a t constraint 'a = 'a * 'a
|}]

type 'a u = int constraint 'a = 'a * 'a
[%%expect{|
type 'a u = int constraint 'a = 'a * 'a
|}]
