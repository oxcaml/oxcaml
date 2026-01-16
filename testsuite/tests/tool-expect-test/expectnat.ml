(* TEST
 expect.opt;
*)

(* Demonstrate that the native backend is used *)
let x : int = Obj.magic Obj.magic #5s
[%%expect {|
val x : int = 2
|}]
