(* TEST
 expect.opt;
*)

(* Demonstrate that the native backend is used *)
let _ = Sys.backend_type

[%%expect{|
- : Sys.backend_type = Sys.Native
|}]
