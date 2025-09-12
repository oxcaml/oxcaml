(* TEST
 flambda2;
 {
   toplevel;
 }{
   toplevel.opt;
 }
*)

(* untagged chars *)

let zero =
  match #'c' with
  | _ -> 0
;;

let one =
  match #'c' with
  | #'c' -> 1
  | _ -> 2
;;

let thirty =
  match #'A' with
  | #'0' -> 10
  | #'c' -> 20
  | #'A' -> 30
  | _ -> 40
;;

let forty =
  match #'a' with
  | #'0' -> 10
  | #'c' -> 20
  | #'A' -> 30
  | _ -> 40
;;

(* tagged i8s *)

(* untagged i8s *)
