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

let three =
  match #'c' with
  | #'c' -> 3
  | #'c' -> 4
  | _ -> 5

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

let fifty =
  match #'m' with
  | #'a'..#'t' -> 50
  | #'f'..#'z' -> 60
  | _ -> 70
;;

let eighty =
  match #'m' with
  | #'t'..#'a' -> 80
  | _ -> 90
;;

let one_hundred =
  match #'m' with
  | #'m'..#'m' -> 100
  | _ -> 110
;;

let two_hundred =
  match #'m' with
  | #'a'..#'z' -> 200
  | #'f'..#'t' -> 300
  | _ -> 400
;;

let five_hundred =
  match #'m' with
  | #'a'..#'z' -> 500
;;

let seven_hundred =
  match #'\x99' with
  | #'\x00'..#'\x80' -> 600
  | #'\x81'..#'\xff' -> 700
;;

let nine_hundred =
  match #'\x99' with
  | #'\x00'..#'\x80' -> 800
  | #'\x81'..#'\xff' -> 900
  | _ -> 1000
;;

(* tagged i8s *)

(* untagged i8s *)
