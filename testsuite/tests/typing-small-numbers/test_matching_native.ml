(* TEST
 flambda2;
 {
   toplevel.opt;
 }
*)

(* untagged char *)

let zero =
  match #'c' with
  | _ -> 0
;;

let one =
  match #'c' with
  | #'c' -> 1
  | _ -> 2
;;

let three_redundant =
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

let fifty_partial_overlap =
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

let two_hundred_full_overlap =
  match #'m' with
  | #'a'..#'z' -> 200
  | #'f'..#'t' -> 300
  | _ -> 400
;;

let five_hundred_partial =
  match #'m' with
  | #'a'..#'z' -> 500
;;

let seven_hundred =
  match #'\x99' with
  | #'\x00'..#'\x80' -> 600
  | #'\x81'..#'\xff' -> 700
;;

let nine_hundred_redundant_wildcard =
  match #'\x99' with
  | #'\x00'..#'\x80' -> 800
  | #'\x81'..#'\xff' -> 900
  | _ -> 1000
;;

(* int8 *)

let zero =
  match 3s with
  | _ -> 0
;;

let one =
  match 3s with
  | 3s -> 1
  | _ -> 2
;;

let three_redundant =
  match 3s with
  | 3s -> 3
  | 3s -> 4
  | _ -> 5

let thirty =
  match 10s with
  | 0s -> 10
  | 5s -> 20
  | 10s -> 30
  | _ -> 40
;;

let forty =
  match 11s with
  | 0s -> 10
  | 5s -> 20
  | 10s -> 30
  | _ -> 40
;;

let five_hundred_partial =
  match 6s with
  | 0s | 1s | 2s | 3s | 4s | 5s | 6s | 7s | 8s | 9s | 10s -> 500
;;

(* untagged int8 *)

let zero =
  match #3s with
  | _ -> 0
;;

let one =
  match #3s with
  | #3s -> 1
  | _ -> 2
;;

let three_redundant =
  match #3s with
  | #3s -> 3
  | #3s -> 4
  | _ -> 5

let thirty =
  match #10s with
  | #0s -> 10
  | #5s -> 20
  | #10s -> 30
  | _ -> 40
;;

let forty =
  match #11s with
  | #0s -> 10
  | #5s -> 20
  | #10s -> 30
  | _ -> 40
;;

let five_hundred_partial =
  match #6s with
  | #0s | #1s | #2s | #3s | #4s | #5s | #6s | #7s | #8s | #9s | #10s -> 500
;;

(* int16 *)

let zero =
  match 3S with
  | _ -> 0
;;

let one =
  match 3S with
  | 3S -> 1
  | _ -> 2
;;

let three_redundant =
  match 3S with
  | 3S -> 3
  | 3S -> 4
  | _ -> 5

let thirty =
  match 10S with
  | 0S -> 10
  | 5S -> 20
  | 10S -> 30
  | _ -> 40
;;

let forty =
  match 11S with
  | 0S -> 10
  | 5S -> 20
  | 10S -> 30
  | _ -> 40
;;

let five_hundred_partial =
  match 6S with
  | 0S | 1S | 2S | 3S | 4S | 5S | 6S | 7S | 8S | 9S | 10S -> 500
;;

(* untagged int16 *)

let zero =
  match #3S with
  | _ -> 0
;;

let one =
  match #3S with
  | #3S -> 1
  | _ -> 2
;;

let three_redundant =
  match #3S with
  | #3S -> 3
  | #3S -> 4
  | _ -> 5

let thirty =
  match #10S with
  | #0S -> 10
  | #5S -> 20
  | #10S -> 30
  | _ -> 40
;;

let forty =
  match #11S with
  | #0S -> 10
  | #5S -> 20
  | #10S -> 30
  | _ -> 40
;;

let five_hundred_partial =
  match #6S with
  | #0S | #1S | #2S | #3S | #4S | #5S | #6S | #7S | #8S | #9S | #10S -> 500
;;
