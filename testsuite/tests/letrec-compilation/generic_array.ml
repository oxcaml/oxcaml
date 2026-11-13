(* TEST *)

let rec x = let _y = [| |] in ();;

type 'a l = { hd : 'a; tl : unit -> 'a l }

let rec x = let y = [| |] in { hd = y; tl = fun () -> x };;
