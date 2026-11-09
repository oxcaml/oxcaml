(* TEST
 expect;
*)

let rec a = lazy b and b = 3;;
[%%expect{|
val a : int lazy_t = <lazy>
val b : int = 3
|}];;

let rec e = lazy (fun _ -> f) and f = ();;
[%%expect{|
val e : ('a -> unit) lazy_t = <lazy>
val f : unit = ()
|}];;

let rec x = lazy (Lazy.force x + Lazy.force x)
  ;;
[%%expect{|
val x : int Lazy.t = <lazy>
|}];;
