(* TEST
   flambda2;
   native;
*)

(* TEST
   flambda2;
   javascript;
*)

(* Bug found through generated test, kept for regression testing. *)

type t20 = #{ a20 : int; b20 : int }
type t21 = { a21 : int; mutable b21 : t20 }

let () =
  let r = { a21 = 0; b21 = #{ a20 = 1; b20 = 2 } } in
  r.b21 <- #{ a20 = 101; b20 = 102 };
  ()
;;
