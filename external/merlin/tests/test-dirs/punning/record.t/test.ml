type t =
  { a : string
  ; b : int
  }

(* Punned record field in an expression *)

let _ =
  let a = "hello" in
  let b = 42 in
  { a; b }

(* Punned record field in a pattern *)

let f { a; b } =
  ignore a;
  ignore b
