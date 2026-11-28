(* minimal BUG example from alanechang *)
let _ =
  (fun f ->
    [%probe
      ""
        (let a @ local = "abc" in
         f a;
         ())]) (fun x -> ())
