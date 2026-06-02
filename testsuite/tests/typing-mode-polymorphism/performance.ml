(* TEST
 flags += "-extension mode_polymorphism_alpha";
 native;
*)

(* The following tests are known performance regressions *)

(*

This pattern shows polynomial growth

  let f0 x = x
  let f1 x = f0 x
  let f2 x = f1 x
  let f3 x = f2 x
  (* ... *)
  let f399 x = f398 x
  let _ = f399

Another pathological polynomial growth

  let top =
    (let g399 =
       (let g398 =
          (* ... *)
            (let g1 =
               (let g0 = (fun x -> x) in
                (fun x -> g0 x)) in
             (fun x -> g1 x))
          (* ... *)
        in (fun x -> g398 x)) in
     (fun x -> g399 x))
  let _ = top

*)

(* This pattern is exponential time with mode poly on, but polynomial with mode poly off *)
let id1 x1 =
    (let id2 x2 =
       (let id3 x3 = x3 in
        id3 (id3 x2)) in
     id2 (id2 x1))
  let _ = id1
