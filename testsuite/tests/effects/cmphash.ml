(* TEST
   runtime5;
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Deep

type _ t += E : unit t

let () =
  try_with perform E
    { effc =
        (fun (type a) (e : a t) ->
          match e with
          | E ->
            Some
              (fun k ->
                (match k = k with
                | _ -> assert false
                | exception Invalid_argument _ -> print_endline "ok");
                match Hashtbl.hash k with _ -> print_endline "ok")
          | e -> None)
    }
