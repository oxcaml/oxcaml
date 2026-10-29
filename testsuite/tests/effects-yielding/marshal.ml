(* TEST
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Deep
open Effect.Deep.Yielding

type _ t += E : unit t

let () =
  try_with (fun h eff -> Effect.Yielding.perform h eff [@nontail]) E
  { effc = fun (type a) (e : a t) ->
      Some (fun k ->
          match Marshal.to_string k [] with
          | _ -> assert false
          | exception (Invalid_argument _) -> print_endline "ok"
          ) }
