(* TEST
 multicore;
 {
   bytecode;
 }{
   native;
 }
*)

external thread_self_id : unit -> int = "%thread_self_id"

module Int_set = Set.Make (Int)

let () =
  Printf.printf "main domain: %d\n" (thread_self_id ());
  let ids =
    thread_self_id ()
    :: List.map Domain.join (List.init 4 (fun _ -> Domain.spawn thread_self_id))
  in
  Printf.printf "ids are unique: %b\n"
    (Int_set.cardinal (Int_set.of_list ids) = List.length ids)
