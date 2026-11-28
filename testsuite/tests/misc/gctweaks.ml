(* TEST
 ocamlrunparam += ",Xsmall_heap_limit=256k";
 {
   bytecode;
 }{
   native;
 }
*)

(* This test depends on the major GC running: it undoes the default
   OCAMLRUNPARAM of the top-level Makefile. *)

let () =
  (match Gc.Tweak.get "blorp" with
   | exception Invalid_argument _ -> ()
   | _ -> assert false);
  (match Gc.Tweak.set "blorp" 100 with
   | exception Invalid_argument _ -> ()
   | _ -> assert false);
  let def = Gc.Tweak.get "custom_work_max_multiplier" in
  Gc.Tweak.set "custom_work_max_multiplier" 100;
  Printf.printf "%d\n" (Gc.Tweak.get "custom_work_max_multiplier");
  (match Gc.Tweak.list_active () with
   | ["custom_work_max_multiplier", 100] -> ()
   | _ -> assert false);
  Gc.Tweak.set "custom_work_max_multiplier" def;
  assert (Gc.Tweak.list_active () = []);
  Printf.printf "ok\n";
  ()
