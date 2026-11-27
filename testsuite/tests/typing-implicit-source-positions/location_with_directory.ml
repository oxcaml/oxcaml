(* TEST
 flags = "-directory app/foo";
 {
   native;
 }{
   jsir;
 }
*)

let f = fun ~(call_pos:[%call_pos]) () -> call_pos
let _ = print_string (f ()).pos_fname;;
