(* [Bundle_share.Make] is generative: each application has its own counter. *)

module R1 = Bundle_share.Make (P_int) ()
module R2 = Bundle_share.Make (P_int) ()

let () =
  assert (R1.Basic_share.get_count () = 0);
  assert (R2.Basic_share.get_count () = 0);
  ignore (R1.Basic_share.create 1);
  ignore (R1.Basic_share.create 2);
  assert (R1.Basic_share.get_count () = 2);
  assert (R2.Basic_share.get_count () = 0);
  ignore (R2.Basic_share.create 10);
  assert (R2.Basic_share.get_count () = 1);
  assert (R1.Basic_share.get_count () = 2);
  print_endline "OK"
