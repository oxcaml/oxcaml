(* TEST *)

let[@tail_mod_cons] rec test_1 = function
  | [] -> []
  | hd :: tl ->
     try int_of_string hd :: (test_1[@tailcall false]) tl
     with Failure _ -> -1 :: (test_1[@tailcall true]) tl

let () =
  assert (test_1 ["1"; "2"; "foo"; "3"; "4"] = [1; 2; -1; 3; 4])

(* The body of a [try] is not in tail position, so the TMC call in it must not
   be annotated as a tailcall. *)
let[@tail_mod_cons] rec count_down n =
  try
    if n = 0 then raise Exit;
    n :: count_down (n - 1)
  with Exit -> []

let () = assert (count_down 3 = [3; 2; 1])
