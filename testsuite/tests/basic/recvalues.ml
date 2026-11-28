(* TEST *)

(* Recursive value definitions *)

type 'a mlist = Nil | Cons of { hd : 'a; mutable tl : 'a mlist }

let _ =
  let rec x = Cons { hd = 1; tl = x } in
  if match x with
       Cons { hd = 1; tl = x' } -> x == x'
     | _ -> false
  then print_string "Test 1: passed\n"
  else print_string "Test 1: FAILED\n";
  let one = 1 in
  let rec y = Cons { hd = (one, one+1); tl = y } in
  if match y with
       Cons { hd = (1,2); tl = y' } -> y == y'
     | _ -> false
  then print_string "Test 2: passed\n"
  else print_string "Test 2: FAILED\n";
  let rec z = Cons { hd = (Gc.minor(); (one, one+1)); tl = z } in
  (* Trash the minor generation *)
  for i = 0 to 50000 do ignore (ref 0) done;
  if match z with
       Cons { hd = (1,2); tl = z' } -> z == z'
     | _ -> false
  then print_string "Test 3: passed\n"
  else print_string "Test 3: FAILED\n";
;;

let rec s = "bar"
and idx = 1
and x1 = let f x = Printf.printf "%s\n" x in f "foo"; s, x4
and x2 = [| x1; x1 |]
and x3 = Cons { hd = (fun () -> fst (x2.(idx))); tl = x3 }
and x4 = {contents = x3}
;;

let hd = function Cons { hd; _ } -> hd | Nil -> raise Not_found

;;
Gc.minor ();;
if (hd (!(snd (x2.(0))))) () == s
then print_string "Test 4: passed\n"
else print_string "Test 4: FAILED\n"
