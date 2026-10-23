(* TEST
 stack-allocation;
 {
   bytecode;
 }{
   native;
 }
*)

(* This was producing an error in ocamlopt because the call to [failwith] had
   the close-at-apply flag on, which wasn't cleared when tmc rewrote the call to
   one not in tail position. *)

let[@tail_mod_cons] rec map2_exn l0 l1 ~f =
  match l0, l1 with
  | [], [] -> []
  | h0 :: t0, h1 :: t1 -> f h0 h1 :: map2_exn t0 t1 ~f
  | _ -> failwith (let message () = "urk" in message ())
;;

(* The direct function builds the cons cell, then fills its hole with dps
   code. In that dps code the result of [List.rev xs] is written to the
   destination, so the call is no longer in tail position of the region of
   the inlined helper [go], and must not close it. *)
let[@inline never] use_local (local_ _ : int ref) = ()

let[@tail_mod_cons] rec rev_after_zero = function
  | [] -> []
  | x :: xs ->
    x :: (let go () =
            let local_ r = ref x in
            use_local r;
            if x = 0 then List.rev xs else rev_after_zero xs
          in
          go ())

let () = assert (rev_after_zero [1; 2; 0; 3; 4] = [1; 2; 0; 4; 3])

(* As above, for a method call. *)
let[@tail_mod_cons] rec send_after_zero (o : < m : int list >) = function
  | [] -> []
  | x :: xs ->
    x :: (let go () =
            let local_ r = ref x in
            use_local r;
            if x = 0 then o#m else send_after_zero o xs
          in
          go ())

let () =
  let o = object method m = [42] end in
  assert (send_after_zero o [1; 2; 0; 3] = [1; 2; 0; 42])
