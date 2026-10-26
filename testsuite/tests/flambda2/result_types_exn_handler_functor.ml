(* TEST
 flambda2;
 flags = "-O3 -flambda2-join-algorithm=n-way";
 native;
*)

(* Same as [result_types_exn_handler.ml], but relying on functors having result
   types by default at -O3. *)

exception Fail of string

module [@inline never] F (X : sig
  val b : bool
end) =
struct
  let v = if X.b then raise (Fail "k") else "x"
end

module B = struct
  let b = true
end

let () =
  match
    let module M = F (B) in
    M.v
  with
  | s -> print_endline s
  | exception Fail s -> print_endline ("Fail " ^ s)
