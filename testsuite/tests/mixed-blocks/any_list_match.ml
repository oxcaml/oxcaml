(* TEST
   include stdlib_upstream_compatible;
   ocamlopt_flags="-extension layouts_beta";
*)

(* Construct and pattern-match built-in [list]s at non-value kinds. *)

module Float_u = Stdlib_upstream_compatible.Float_u
module Int64_u = Stdlib_upstream_compatible.Int64_u

let () =
  let l = [Float_u.of_float 1.5; Float_u.of_float 2.5] in
  Gc.compact ();
  match l with
  | [a; b] ->
    Printf.printf "floats: %.1f %.1f\n" (Float_u.to_float a)
      (Float_u.to_float b)
  | _ -> print_endline "floats: unexpected shape"

let () =
  let l = [Int64_u.of_int64 3L; Int64_u.of_int64 4L] in
  Gc.compact ();
  match l with
  | [a; b] ->
    Printf.printf "int64s: %Ld %Ld\n" (Int64_u.to_int64 a)
      (Int64_u.to_int64 b)
  | _ -> print_endline "int64s: unexpected shape"

let () =
  let rec len (l : Float_u.t list) =
    match l with
    | [] -> 0
    | _ :: tl -> 1 + len tl
  in
  Printf.printf "length: %d\n"
    (len [Float_u.of_float 0.5; Float_u.of_float 1.5; Float_u.of_float 2.5])
