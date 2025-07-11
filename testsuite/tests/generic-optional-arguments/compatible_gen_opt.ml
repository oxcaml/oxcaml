(* TEST
 flags = "-extension-universe alpha";
*)

(* Compatible scenarios - Stdlib.Option.?' is compatible with vanilla ? *)

(* Function with Stdlib.Option.?' parameter *)
module M1 = struct
  let f Stdlib.Option.?'(x : int = 42) () = x
end

(* Function with vanilla ? parameter *)
module M2 = struct
  let f ?x () = match x with None -> 0 | Some v -> v
end

(* These should work: *)
let () =
  (* Call Stdlib.Option.?' function with vanilla ? *)
  let _ = M1.f ?x:(Some 10) () in
  let _ = M1.f ?x:None () in

  (* Call vanilla ? function with Stdlib.Option.?' *)
  let _ = M2.f Stdlib.Option.?'x () in
  let _ = M2.f () in

  (* Interface compatibility *)
  let module type S = sig
    val f : ?x:int -> unit -> int
  end in
  let module N : S = struct
    let f Stdlib.Option.?'(x : int = 42) () = x
  end in

  (* Interface compatibility reverse *)
  let module type T = sig
    val f : Stdlib.Option.?'x:int -> unit -> int
  end in
  let module O : T = struct
    let f ?x () = match x with None -> 0 | Some v -> v
  end in

  print_endline "All compatible scenarios work!"
