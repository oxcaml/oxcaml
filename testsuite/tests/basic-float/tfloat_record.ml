(* TEST *)

module Float_record : sig
  type t = private float;;

  val make : float -> t;;
  val from : t -> float;;

  type s = {f : t};;
end = struct
  type t = float;;

  let make f = f;;

  let from t = t;;

  type s = {f : t};;
end

module Float_array = struct
  let small_float_array x =
    [|1.;2.;3.|], x

  let longer_float_array x =
    [|1.;2.;3.;4.;5.;6.;7.;8.;9.;0.;
      1.;2.;3.;4.;5.;6.;7.;8.;9.;0.;
      1.;2.;3.;4.;5.;6.;7.;8.;9.;0.;
      1.;2.;3.;4.;5.;6.;7.;8.;9.;0.;|], x
end

let s = { Float_record.f = Float_record.make 1.0 };;

print_float (Float_record.from s.Float_record.f);;
print_newline ();;


let b = Float_array.small_float_array 12
(* CR xclerc for lmaurer: uncomment the `[@inlined]` annotation below. *)
let c = (Float_array.longer_float_array (*[@inlined]*)) 34

let print_array a =
  Array.iter (fun f ->
      print_float f;
      print_newline ()) a;
  print_newline ()

let () =
  print_array (fst b);
  print_array (fst c);

module Poly_record : sig
  type 'a t = { f : 'a }

  val make : 'a -> 'a t
  val f : 'a t -> 'a
end = struct
  type 'a t = { f : 'a }

  let[@inline never] make f =
    (* [f] has unknown type so this can't be a flat float block *)
    { f }
  let[@inline never] f t =
    (* Similarly, this can't access [t] as a flat float block *)
    t.f
end

let f =
  let open Poly_record in
  (* Make sure it's not possible to create a [Poly_record.t] as a flat float
     block or to access one as if it's a flat float block *)
  let t = { f = 1. } in
  print_float (f t); (* crash if [t] is flat *)
  print_newline ();

  let t = make 2. in
  print_float t.f; (* crash if we think [t] is flat *)
  print_newline ();
