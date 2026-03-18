(* TEST
  flags = "-extension runtime_metaprogramming";
  native;
*)

#syntax quotations on

(* Test that local (stack_) allocations work inside splices.
   This exercises the Lregion wrapping of antiquotations
   in translquote.ml. The outer Sys.opaque_identity prevents
   dead-code elimination of the local alloc; the inner one
   prevents static allocation. The fun () -> wrapper places the
   splice inside a quotation function binding (nlocal=0), ensuring
   there is no enclosing region other than the Lregion. *)

let () =
  Printf.printf "\nTest stack_ tuple in splice\n";
  let f = [%eval: unit -> int]
    <[ fun () -> $(
      let local_ _p : int * int =
        Sys.opaque_identity
          (stack_ (Sys.opaque_identity 1, 2))
      in
      <[ 42 ]>
    ) ]> in
  Printf.printf "Output: %d\n" (f ())
;;

let () =
  Printf.printf "\nTest stack_ ref in splice\n";
  let f = [%eval: unit -> int]
    <[ fun () -> $(
      let local_ r =
        Sys.opaque_identity
          (stack_ (ref (Sys.opaque_identity true)))
      in
      if !r then <[ 55 ]> else <[ 0 ]>
    ) ]> in
  Printf.printf "Output: %d\n" (f ())
;;

let () =
  Printf.printf "\nTest stack_ option in splice\n";
  let f = [%eval: unit -> int]
    <[ fun () -> $(
      let local_ opt =
        Sys.opaque_identity
          (stack_ (Some (Sys.opaque_identity 42)))
      in
      match opt with
      | Some n -> if n > 0 then <[ 99 ]> else <[ 0 ]>
      | None -> <[ 0 ]>
    ) ]> in
  Printf.printf "Output: %d\n" (f ())
;;
