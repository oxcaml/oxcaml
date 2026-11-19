open Ppxlib

let loc = Location.none

module Ast_builder = (val Ast_builder.make loc)

let eta_reduce x =
  Format.printf "%a@." Pprintast.expression
    (Ast_builder.eta_reduce_if_possible x)

let () =
  print_endline "eta reduction applies:";
  eta_reduce [%expr fun x -> f x];
  eta_reduce [%expr fun x y -> f x y];
  print_endline "eta reduction doesn't apply:";
  (* There's no reason we couldn't eta-reduce here; it's just that the current
     code doesn't handle single-case functions specially. *)
  eta_reduce [%expr function x -> f x];
  eta_reduce [%expr fun y -> f x y];
  eta_reduce [%expr fun x -> f x y];
  eta_reduce [%expr fun x -> Non_identifier x];
  (* Attributes block eta reduction. *)
  eta_reduce [%expr fun [@attr] x y -> f x y];
  eta_reduce [%expr fun x -> fun [@attr] y -> f x y]
