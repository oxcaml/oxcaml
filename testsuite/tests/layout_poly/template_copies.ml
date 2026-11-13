(* TEST
 flambda2;
 (* The unrolling is refused, with warning 55. *)
 flags = "-extension layout_poly_alpha -w -55";
 {
   ocamlopt_flags = "-O3";
   native;
 }{
   ocamlopt_flags = "-O3 -flambda2-reaper";
   native;
 }{
   ocamlopt_flags = "-O3 -flambda2-match-in-match";
   native;
 }{
   ocamlopt_flags = "-Oclassic";
   native;
 }
*)

(* The code of an instantiation of a template is simplified once, using the type
   that the template's environments have at the template's definition (see
   [Flambda_primitive.Template_env]). That is only correct if Simplify never
   copies the definition before simplifying it, since a copy could create
   environments with a different type: here, each copy of a function would lift
   its own closure for [k] to a symbol. So the unrolling asked for below is not
   done. *)

(* The template is defined in the function being unrolled. *)
let rec defined_in_body n =
  if n <= 0
  then true
  else
    let k x = x + 1 in
    let poly_ t y = #(k, y) in
    let #(k', _) = t 1 in
    let here = k' == k in
    here && (defined_in_body [@unrolled 3]) (n - 1)

(* The template is defined in a function defined in the function being
   unrolled: unrolling would copy the code of [nested] too. *)
let rec defined_in_nested_function n =
  if n <= 0
  then true
  else
    let[@inline never] nested () =
      let k x = x + 1 in
      let poly_ t y = #(k, y) in
      let #(k', _) = t 1 in
      k' == k
    in
    let here = nested () in
    here && (defined_in_nested_function [@unrolled 3]) (n - 1)

(* The partial application of an instantiation creates a closure whose code is
   shared by every closure created there. *)
let rec partially_applied n =
  if n <= 0
  then true
  else
    let k x = x + 1 in
    let poly_ t y = k, y in
    let here =
      match List.map t [1] with [(k', _)] -> k' == k | _ -> false
    in
    here && (partially_applied [@unrolled 3]) (n - 1)

(* The code of [F]'s instantiation for this argument is shared by every
   application of [F] to it, and calls the code of [g]'s instantiation
   directly. *)
module type S = sig
  val g : layout_ l. ('a : l). 'a -> #((int -> int) * 'a)
end

module F (X : S @ static) = struct
  let[@inline never] r () =
    let #(k, _) = X.g 1 in
    k
end

let rec called_from_functor n =
  if n <= 0
  then true
  else
    let k x = x + 1 in
    let module R = F (struct
      let poly_ g y = #(k, y)
    end) in
    let here = R.r () == k in
    here && (called_from_functor [@unrolled 3]) (n - 1)

(* The template is defined in the handler of a join point, which match-in-match
   would specialise for each of the join point's uses. *)
type ab =
  | A of int
  | B of int

let[@inline never] defined_in_join n =
  let ab = if n < 0 then A n else B n in
  let poly_ _t y = #(ab, y) in
  match ab with A m -> m < 0 | B m -> m >= 0

let () =
  let n = Sys.opaque_identity 4 in
  Printf.printf "%b %b %b %b %b\n" (defined_in_body n)
    (defined_in_nested_function n) (partially_applied n) (called_from_functor n)
    (defined_in_join n && defined_in_join (-n))
