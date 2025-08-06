(* TEST
 include stdlib_upstream_compatible;
 flags = "-extension-universe alpha -dlambda";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

type ('b : bits32) bits32_option =
  | None32
  | Some32 of 'b
[@@option_like]

module Int32_u = Stdlib_upstream_compatible.Int32_u

let print_int32u prefix x =
  Printf.printf "%s: %ld\n" prefix (Int32_u.to_int32 x)

let print_int32u_bits32_option prefix x =
  match x with
  | None32 -> Printf.printf "%s: None32\n" prefix
  | Some32 x -> Printf.printf "%s: Some32 %ld\n" prefix (Int32_u.to_int32 x)

let test() =
  let f (?x : 'a bits32_option) () = x in
  let v = f () in
  let _ = print_int32u_bits32_option "v1=None" v in
  let v = f ~x:(#3l) () in
  let _ = print_int32u_bits32_option "v2=3l" v in
  let v = f ?x:(Some32 #3l) () in
  let _ = print_int32u_bits32_option "v3=3l" v in
  let v = f ?x:(None32) () in
  let _ = print_int32u_bits32_option "v4=None" v in
  let g (?(x = #2l) : int32# bits32_option) () = x in
  let v = g() in
  let _ = print_int32u "v5=2l" v in
  let v = g ~x:#3l () in
  let _ = print_int32u "v6=3l" v in
  let v = g ?x:(Some32 #3l) () in
  let _ = print_int32u "v7=3l" v in
  let v = g ?x:(None32) () in
  let _ = print_int32u "v8=2l" v in
  ()

(* CR generic-optional: check matching.ml access patterns

  Currently the code below triggers a Core Dump:

  {v
    let _ = test ()
  v}

  The reason is that we are using Pfield to access the element of an option,
  but we should use Pmixedfield *)
