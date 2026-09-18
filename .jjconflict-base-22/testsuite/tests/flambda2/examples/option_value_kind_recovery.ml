(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-dlambda -dcanonical-ids";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* The parameter of [option] has kind [any]. If an option's payload type still
   has jkind [any] at translation time, [Typeopt.value_kind] then gives the
   option the variant kind [(consts (0)) (non_consts ([0: ?]))]. *)

let ret_phantom (h : unit -> 'a option) = h ()

let ret_sorted (h : unit -> 'a option) (d : 'a) = ignore d; h ()

let match_some (h : unit -> 'a option) =
  match h () with
  | None -> h ()
  | Some _ -> h ()

let match_wild (h : unit -> 'a option) =
  let o = h () in
  match o with
  | None -> true
  | _ -> false
