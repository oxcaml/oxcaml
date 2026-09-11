(* -*- tuareg -*- *)

include StdLabels
include Printf

let capitalize_ascii = Stdppx.String.capitalize_ascii

(* Reexports from [Astlib_cinaps_helpers] *)
let nl = Astlib_cinaps_helpers.nl
let qualified_types = Astlib_cinaps_helpers.qualified_types
let foreach_module = Astlib_cinaps_helpers.foreach_module
let foreach_type = Astlib_cinaps_helpers.foreach_type
let all_types = List.concat (List.map ~f:snd qualified_types)

let foreach_version f =
  nl ();
  List.iter Supported_version.all ~f:(fun v ->
      f (Supported_version.to_int v) (Supported_version.to_string v))

let foreach_version_pair f =
  nl ();
  let rec aux = function
    | x :: (y :: _ as tail) ->
        f (Supported_version.to_int x) (Supported_version.to_int y);
        aux tail
    | [ _ ] | [] -> ()
  in
  aux Supported_version.all

let with_then_and () =
  let first = ref true in
  fun oc ->
    output_string oc (if !first then "with" else " and");
    first := false
