(* This shows the use of [%probe_is_enabled name ..] without the corresponding
   [%probe name] in the same compilation unit. Another probe is present. *)
[@@@warning "+a-56"]

let () = [%probe "other" ()]

module Nothing = struct
  type t = |

  let unreachable_code = function (_ : t) -> .
end

let[@inline never] [@local never] test () =
  (* Probes_lib.Self.update (All Enable) ~force:true; *)
  Printf.printf "%%probe_is_enabled \"broken_probe\" = %b\n"
    [%probe_is_enabled "broken_probe"];
  match
    (if [%probe_is_enabled "broken_probe"]
     then (
       print_endline "probe on";
       None)
     else failwith "probe off"
      : Nothing.t option)
  with
  | None -> ()
  | Some nothing ->
    [%probe "broken_probe" ()];
    Nothing.unreachable_code nothing

let () = test ()
