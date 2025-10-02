(* TEST
 include ocamlcommon;
*)

let locs_to_string ?(sep = " ") locs f : string =
  List.map
    (fun (m : _ Location.loc) ->
       Format.asprintf "%s [%a]" (f m.txt) Location.print_loc m.loc
    )
    locs
  |> String.concat sep

let mapper: Ast_mapper.mapper =
  let open Ast_mapper in
  { default_mapper with
    modes = (fun sub m ->
      (match m with
      | { pmode_modes = []; pmode_crossings = []; _ } -> ();
      | { pmode_modes; pmode_crossings; _ } ->
        Format.printf "modes: %s%s\n"
          (locs_to_string pmode_modes (fun (Mode s) -> s))
          (locs_to_string ~sep:"" pmode_crossings
            (fun (Crossing s) -> Format.sprintf " mod %s" s));
      );
      default_mapper.modes sub m
    );
    modalities = (fun sub m ->
      (match m with
        | { pmoda_modalities = []; pmoda_crossings = []; _ } -> ();
        | { pmoda_modalities; pmoda_crossings; _ } ->
          Format.printf "modalities: %s%s\n"
            (locs_to_string pmoda_modalities (fun (Modality s) -> s))
            (locs_to_string ~sep:"" pmoda_crossings
              (fun (Crossing s) -> Format.sprintf " mod %s" s));
      );
      default_mapper.modalities sub m
    );
  }

let test mapper s =
  let p = Lexing.from_string s |> Parse.implementation in
  ignore (mapper.Ast_mapper.structure mapper p);
  Format.printf "------------------------------\n"

let () =
  test mapper "let f (local_ x) = x";
  test mapper "let unique_ f (local_ x) = x";
  test mapper "let local_ f x: int -> int = x";
  test mapper "module M : sig val x : string -> string @ foo @@ bar hello end = struct end";
  (* CR zeisbach: add test cases for mods *)
  ()
