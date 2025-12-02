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
      | No_modes -> ();
      | Modes { modes; crossings; _ } ->
        Format.printf "modes: %s%s\n"
          (locs_to_string modes (fun (Mode s) -> s))
          (locs_to_string ~sep:"" crossings
            (fun (Crossing s) -> Format.sprintf " mod %s" s));
      );
      default_mapper.modes sub m
    );
    modalities = (fun sub m ->
      (match m with
        | No_modalities -> ();
        | Modalities { modalities; crossings; _ } ->
          Format.printf "modalities: %s%s\n"
            (locs_to_string modalities (fun (Modality s) -> s))
            (locs_to_string ~sep:"" crossings
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
  ()
