let env_var = "OXCAML_DISSECTOR_GC"

let config = lazy (
  match Sys.getenv_opt env_var with
  | None -> None
  | Some s -> Some (String.split_on_char ',' s))

let phase_enabled name =
  match Lazy.force config with
  | None -> true
  | Some phases -> List.exists (fun p -> String.equal p name) phases

let verbose () = Option.is_some (Lazy.force config)

let heap_words () =
  let stat = Gc.quick_stat () in
  stat.heap_words

let compact_phase name =
  if phase_enabled name then begin
    let before = heap_words () in
    Gc.compact ();
    let after = heap_words () in
    if verbose () then
      Printf.eprintf
        "Dissector GC compact [%s]: %d -> %d words (freed %d words)\n%!"
        name before after (before - after)
  end
