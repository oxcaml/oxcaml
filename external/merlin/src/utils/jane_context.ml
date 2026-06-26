type t = Vanilla | Jane_street

let initial_value =
  let context_str =
    Sys.getenv_opt "MERLIN_JANE_CONTEXT" |> Option.map String.lowercase_ascii
  in
  match context_str with
  | Some "vanilla" -> Vanilla
  | Some "jane" -> Jane_street
  | Some _ | None -> Vanilla

let current = ref initial_value
let get () = !current
let set value = current := value
