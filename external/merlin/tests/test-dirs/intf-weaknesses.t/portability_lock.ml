type 'a u = (string * 'a) list
type 'a t = { mutable entries : 'a u }

(* Legacy toplevel state: [create] reads it through the portability lock, which keeps
   [create] unportable while the other values stay portable. *)
let total_registries = ref 0

let create () =
  incr total_registries;
  { entries = [] }
;;

let rec find_entry name = function
  | [] -> None
  | (key, data) :: rest ->
    if String.equal key name then Some data else find_entry name rest
;;

let register t ~name value =
  match find_entry name t.entries with
  | Some _ -> Error "duplicate registration"
  | None ->
    t.entries <- (name, value) :: t.entries;
    Ok ()
;;

let find t name = find_entry name t.entries

let rec iter_data f = function
  | [] -> ()
  | (_, data) :: rest ->
    f data;
    iter_data f rest
;;

let iter t ~f = iter_data f t.entries
