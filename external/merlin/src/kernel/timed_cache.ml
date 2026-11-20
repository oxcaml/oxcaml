type 'a entry = { value : 'a; mutable last_used : float }
type ('a, 'b) t = ('a, 'b entry) Hashtbl.t

let create () = Hashtbl.create 17

let read_or_compute t ~key ~f =
  match Hashtbl.find_opt t key with
  | Some entry ->
    entry.last_used <- Unix.time ();
    entry.value
  | None ->
    let value = f () in
    Hashtbl.add t key { last_used = Unix.time (); value };
    value

let flush ?older_than t =
  Option.iter
    (fun older_than ->
      let limit = Unix.time () -. older_than in
      Hashtbl.filter_map_inplace
        (fun _ entry -> if entry.last_used > limit then Some entry else None)
        t)
    older_than
