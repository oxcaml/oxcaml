module Make (Input : sig
  type t

  val read : string -> t
end) =
struct
  let cache : (string, File_id.t * float ref * Input.t) Hashtbl.t = Hashtbl.create 17

  let get_cached_entry ~title fid filename =
    let _ = title in
    let fid', latest_use, file = Hashtbl.find cache filename in
    if File_id.check fid fid' then () else raise Not_found;
    latest_use := Unix.time ();
    file
  ;;

  let read filename =
    let fid = File_id.get filename in
    let title = "read" in
    try get_cached_entry ~title fid filename with
    | Not_found ->
      (try
         let file = Input.read filename in
         Hashtbl.replace cache filename (fid, ref (Unix.time ()), file);
         file
       with
       | exn ->
         Hashtbl.remove cache filename;
         raise exn)
  ;;

  let get_cached_entry filename =
    let fid = File_id.get filename in
    let title = "get_cached_entry" in
    get_cached_entry ~title fid filename
  ;;

  let flush ?older_than () =
    let limit =
      match older_than with
      | None -> -.max_float
      | Some dt -> Unix.time () -. dt
    in
    let add_invalid filename (fid, latest_use, _) invalids =
      if !latest_use > limit && File_id.check (File_id.get filename) fid
      then invalids
      else filename :: invalids
    in
    let invalid = Hashtbl.fold add_invalid cache [] in
    List.iter (Hashtbl.remove cache) invalid
  ;;

  let clear () = Hashtbl.clear cache
end
