exception Error of Location.t * string

let fail loc message = raise (Error (loc, message))

let failf loc fmt = Printf.ksprintf (fun message -> fail loc message) fmt

let to_string (loc, message) =
  Printf.sprintf "%s: %s" (Location.to_string loc) message

let describe_exn = function
  | Error (loc, message) -> to_string (loc, message)
  | exn -> Printexc.to_string exn
