exception Failed

external expose : string -> 'a -> unit = "partition_test_expose"
external record : string -> unit = "partition_test_record"
external probe : unit -> int = "partition_test_b_only"
external raise_ : exn -> 'a = "%raise"

let () =
  record "terminal-eager";
  expose "expectedException" (fun () -> Failed);
  expose "beforeFailure" (fun () -> 17);
  if probe () = 0
  then (
    record "left";
    raise_ Failed)
  else (
    record "right";
    raise_ Failed)
;;
