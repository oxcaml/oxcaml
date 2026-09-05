let main () =
  let buf = Buffer.create 4096 in
  let data = Mocks.large_int_assoc in
  Benchmark.throughputN ~repeat:3 8
    [
      ( "JSON writing with internal buffer",
        (fun () ->
          Out_channel.with_open_bin "/dev/null" (fun oc ->
              ignore (Yojson.Safe.to_channel oc data))),
        () );
      ( "JSON writing with provided buffer",
        (fun () ->
          Out_channel.with_open_bin "/dev/null" (fun oc ->
              ignore (Yojson.Safe.to_channel ~buf oc data))),
        () );
    ]
  |> Benchmark.tabulate

let () = main ()
