let main () =
  Benchmark.throughputN ~repeat:3 8
    [
      ( "JSON reading",
        (fun () -> ignore (Yojson.Safe.from_string Mocks.data)),
        () );
      ( "JSON writing",
        (fun () -> ignore (Yojson.Safe.to_string Mocks.yojson_data)),
        () );
      ( "JSON writing assoc",
        (fun () -> ignore (Yojson.Safe.to_string Mocks.large_int_assoc)),
        () );
      ( "JSON writing int list",
        (fun () -> ignore (Yojson.Safe.to_string Mocks.large_int_list)),
        () );
      ( "JSON writing string list",
        (fun () -> ignore (Yojson.Safe.to_string Mocks.large_string_list)),
        () );
      ( "JSON writing int list to channel",
        (fun () ->
          Out_channel.with_open_bin "/dev/null" @@ fun oc ->
          ignore (Yojson.Safe.to_channel oc Mocks.large_int_list)),
        () );
      ( "JSON writing string list to channel",
        (fun () ->
          Out_channel.with_open_bin "/dev/null" @@ fun oc ->
          ignore (Yojson.Safe.to_channel oc Mocks.large_string_list)),
        () );
      ( "JSON writing assoc to channel",
        (fun () ->
          Out_channel.with_open_bin "/dev/null" @@ fun oc ->
          ignore (Yojson.Safe.to_channel oc Mocks.large_int_assoc)),
        () );
      (let buf = Buffer.create 1000 in
       ( "JSON seq roundtrip",
         (fun () ->
           let stream =
             Yojson.Safe.seq_from_string ~buf Mocks.streamable_string
           in
           ignore (Yojson.Safe.seq_to_string ~buf stream)),
         () ));
    ]
  |> Benchmark.tabulate

let () = main ()
