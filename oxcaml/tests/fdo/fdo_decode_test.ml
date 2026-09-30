(* Tests of oxcaml-fdo-decode's perf script parsing. *)
module Perf_script = Fdo_decode_lib.Perf_script

let failures = ref 0

let check name cond =
  if not cond
  then (
    incr failures;
    Printf.eprintf "FAILED: %s\n%!" name)

let of_string ?executable text =
  let file = Filename.temp_file "fdo_decode_test" ".txt" in
  Fun.protect
    ~finally:(fun () -> Sys.remove file)
    (fun () ->
      Out_channel.with_open_text file (fun oc ->
          Out_channel.output_string oc text);
      let samples = ref [] in
      In_channel.with_open_text file (fun ic ->
          Perf_script.iter_channel ?executable ic ~f:(fun sample ->
              samples := sample :: !samples));
      List.rev !samples)

let fails f = match f () with () -> false | exception Failure _ -> true

let () =
  let samples =
    of_string
      "  2 401234 0x401000/0x401100/P/-/-/0/COND/- \
       0x7f0000000000/0x401000/P/-/-/0\n\
       7\n\
       \t401567\n\
       \t7f0000000000\n\
      \ 0x402000/0x403000/P/-/-/0\n\
       0x404000/0x405000/P/-/-/0\n"
  in
  check "sample order, weights, IP and branch order preserved"
    (samples
    = [ { Perf_script.count = 2L;
          ip = Some 0x401234L;
          branches = [0x401000L, 0x401100L; 0x7f0000000000L, 0x401000L]
        };
        { Perf_script.count = 7L;
          ip = Some 0x401567L;
          branches = [0x402000L, 0x403000L]
        };
        { Perf_script.count = 1L; ip = None; branches = [0x404000L, 0x405000L] }
      ]);
  check "garbage sample aborts"
    (fails (fun () -> ignore (of_string "not a sample")));
  check "garbage branch aborts" (fails (fun () -> ignore (of_string "x/y/z")));
  check "orphan callchain aborts"
    (fails (fun () -> ignore (of_string "\t401234")))

(* Process-specific PIE mappings, fork inheritance, exec and foreign code. *)
let () =
  let executable : Perf_script.executable =
    { path = "/bin/app.exe";
      address_of_offset =
        (fun offset ->
          if offset >= 0x1000L && offset < 0x2000L
          then Some (Int64.add 0x400000L offset)
          else None)
    }
  in
  let samples =
    of_string ~executable
      (String.concat "\n"
         [ "10 PERF_RECORD_COMM exec: app.exe:10/10";
           "10 PERF_RECORD_MMAP2 10/10: [0x55550000(0x1000) @ 0x1000 fd:01 1 \
            2]: r-xp /bin/app.exe";
           "10 2 55550020 0x55550010/0x55550004/P/-/-/0";
           "10 PERF_RECORD_FORK(11:11):(10:10)";
           "11 3 55550020 0x55550010/0x55550004/P/-/-/0";
           "12 PERF_RECORD_COMM exec: app.exe:12/12";
           "12 PERF_RECORD_MMAP2 12/12: [0x66660000(0x1000) @ 0x1000 fd:01 1 \
            2]: r-xp /bin/app.exe";
           "12 5 66660020 0x66660010/0x66660004/P/-/-/0";
           "12 7 7f000020 0x66660010/0x7f000004/P/-/-/0" ])
  in
  check "PIE translation and fork inheritance"
    (List.map (fun (s : Perf_script.sample) -> s.branches) samples
    = [ [0x401010L, 0x401004L];
        [0x401010L, 0x401004L];
        [0x401010L, 0x401004L];
        [0x401010L, Perf_script.foreign] ]);
  check "sample IP translated"
    (List.map (fun (s : Perf_script.sample) -> s.ip) samples
    = [Some 0x401020L; Some 0x401020L; Some 0x401020L; Some Perf_script.foreign]
    );
  check "missing executable mapping aborts"
    (fails (fun () ->
         ignore
           (of_string ~executable "10 1 55550020 0x55550010/0x55550004/P/-/-/0")))

let () =
  if !failures > 0
  then (
    Printf.eprintf "%d test(s) failed\n%!" !failures;
    exit 1)
  else print_endline "All tests passed"
