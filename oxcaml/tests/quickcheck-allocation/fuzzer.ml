module Suspect = struct
  type t =
    { seed : int;
      sample : Gen.Sample.t
    }

  let to_string { seed; sample } =
    Printf.sprintf "seed=%d\n%s" seed (Gen.Sample.to_string sample)
end

module Gap = struct
  type t =
    { seed : int;
      cause : string;
      sample : Gen.Sample.t
    }
end

module Stats = struct
  type t =
    { mutable agree_noalloc : int;
      mutable agree_alloc : int;
      mutable gen_errors : int;
      suspects : Suspect.t Queue.t;
      gaps : Gap.t Queue.t
    }

  let create () =
    { agree_noalloc = 0;
      agree_alloc = 0;
      gen_errors = 0;
      suspects = Queue.create ();
      gaps = Queue.create ()
    }

  let agree_noalloc t = t.agree_noalloc

  let agree_alloc t = t.agree_alloc

  let gen_errors t = t.gen_errors

  let suspects t = List.of_seq (Queue.to_seq t.suspects)

  let gaps t = List.of_seq (Queue.to_seq t.gaps)

  let record t ~seed ~(sample : Gen.Sample.t) ~frontend ~backend =
    match Oracle.classify ~frontend ~backend with
    | Error _ -> t.gen_errors <- t.gen_errors + 1
    | Ok Oracle.Quadrant.Agree_noalloc -> t.agree_noalloc <- t.agree_noalloc + 1
    | Ok Oracle.Quadrant.Agree_alloc -> t.agree_alloc <- t.agree_alloc + 1
    | Ok Oracle.Quadrant.Soundness_suspect ->
      Queue.add { Suspect.seed; sample } t.suspects
    | Ok Oracle.Quadrant.Precision_gap ->
      let cause = Oracle.Verdict.cause frontend in
      Queue.add { Gap.seed; cause; sample } t.gaps
end

let write_source path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

let cleanup mlfile =
  let base = Filename.remove_extension mlfile in
  List.iter
    (fun ext -> try Sys.remove (base ^ ext) with _ -> ())
    [".ml"; ".cmi"; ".cmo"; ".cmx"; ".cmt"; ".cmti"; ".o"]

let run_one stats ~compiler ~seed ~mode =
  let sample = Gen.generate ~mode ~seed in
  let fe = Filename.temp_file "qc_fe" ".ml" in
  let be = Filename.temp_file "qc_be" ".ml" in
  write_source fe sample.Gen.Sample.fe_source;
  write_source be sample.Gen.Sample.be_source;
  let frontend = Oracle.run_frontend ~compiler ~file:fe in
  let backend = Oracle.run_backend ~compiler ~file:be in
  Stats.record stats ~seed ~sample ~frontend ~backend;
  cleanup fe;
  cleanup be

let run ~compiler ~count ~seed0 ~mode =
  let stats = Stats.create () in
  for k = 0 to count - 1 do
    let seed = seed0 + k in
    run_one stats ~compiler ~seed ~mode
  done;
  stats

let report stats =
  let suspects = Stats.suspects stats in
  let gaps = Stats.gaps stats in
  Printf.printf
    "agree(noalloc)=%d agree(alloc)=%d suspects=%d gaps=%d gen_errors=%d\n"
    (Stats.agree_noalloc stats)
    (Stats.agree_alloc stats) (List.length suspects) (List.length gaps)
    (Stats.gen_errors stats);
  if suspects <> []
  then (
    print_endline "\nsoundness suspects (FE-accept & BE-reject):";
    List.iter (fun s -> print_endline (Suspect.to_string s)) suspects);
  if gaps <> []
  then (
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun { Gap.cause; _ } ->
        let n = try Hashtbl.find tbl cause with Not_found -> 0 in
        Hashtbl.replace tbl cause (n + 1))
      gaps;
    let ranked =
      List.sort
        (fun (_, a) (_, b) -> compare b a)
        (Hashtbl.fold (fun c n acc -> (c, n) :: acc) tbl [])
    in
    print_endline "\nprecision-gap causes (ranked):";
    List.iter (fun (c, n) -> Printf.printf "  %4d  %s\n" n c) ranked)
