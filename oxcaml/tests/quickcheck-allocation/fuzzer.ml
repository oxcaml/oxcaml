module Suspect = struct
  type t =
    { seed : int;
      sample : Gen.Sample.t;
      backend_error : string
    }

  let to_string { seed; sample; backend_error } =
    Printf.sprintf
      "seed=%d\n%sbackend error:\n%s"
      seed
      (Gen.Sample.to_string sample)
      backend_error
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
      mutable gen_errors : int;
      suspects : Suspect.t Queue.t;
      gaps : Gap.t Queue.t
    }

  let create () =
    { agree_noalloc = 0;
      gen_errors = 0;
      suspects = Queue.create ();
      gaps = Queue.create ()
    }

  let agree_noalloc t = t.agree_noalloc

  let gen_errors t = t.gen_errors

  let suspects t = List.of_seq (Queue.to_seq t.suspects)

  let gaps t = List.of_seq (Queue.to_seq t.gaps)

  let record t ~seed ~(sample : Gen.Sample.t) ~outcome =
    match (outcome : (Oracle.Outcome.t, string) result) with
    | Error _ -> t.gen_errors <- t.gen_errors + 1
    | Ok Oracle.Outcome.Agree_noalloc -> t.agree_noalloc <- t.agree_noalloc + 1
    | Ok (Oracle.Outcome.Soundness_suspect { backend_error }) ->
      Queue.add { Suspect.seed; sample; backend_error } t.suspects
    | Ok (Oracle.Outcome.Fe_reject { cause }) ->
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
  let file = Filename.temp_file "qc_alloc" ".ml" in
  write_source file sample.Gen.Sample.source;
  let outcome = Oracle.check ~compiler ~file in
  Stats.record stats ~seed ~sample ~outcome;
  cleanup file

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
    "agree(noalloc)=%d suspects=%d fe_rejects=%d gen_errors=%d\n"
    (Stats.agree_noalloc stats)
    (List.length suspects) (List.length gaps) (Stats.gen_errors stats);
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
    print_endline "\nfrontend rejections (completeness candidates, ranked by cause):";
    List.iter (fun (c, n) -> Printf.printf "  %4d  %s\n" n c) ranked)
