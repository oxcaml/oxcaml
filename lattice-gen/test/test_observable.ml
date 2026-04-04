let replace_once haystack needle replacement =
  let h_len = String.length haystack in
  let n_len = String.length needle in
  let rec loop i =
    if i + n_len > h_len
    then None
    else if String.sub haystack i n_len = needle
    then Some i
    else loop (i + 1)
  in
  match loop 0 with
  | None -> failwith ("missing substring: " ^ needle)
  | Some i ->
    String.sub haystack 0 i
    ^ replacement
    ^ String.sub haystack (i + n_len) (h_len - i - n_len)

let slice_between haystack start_needle end_needle =
  let h_len = String.length haystack in
  let s_len = String.length start_needle in
  let e_len = String.length end_needle in
  let rec find_from i needle needle_len =
    if i + needle_len > h_len
    then None
    else if String.sub haystack i needle_len = needle
    then Some i
    else find_from (i + 1) needle needle_len
  in
  let start =
    match find_from 0 start_needle s_len with
    | Some i -> i
    | None -> failwith ("missing substring: " ^ start_needle)
  in
  let stop =
    match find_from start end_needle e_len with
    | Some i -> i + e_len
    | None -> failwith ("missing substring: " ^ end_needle)
  in
  String.sub haystack start (stop - start)

let expect_observable_error f needle =
  match f () with
  | () -> Test_support.failf "expected observable error containing %S" needle
  | exception Observable.Error message ->
    Test_support.ensure
      (Test_support.contains message needle)
      "expected observable error containing %S, got %S"
      needle
      message

let fixture_dir () =
  Filename.concat (Sys.getcwd ()) "observable-fixtures"

let read_fixture dir name = Test_support.read_file (Filename.concat dir name)

let check_basic dir =
  let html = read_fixture dir "basic.html" in
  Test_support.ensure
    (Test_support.contains html "<out>")
    "basic fixture should contain managed <out>";
  Test_support.ensure
    (Test_support.contains html "<ocaml-out>")
    "basic fixture should contain managed <ocaml-out>";
  Test_support.ensure
    (Test_support.contains html "Demo.ml")
    "basic fixture should render Demo.ml tab";
  Test_support.ensure
    (Test_support.contains html "Demo.mli")
    "basic fixture should render Demo.mli tab";
  Test_support.ensure
    (Test_support.contains html "generated lattice library")
    "basic fixture should render lattice header";
  Test_support.ensure
    (Test_support.contains html ">Local</pre>")
    "basic fixture should capture OCaml stdout"

let check_multi_lat dir =
  let html = read_fixture dir "multi-lat.html" in
  Test_support.ensure
    (Test_support.contains html "Lat_001.ml")
    "default root-module should use Lat_001";
  Test_support.ensure
    (Test_support.contains html "Extra.ml")
    "second lattice should render explicit module name";
  Test_support.ensure
    (Test_support.contains html ">Local\nB</pre>")
    "multi-lat fixture should preserve previous ocaml state";
  Test_support.ensure
    (Test_support.contains html "Lat_001.Locality.show saved")
    "source HTML should be preserved byte-for-byte"

let check_filtered dir =
  let html = read_fixture dir "filtered.html" in
  let no_solver_out = slice_between html "NoSolver.ml" "</out>" in
  Test_support.ensure
    (not (Test_support.contains no_solver_out "module Solver"))
    "include-solver=false should omit Solver";
  Test_support.ensure
    (Test_support.contains html ">B</pre>")
    "filtered fixture should still run ocaml block"

let check_cumulative dir =
  let html = read_fixture dir "cumulative.html" in
  Test_support.ensure
    (Test_support.contains html ">7\nB</pre>")
    "cumulative fixture should see previous ocaml definitions and prior lattices"

let run () =
  Test_support.with_temp_dir "observable-suite-" (fun dir ->
    let fixtures_src = fixture_dir () in
    let fixtures_dst = Filename.concat dir "observable-fixtures" in
    Test_support.copy_tree ~src:fixtures_src ~dst:fixtures_dst;
    Observable.process_paths ~mode:Observable.Update [ fixtures_dst ];
    Test_support.ensure
      (Sys.file_exists (Filename.concat fixtures_dst "lattice-observable.css"))
      "missing observable css asset";
    Test_support.ensure
      (Sys.file_exists (Filename.concat fixtures_dst "lattice-observable.js"))
      "missing observable js asset";
    check_basic fixtures_dst;
    check_multi_lat fixtures_dst;
    check_filtered fixtures_dst;
    check_cumulative fixtures_dst;
    Observable.process_paths ~mode:Observable.Check [ fixtures_dst ];
    let cumulative_path = Filename.concat fixtures_dst "cumulative.html" in
    let broken =
      replace_once
        (Test_support.read_file cumulative_path)
        ">7\nB</pre>"
        ">BROKEN</pre>"
    in
    Test_support.write_file cumulative_path broken;
    expect_observable_error
      (fun () -> Observable.process_paths ~mode:Observable.Check [ fixtures_dst ])
      "ocaml block")
