open Merlin_kernel

let create_position pos_cnum =
  { Lexing.pos_fname = "test.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum }

let create_tree intervals =
  intervals
  |> List.map (fun ((low, high), payload) ->
         let loc =
           { Location.loc_start = create_position low;
             loc_end = create_position high;
             loc_ghost = false
           }
         in
         Result.get_ok (Overrides_interval_tree.Interval.create ~loc ~payload))
  |> Overrides_interval_tree.of_alist

let test_of_alist_exn =
  let open Alcotest in
  test_case "test basic list construction" `Quick (fun () ->
      let _ : string Overrides_interval_tree.t =
        create_tree
          [ ((0, 1), "1");
            ((0, 3), "2");
            ((2, 3), "3");
            ((0, 4), "4");
            ((0, 10), "5");
            ((5, 10), "6");
            ((5, 7), "7");
            ((8, 10), "8");
            ((0, 2), "9")
          ]
      in
      ())

let test_invalid_interval =
  let open Alcotest in
  test_case "test creating invalid interval" `Quick (fun () ->
      let loc =
        { Location.loc_start = create_position 5;
          loc_end = create_position 0;
          loc_ghost = false
        }
      in
      let interval =
        Overrides_interval_tree.Interval.create ~loc ~payload:"invalid"
      in
      let is_ok = Result.is_ok interval in
      check bool "should be equal" is_ok false)

let test_find ~input ~expected =
  (*
    0 1 2 3 4 5 6 7 8 9 10
    ----------e----------
    ----d---- -----f-----
    ---b---   --g-- --h--
    -a- c             -i-
   *)
  let tree =
    create_tree
      [ ((0, 1), "a");
        ((0, 3), "b");
        ((2, 2), "c");
        ((0, 4), "d");
        ((0, 10), "e");
        ((5, 10), "f");
        ((5, 6), "g");
        ((8, 10), "h");
        ((9, 10), "i")
      ]
  in
  let open Alcotest in
  test_case
    ("test find on input " ^ Int.to_string input)
    `Quick
    (fun () ->
      let pos = create_position input in
      let payload = Overrides_interval_tree.find tree pos in
      check (option string) "should be equal" expected payload)

let _test_find_first =
  let tree = create_tree [ ((0, 4), "0"); ((2, 3), "1"); ((2, 3), "2") ] in
  let open Alcotest in
  test_case "test find on input with duplicate intervals" `Quick (fun () ->
      let expected = Some "1" in
      let pos = create_position 2 in
      let payload = Overrides_interval_tree.find tree pos in
      check (option string) "should be equal" expected payload)

let test_find_empty =
  let tree = create_tree [] in
  let open Alcotest in
  test_case "test find on empty tree" `Quick (fun () ->
      let expected = None in
      let pos = create_position 0 in
      let payload = Overrides_interval_tree.find tree pos in
      check (option string) "should be equal" expected payload)

let cases =
  ( "overrides-interval-tree",
    [ test_of_alist_exn;
      test_invalid_interval;
      test_find ~input:0 ~expected:(Some "a");
      test_find ~input:1 ~expected:(Some "a");
      test_find ~input:2 ~expected:(Some "c");
      test_find ~input:3 ~expected:(Some "b");
      test_find ~input:4 ~expected:(Some "d");
      test_find ~input:5 ~expected:(Some "g");
      test_find ~input:6 ~expected:(Some "g");
      test_find ~input:7 ~expected:(Some "f");
      test_find ~input:8 ~expected:(Some "h");
      test_find ~input:9 ~expected:(Some "i");
      test_find ~input:10 ~expected:(Some "i");
      test_find ~input:11 ~expected:None;
      test_find_empty
    ] )
