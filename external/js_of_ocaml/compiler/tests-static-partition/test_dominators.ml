open! Js_of_ocaml_compiler.Stdlib
module Dominators = Jsoo_static_partition.Dominators

let print edges ~root ~nodes =
  let successors node =
    List.filter_map edges ~f:(fun (source, destination) ->
        if source = node then Some destination else None)
  in
  let idom = Dominators.immediate_dominators ~successors ~root in
  List.iter nodes ~f:(fun node ->
      match idom node with
      | dominator -> Printf.printf "%d: %d\n" node dominator
      | exception Not_found -> Printf.printf "%d: unreachable\n" node)

let%expect_test "diamond, loop, irreducible region, and unreachable node" =
  (* 0 -> 1 -> {2, 3} -> 4 -> 1 (loop); 5 is only reachable through 6, which is not. *)
  print
    [ 0, 1; 1, 2; 1, 3; 2, 4; 3, 4; 4, 1; 4, 7; 6, 5 ]
    ~root:0
    ~nodes:[ 0; 1; 2; 3; 4; 5; 6; 7 ];
  [%expect
    {|
    0: 0
    1: 0
    2: 1
    3: 1
    4: 1
    5: unreachable
    6: unreachable
    7: 4
    |}];
  (* Two entries into the cycle 2 <-> 3: neither dominates the other. *)
  print [ 0, 1; 0, 2; 1, 3; 2, 3; 3, 2; 3, 4 ] ~root:0 ~nodes:[ 1; 2; 3; 4 ];
  [%expect
    {|
    1: 0
    2: 0
    3: 0
    4: 3
    |}]
