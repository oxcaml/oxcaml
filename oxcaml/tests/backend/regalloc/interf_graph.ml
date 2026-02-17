(* Random-based equivalence testing for BitMatrix vs EdgeSet *)

open Regalloc_interf_graph

(* Test helper to compare two implementations *)
module Test (S1 : S) (S2 : S) = struct
  let test_equivalence ~num_registers ~num_operations =
    Random.init 42;
    let s1 = S1.make ~num_registers in
    let s2 = S2.make ~num_registers in
    let check_mem_equal p =
      let m1 = S1.mem s1 p in
      let m2 = S2.mem s2 p in
      if m1 <> m2
      then
        Printf.printf "FAIL: mem mismatch for edge %s: s1=%b s2=%b\n"
          (Edge.to_string p) m1 m2;
      assert (m1 = m2)
    in
    let check_cardinal_equal () =
      let c1 = S1.For_debug.cardinal s1 in
      let c2 = S2.For_debug.cardinal s2 in
      if c1 <> c2
      then Printf.printf "FAIL: cardinal mismatch: s1=%d s2=%d\n" c1 c2;
      assert (c1 = c2)
    in
    let check_iter_equal () =
      let l1 = ref [] in
      let l2 = ref [] in
      S1.For_debug.iter s1 ~f:(fun p -> l1 := p :: !l1);
      S2.For_debug.iter s2 ~f:(fun p -> l2 := p :: !l2);
      let sort_edges l =
        List.sort
          (fun p1 p2 ->
            let c = Reg.Stamp.compare (Edge.fst p1) (Edge.fst p2) in
            if c <> 0 then c else Reg.Stamp.compare (Edge.snd p1) (Edge.snd p2))
          l
      in
      let l1_sorted = sort_edges !l1 in
      let l2_sorted = sort_edges !l2 in
      if l1_sorted <> l2_sorted
      then begin
        Printf.printf "FAIL: iter mismatch\n";
        Printf.printf "s1 edges: ";
        List.iter (fun p -> Printf.printf "%s " (Edge.to_string p)) l1_sorted;
        Printf.printf "\ns2 edges: ";
        List.iter (fun p -> Printf.printf "%s " (Edge.to_string p)) l2_sorted;
        Printf.printf "\n"
      end;
      assert (l1_sorted = l2_sorted)
    in
    for i = 0 to num_operations - 1 do
      let op = Random.int 100 in
      if op < 5
      then begin
        (* Clear operation (5%) *)
        S1.clear s1;
        S2.clear s2;
        check_cardinal_equal ();
        check_iter_equal ()
      end
      else if op < 55
      then begin
        (* Add operation (50%) *)
        let r1 = Random.int num_registers in
        let r2 = Random.int num_registers in
        let p =
          Edge.make (Reg.Stamp.of_int_unsafe r1) (Reg.Stamp.of_int_unsafe r2)
        in
        S1.add s1 p;
        S2.add s2 p;
        check_mem_equal p
      end
      else begin
        (* Membership test (45%) *)
        let r1 = Random.int num_registers in
        let r2 = Random.int num_registers in
        let p =
          Edge.make (Reg.Stamp.of_int_unsafe r1) (Reg.Stamp.of_int_unsafe r2)
        in
        check_mem_equal p
      end;
      (* Periodically check cardinal and iter *)
      if i mod 1000 = 0
      then begin
        check_cardinal_equal ();
        check_iter_equal ()
      end
    done;
    (* Final comprehensive check *)
    check_cardinal_equal ();
    check_iter_equal ();
    (* Check all possible edges *)
    for i = 0 to num_registers - 1 do
      for j = i to num_registers - 1 do
        check_mem_equal
          (Edge.make (Reg.Stamp.of_int_unsafe i) (Reg.Stamp.of_int_unsafe j))
      done
    done
end

module EdgeSet_vs_BitMatrix = Test (EdgeSet) (BitMatrix)

let () =
  Printf.printf "Testing EdgeSet vs BitMatrix equivalence...\n";
  (* Small test *)
  Printf.printf "  Small test (10 registers, 10k operations)...\n";
  EdgeSet_vs_BitMatrix.test_equivalence ~num_registers:10 ~num_operations:10_000;
  (* Medium test *)
  Printf.printf "  Medium test (50 registers, 50k operations)...\n";
  EdgeSet_vs_BitMatrix.test_equivalence ~num_registers:50 ~num_operations:50_000;
  (* Large test *)
  Printf.printf "  Large test (200 registers, 100k operations)...\n";
  EdgeSet_vs_BitMatrix.test_equivalence ~num_registers:200
    ~num_operations:100_000;
  Printf.printf "All tests passed!\n"
