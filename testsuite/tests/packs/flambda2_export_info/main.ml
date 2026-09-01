(* TEST
 flambda2;
 readonly_files = "member_a.ml member_b.ml";
 setup-ocamlopt.byte-build-env;
 ocamlopt_flags = "-O3";
 flags = "-for-pack Pack";
 module = "member_a.ml";
 ocamlopt.byte;
 module = "member_b.ml";
 ocamlopt.byte;
 module = "";
 flags = "-pack";
 program = "pack.cmx";
 all_modules = "member_a.cmx member_b.cmx";
 ocamlopt.byte;
 flags = "-w +a-70 -warn-error +55";
 module = "main.ml";
 ocamlopt.byte;
 module = "";
 program = "${test_build_directory}/main.exe";
 all_modules = "pack.cmx main.cmx";
 ocamlopt.byte;
 exit_status = "0";
 run;
 check-program-output;
*)

(* This test checks that the Flambda 2 export information stored in a packed
   [.cmx] is correct, by forcing a downstream module to inline code coming from
   packed members. Warning 55 ("inlining impossible"), turned into an error
   below, would fire if the packed code sections could not be found and
   inlined. *)

let () =
  let a = (Pack.Member_a.triple [@inlined]) 4 in
  let b = (Pack.Member_b.add_seven [@inlined]) 10 in
  let add100 = Pack.Member_a.make_adder 100 in
  Printf.printf "%d %d %d\n" a b (add100 1);
  assert (a = 12);
  assert (b = 17);
  assert (add100 1 = 101)
