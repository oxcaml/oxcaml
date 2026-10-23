(* TEST
 (* [Big] has more runtime parameters than [Lambda.max_arity], so its
    instantiating functor must be split into a curried chain. *)

 readonly_files = "gen.sh p.mli p_int.mli p_int.ml";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "sh gen.sh .";
 script;

 set ms_ml = "\
   m001.ml m002.ml m003.ml m004.ml m005.ml m006.ml m007.ml m008.ml \
   m009.ml m010.ml m011.ml m012.ml m013.ml m014.ml m015.ml m016.ml \
   m017.ml m018.ml m019.ml m020.ml m021.ml m022.ml m023.ml m024.ml \
   m025.ml m026.ml m027.ml m028.ml m029.ml m030.ml m031.ml m032.ml \
   m033.ml m034.ml m035.ml m036.ml m037.ml m038.ml m039.ml m040.ml \
   m041.ml m042.ml m043.ml m044.ml m045.ml m046.ml m047.ml m048.ml \
   m049.ml m050.ml m051.ml m052.ml m053.ml m054.ml m055.ml m056.ml \
   m057.ml m058.ml m059.ml m060.ml m061.ml m062.ml m063.ml m064.ml \
   m065.ml m066.ml m067.ml m068.ml m069.ml m070.ml m071.ml m072.ml \
   m073.ml m074.ml m075.ml m076.ml m077.ml m078.ml m079.ml m080.ml \
   m081.ml m082.ml m083.ml m084.ml m085.ml m086.ml m087.ml m088.ml \
   m089.ml m090.ml m091.ml m092.ml m093.ml m094.ml m095.ml m096.ml \
   m097.ml m098.ml m099.ml m100.ml m101.ml m102.ml m103.ml m104.ml \
   m105.ml m106.ml m107.ml m108.ml m109.ml m110.ml m111.ml m112.ml \
   m113.ml m114.ml m115.ml m116.ml m117.ml m118.ml m119.ml m120.ml \
   m121.ml m122.ml m123.ml m124.ml m125.ml m126.ml m127.ml m128.ml \
   m129.ml m130.ml \
 ";

 set ms_cmx = "\
   m001.cmx m002.cmx m003.cmx m004.cmx m005.cmx m006.cmx m007.cmx \
   m008.cmx m009.cmx m010.cmx m011.cmx m012.cmx m013.cmx m014.cmx \
   m015.cmx m016.cmx m017.cmx m018.cmx m019.cmx m020.cmx m021.cmx \
   m022.cmx m023.cmx m024.cmx m025.cmx m026.cmx m027.cmx m028.cmx \
   m029.cmx m030.cmx m031.cmx m032.cmx m033.cmx m034.cmx m035.cmx \
   m036.cmx m037.cmx m038.cmx m039.cmx m040.cmx m041.cmx m042.cmx \
   m043.cmx m044.cmx m045.cmx m046.cmx m047.cmx m048.cmx m049.cmx \
   m050.cmx m051.cmx m052.cmx m053.cmx m054.cmx m055.cmx m056.cmx \
   m057.cmx m058.cmx m059.cmx m060.cmx m061.cmx m062.cmx m063.cmx \
   m064.cmx m065.cmx m066.cmx m067.cmx m068.cmx m069.cmx m070.cmx \
   m071.cmx m072.cmx m073.cmx m074.cmx m075.cmx m076.cmx m077.cmx \
   m078.cmx m079.cmx m080.cmx m081.cmx m082.cmx m083.cmx m084.cmx \
   m085.cmx m086.cmx m087.cmx m088.cmx m089.cmx m090.cmx m091.cmx \
   m092.cmx m093.cmx m094.cmx m095.cmx m096.cmx m097.cmx m098.cmx \
   m099.cmx m100.cmx m101.cmx m102.cmx m103.cmx m104.cmx m105.cmx \
   m106.cmx m107.cmx m108.cmx m109.cmx m110.cmx m111.cmx m112.cmx \
   m113.cmx m114.cmx m115.cmx m116.cmx m117.cmx m118.cmx m119.cmx \
   m120.cmx m121.cmx m122.cmx m123.cmx m124.cmx m125.cmx m126.cmx \
   m127.cmx m128.cmx m129.cmx m130.cmx \
 ";

 flags = "-as-parameter";
 module = "p.mli";
 ocamlopt.byte;

 flags = "-as-argument-for P";
 module = "p_int.mli p_int.ml";
 ocamlopt.byte;

 flags = "-parameter P -w -70";
 module = "$ms_ml";
 ocamlopt.byte;

 flags = "-parameter P -w -70";
 module = "big.ml";
 ocamlopt.byte;

 (* Instantiating [Big] applies the functor to all its arguments at once. *)
 flags = "-instantiate";
 module = "";
 program = "big-P_int.cmx";
 all_modules = "big.cmx p_int.cmx";
 ocamlopt.byte;

 (* So does [bind_local_instance], which also lets us run the result without
    instantiating every [Mnnn]. *)
 flags = "-functorize Big";
 module = "";
 program = "bundle.cmx";
 all_modules = "";
 ocamlopt.byte;

 flags = "-w -70";
 module = "test.ml";
 program = "";
 all_modules = "";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "${test_build_directory}/test.exe";
 all_modules = "p_int.cmx $ms_cmx big.cmx bundle.cmx test.cmx";
 ocamlopt.byte;

 run;

 check-program-output;
*)

module Inst = Bundle.Make (P_int) ()

let expected = List.init 130 (fun i -> P_int.v * (i + 1))

let () =
  if Inst.Big.xs = expected
  then print_endline "ok"
  else List.iter (fun x -> Printf.printf "%d\n" x) Inst.Big.xs
