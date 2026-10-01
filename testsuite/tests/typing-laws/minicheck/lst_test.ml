(* TEST
 readonly_files = "lst.mli lst.ml minicheck.ml";
 setup-ocamlopt.opt-build-env;

 (* Compile the library and generate the laws file from its interface. *)
 flags = "-extension laws";
 module = "lst.mli lst.ml";
 ocamlopt.opt;
 flags = "-generate-laws-implementation -o lst_laws.ml";
 module = "lst.cmi";
 ocamlopt.opt;
 flags = "-generate-laws-interface -o lst_laws.mli";
 module = "lst.cmi";
 ocamlopt.opt;

 (* Compile the generated file, a toy backend and the test. *)
 flags = "";
 module = "lst_laws.mli lst_laws.ml minicheck.ml lst_test.ml";
 ocamlopt.opt;

 module = "";
 program = "${test_build_directory}/lst_test.exe";
 all_modules = "lst.cmx lst_laws.cmx minicheck.cmx lst_test.cmx";
 ocamlopt.opt;
 run;
 check-program-output;
*)

(* End-to-end test: the laws file generated from lst.mli is instantiated
   with the choices of the toy backend [Minicheck] and run, with a passing
   set of choices and a failing one. The other generation tests only check
   and compile the generated files (see ../gen_*.ml). *)

open Lst_laws
module L = Instantiate (Minicheck.Choice)

let ints = [ -3; 0; 1; 7 ]
let lists = [ []; [ 1 ]; [ 2; -1 ]; [ 0; 4; 4 ] ]
let show_ints xs = "[" ^ String.concat "; " (List.map string_of_int xs) ^ "]"

module Choices = struct
  let unzip_zip =
    Minicheck.choice
      ~show:(fun (Unzip_zip_input { xs; ys }) -> show_ints xs ^ ", " ^ show_ints ys)
      (List.concat_map
         (fun xs -> List.map (fun ys -> Unzip_zip_input { xs; ys }) lists)
         lists)

  let zip_unzip =
    Minicheck.choice
      (List.map (fun xs -> Zip_unzip_input { zs = List.map (fun x -> x, -x) xs })
         lists)

  let init_length =
    Minicheck.choice
      ~show:(fun (Init_length_input { n; _ }) -> string_of_int n)
      (List.map (fun n -> Init_length_input { n; f = (fun i -> i * 2) }) ints)

  (* [xs : 'a list] is existential in [length_nonneg_input]. *)
  let length_nonneg =
    Minicheck.choice
      (Length_nonneg_input { xs = [ "a" ] }
       :: List.map (fun xs -> Length_nonneg_input { xs }) lists)

  let trivial = Minicheck.choice [ Trivial_input ]
end

let check choices =
  List.iter2
    (fun name law -> Minicheck.check ~name law)
    (to_list names)
    (to_list (L.laws choices))

let () = check (module Choices)

(* A law that does not hold, to see a counterexample. *)
module Bad_choices = struct
  include Choices
  let init_length =
    Minicheck.choice
      ~show:(fun (Init_length_input { n; _ }) -> string_of_int n)
      (List.map
         (fun n ->
            let f i = if i > 3 then failwith "boom" else i in
            Init_length_input { n; f })
         ints)
end

let () =
  Minicheck.check ~name:"init_length"
    (L.laws (module Bad_choices)).init_length
