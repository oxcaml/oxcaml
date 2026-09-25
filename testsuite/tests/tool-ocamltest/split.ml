(* TEST
 set arm = "unset";
 {
   split [
   | arm = "a"; set in_arm = "x";
     split [
     | set inner = "1";
     | set inner = "2";
     ]
   | arm = "b"; set in_arm = "y"; set inner = "0";
   | arm = "c"; set in_arm = "z"; set inner = "0";
   ]
   set after_split = "${arm}${in_arm}${inner}";
   script = "sh ${test_source_directory}/split-accum.sh";
   script;
 }
 {
   in_arm = "w";
   inner = "3";
   after_split = "done";
   script = "sh ${test_source_directory}/split-check.sh";
   script;
 }
*)

(* This file tests the "split" syntax of ocamltest. The first block should run once per
   alternative (twice for arm "a", which splits again), each run appending the values of
   several variables to a file in the test build directory; the second block checks that
   every alternative actually ran, in order. The [set] statements check that variable
   registrations are scoped to each branch: [in_arm] (declared in each arm),
   [after_split] (declared once per branch by the statements following the split), and
   [inner] (declared in each arm of the nested split, and again in arms "b" and "c" of
   the outer one) would otherwise collide as soon as the second branch runs. The plain
   assignments in the second block check that the registrations nonetheless remain in
   effect after the split, as they would if there were no [split]: assigning to an
   undeclared variable is an error. *)
