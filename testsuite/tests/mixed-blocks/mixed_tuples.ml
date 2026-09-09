(* SCRATCH FOR TESTS! *)

(* easy basic tests *)

(* large mixed tuple does not fatal error *)

(* partial match doesn't explode *)

(* GADT narrowing with mixed tuples of different kinds. basically that M7 case *)

(* test that mixed tuples and mixed records mostly behave the same way *)
(* can look at lambda, or can also add to some of the caml_modify tests to make
   sure that we compute a good value kind *)

(* destructing and reconstructing should be the identity function *)
(* which may be a little bit worse when Any is involved (see ryan's PR) *)

(* generalized let* and and* tests with desugaring *)

(* THEN: next step is to turn to some randomized / generated testing *)
