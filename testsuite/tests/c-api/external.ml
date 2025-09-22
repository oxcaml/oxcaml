(* TEST
 modules = "external_stubs.c external_stubs.js";
 native;
 javascript;
 *)

type data = A | B | C | D | E | F

external test_int : (int [@untagged])
                    -> (char [@untagged]) -> (data [@untagged])
                    -> (int [@untagged]) = "test" "test" [@@noalloc]

external test_char : (int [@untagged])
                    -> (char [@untagged]) -> (data [@untagged])
                    -> (char [@untagged]) = "test" "test" [@@noalloc]

external test_data : (int [@untagged])
                    -> (char [@untagged]) -> (data [@untagged])
                    -> (data [@untagged]) = "test" "test" [@@noalloc]

let _ = assert(test_int 1 '\001' B = 3)
let _ = assert(test_char 1 '\001' B = '\003')
let _ = assert(test_data 1 '\001' B = D)
