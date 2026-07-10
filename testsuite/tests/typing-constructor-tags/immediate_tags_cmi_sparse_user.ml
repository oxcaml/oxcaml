(* A client reads the sparse/negative constructor tags from the [.cmi] and both
   dispatches on them and observes them through polymorphic [compare]. *)
open Immediate_tags_cmi_sparse_impl

let int_of_t (x : t) : int = Obj.magic x

let name = function A -> "A" | B -> "B" | C -> "C"

let () =
  List.iter (fun x -> Printf.printf "%s:%d\n" (name x) (int_of_t x)) [ A; B; C ];
  Printf.printf "compare A B:%d\n" (compare A B);
  Printf.printf "sort:";
  List.iter
    (fun x -> Printf.printf " %s" (name x))
    (List.sort compare [ A; B; C ]);
  print_newline ()
