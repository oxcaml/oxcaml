let int_of_t (x : Immediate_tags_cmi_good_impl.t) : int = Obj.magic x

let print_t x =
  Printf.printf "%s:%d\n"
    (match x with
    | Immediate_tags_cmi_good_impl.A -> "A"
    | Immediate_tags_cmi_good_impl.B -> "B")
    (int_of_t x)

let () =
  print_t Immediate_tags_cmi_good_impl.A;
  print_t Immediate_tags_cmi_good_impl.B
