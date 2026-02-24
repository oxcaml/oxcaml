let () = Printf.printf "Fail_a: initializing\n%!"
let _value_a = Fail_b.value_b + 1
let () = Printf.printf "Fail_a: initialized\n%!"
