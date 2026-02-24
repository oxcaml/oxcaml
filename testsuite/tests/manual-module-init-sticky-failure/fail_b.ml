let () = Printf.printf "Fail_b: initializing\n%!"
let _dep = Fail_c.initialized
let value_b = 42
let () = Printf.printf "Fail_b: initialized with value_b=%d\n%!" value_b
