let[@inline never] catch_and_discard_exceptions f = try f () with _ -> ()

let[@inline never] either f g x b = if b then f x else g x
