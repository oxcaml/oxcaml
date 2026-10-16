let[@inline never] catch_and_discard_exceptions f = try f () with _ -> ()
