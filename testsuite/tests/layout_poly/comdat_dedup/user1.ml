let pair () = Lpoly_lib.lpoly_pair #1.0 #2.0

let call_between () = Lpoly_lib.lpoly_call_between #1.0 #2.0

let select n m = Lpoly_lib.lpoly_select n m #1.0 #2.0

let try_ s = Lpoly_lib.lpoly_try s #1.0 #2.0

let local n = Lpoly_lib.lpoly_local n #1.0 #2.0

(* The only instance of [lpoly_pair] at this layout. *)
let sole () = Lpoly_lib.lpoly_pair #5.0 #6L
