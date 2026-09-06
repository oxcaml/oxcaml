(* TEST
 { expect; }
 { flags = "-extension mode"; expect; }
 { flags = "-extension mode_beta"; expect; }
*)

module type S = sig val x : int @@ portable end
type r = { x : int @@ portable }
[%%expect{|
module type S = sig val x : int @@ portable end
type r = { x : int @@ portable; }
|}]

type wrapped = (int @@ portable)
[%%expect{|
Line 1, characters 15-32:
1 | type wrapped = (int @@ portable)
                   ^^^^^^^^^^^^^^^^^
Error: The extension "mode" is disabled and cannot be used
|}]

type identity = (int @@ nonportable)
[%%expect{|
Line 1, characters 16-36:
1 | type identity = (int @@ nonportable)
                    ^^^^^^^^^^^^^^^^^^^^
Error: The extension "mode" is disabled and cannot be used
|}]
