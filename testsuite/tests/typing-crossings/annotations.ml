(* TEST
 expect;
*)

let not_mod_contended () =
  let x mod contended = [ ref 42 ] in ()
[%%expect {|
Line 2, characters 26-32:
2 |   let x mod contended = [ ref 42 ] in ()
                              ^^^^^^
Error: This is "mod uncontended" but it is expected to be "mod contended".
|}]

let is_mod_contended () =
  let _ : int array mod contended = [| |] in ()
[%%expect {|
val is_mod_contended : unit -> unit = <fun>
|}]

let annotation_is_upper () =
  let x : int array mod uncontended = [| |] in
  let _ @ portable = fun () -> (x : _ @ uncontended) in
  ()
[%%expect {|
Line 3, characters 32-33:
3 |   let _ @ portable = fun () -> (x : _ @ uncontended) in
                                    ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let nested () =
  let _ mod contended = [ ([] : int ref list mod contended); [ ref 42 ]; ([] : _ mod contended) ] in
  ()
[%%expect {|
Line 2, characters 63-69:
2 |   let _ mod contended = [ ([] : int ref list mod contended); [ ref 42 ]; ([] : _ mod contended) ] in
                                                                   ^^^^^^
Error: This is "mod uncontended" but it is expected to be "mod contended".
|}]
