(* TEST
 flags = "-verbose-types";
 expect;
*)

(* Non-local argument should work *)
let ( let@ ) (with_ @ local once) (f @ local) =
  with_ f [@nontail]
;;
[%%expect{|
val ( let@ ) : ('a @ local -> 'b) @ local once -> 'a @ local -> 'b = <fun>
|}]


(* Local argument should work *)
let ( let@ ) (with_ @ local once) (f : (_ @ local -> _) @ local) =
  with_ f [@nontail]
;;
[%%expect{|
val ( let@ ) :
  ('a : any) ('b : any) 'c.
    (('a @ local -> 'b) @ local -> 'c) @ local once ->
    ('a @ local -> 'b) @ local -> 'c =
  <fun>
|}]

let protectx (a @ unique) ~(finally : _ @ unique -> unit) (f @ local) =
  match f (borrow_ a) with
    | res ->
      finally a;
      res
    | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      finally a;
      Printexc.raise_with_backtrace exn bt
  ;;
[%%expect{|
val protectx :
  'a @ unique ->
  finally:('a @ unique -> unit) -> ('a @ local -> 'b) @ local -> 'b = <fun>
|}]


(* Simple test: operator without once, body arg is local *)
let ( let$ ) (with_ @ local) (f : (_ @ local -> _) @ local) =
  with_ f [@nontail]
;;
[%%expect{|
val ( let$ ) :
  ('a : any) ('b : any) 'c.
    (('a @ local -> 'b) @ local -> 'c) @ local ->
    ('a @ local -> 'b) @ local -> 'c =
  <fun>
|}]

let with_foo (f : (_ @ local -> _) @ local) =
  f "hello"
;;
[%%expect{|
val with_foo : (string @ local -> 'a) @ local -> 'a = <fun>
|}]

(* Simple usage should work: foo is local *)
let () =
  let$ foo = with_foo in
  print_endline foo
[%%expect{|
|}]

(* Original test with once *)
let () =
  let@ foo =
    protectx "foo" ~finally:(fun (x @ unique) -> print_endline (x ^ " is done"))
  in
  print_endline foo
[%%expect{|
|}]

(* and* with local/once args: standard monadic pattern *)
let ( let* ) (x @ local once) (f @ local) = f x [@nontail]
;;
[%%expect{|
val ( let* ) : 'a @ local once -> ('a @ local once -> 'b) @ local -> 'b =
  <fun>
|}]

let ( and* ) (a @ local once) (b @ local once) = exclave_ (a, b)
;;
[%%expect{|
val ( and* ) : 'a @ local once -> 'b @ local once -> 'a * 'b @ local once =
  <fun>
|}]

(* Usage: binding expressions are local *)
let () =
  let* x = local_ (Sys.opaque_identity 1)
  and* y = local_ (Sys.opaque_identity 2) in
  Printf.printf "%d\n" (x + y)
;;
[%%expect{|
|}]
