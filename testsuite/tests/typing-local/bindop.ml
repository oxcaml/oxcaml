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
