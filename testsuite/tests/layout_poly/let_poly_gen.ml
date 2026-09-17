(* TEST
 flags = "-extension layout_poly_alpha";
 expect.opt;
*)

(** Obviously polymorphic identity functions **)

let poly_ id x = x;;
[%%expect{|
val poly_ id : 'a -> 'a = <lpoly>
|}]

let id x =
  let poly_ id x = x in
  id x;;
[%%expect{|
val poly_ id : 'a -> 'a = <lpoly>
|}]

let id =
  let poly_ id x = x in
  id;;
[%%expect{|
val poly_ id : 'a -> 'a = <lpoly>
|}]

let id =
  let poly_ id x = x in
  fun x -> id x;;
[%%expect{|
val poly_ id : 'a -> 'a = <lpoly>
|}]
