(* TEST
 expect;
*)

(* Typing for recursive modules checks scope escape *)
module type S = sig
  type t
end;;

let rec (module M : S) =
  (module struct
    type t = M.t
  end : S
    with type t = M.t)
in
();;
[%%expect{|
module type S = sig type t end
Lines 6-9, characters 2-22:
6 | ..(module struct
7 |     type t = M.t
8 |   end : S
9 |     with type t = M.t)
Error: This expression has type "(module S with type t = M.t)"
       but an expression was expected of type "(module S)"
|}];;

let rec k =
  let (module K : S with type t = A.t) = k in
  (module struct
    type t = K.t
  end : S
    with type t = K.t)
and (module A : S) =
  (module struct
    type t = unit

    let x = ()
  end)
in
();;
[%%expect{|
Line 7, characters 12-13:
7 | and (module A : S) =
                ^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

(* The locally abstract type lets us check the module's type
   without scope escape. *)
let f (type a) () =
  let rec (module M : S with type t = a) =
    (module struct
      type t = M.t
    end : S with type t = M.t)
  in
  ()
;;
[%%expect{|
Line 2, characters 18-19:
2 |   let rec (module M : S with type t = a) =
                      ^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

let f (type a) () =
  let rec (module M : S with type t = a) =
    (module struct
      type t = M.t
    end : S with type t = a)
  in
  ();;
[%%expect{|
Line 2, characters 18-19:
2 |   let rec (module M : S with type t = a) =
                      ^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

(* Reject scope escape via unification *)

module type S = sig
  type t
  val x : t
end;;

let f () =
  let (module M : S) =
    (module struct
      type t = unit

      let x = ()
    end)
  in
  let unify x = if true then M.x else x in
  unify ()
;;
[%%expect{|
module type S = sig type t val x : t end
Line 15, characters 8-10:
15 |   unify ()
             ^^
Error: The constructor "()" has type "unit"
       but an expression was expected of type "M.t"
|}];;
