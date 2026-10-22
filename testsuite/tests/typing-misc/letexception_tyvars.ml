(* TEST
 expect;
*)

(* A [let exception] must not reset the enclosing structure item's scope of
   named type variables: ['tv_stmt] and ['tv_export] below are used both
   before and after the [let exception].  (This is the shape of code that
   Menhir's [--infer] generates.) *)

let (xv_sli, xv_stmt, xv_export) =
  let _ = fun (s : 'tv_stmt) : 'tv_sli -> s in
  let _ = fun (names : 'tv_names) : 'tv_export ->
    let exception Invalid of int in
    (try if names = [] then raise (Invalid 0) else 1 with Invalid n -> n)
  in
  let _ = fun () : 'tv_stmt -> 1, "loc" in
  ((fun () -> assert false) : unit -> 'tv_sli),
  ((fun () -> assert false) : unit -> 'tv_stmt),
  ((fun () -> assert false) : unit -> 'tv_export)
[%%expect{|
val xv_sli : unit -> int * string = <fun>
val xv_stmt : unit -> int * string = <fun>
val xv_export : unit -> int = <fun>
|}]

(* A simpler case of the same thing: both ['a]s are the same variable, so
   annotating [y] makes [x] an [int] too. *)

let f (x : 'a) (y : int) =
  let exception E of int in
  ((y : 'a), x)
[%%expect{|
val f : int -> int -> int * int = <fun>
|}]
