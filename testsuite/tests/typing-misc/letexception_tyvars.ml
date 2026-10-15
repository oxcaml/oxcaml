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

(* The exception's own declaration still typechecks normally, and the
   enclosing named variable is still usable after it. *)

let f (x : 'a) : 'a =
  let exception E of int in
  try raise (E 1) with E n -> ignore n; (x : 'a)
[%%expect{|
val f : 'a -> 'a = <fun>
|}]
