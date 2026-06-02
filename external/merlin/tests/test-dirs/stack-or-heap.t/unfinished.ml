(* Snippets in an unfinished file *)

type t = { x : int ref }

let f : int ref -> local_ int ref = fun x -> x

let g x =
  let t = { x = f x } in
               (* ^ *)
  let y =
;;

let f : int ref -> int ref = fun x -> x

let g x =
  let t = { x = f x } in
               (* ^ *)
  let y =
;;

let f x =
  let z = Some x in
            (* ^ *)
;;

let f (local_ x) =
  let z = Some x in
            (* ^ *)
;;

let f x =
  (* TODO: This test returns stack or heap, even though
     we can confidently say that [Some y] is allocated
     on the stack. *)
  let z = Some x in
            (* ^ *)
  5
;;

let f x =
  let z = Some y in
            (* ^ *)
  z
;;

let f (local_ x) y =
  let z = [x, Some y] in
                (* ^ *)
;;

let f x y =
  let z = [x, Some y] in
                (* ^ *)
;;

let f (local_ x) y =
  let z = match Some 10 with
    | Some _ -> [x; Some y]
    | None -> [Some y]
                 (* ^ *)
  in
;;

let f x y =
  let z = match Some 10 with
    | Some _ -> [x; Some y]
    | None -> [Some y]
                 (* ^ *)
  in
;;

let f x y =
  let z = match Some 10 with
    | Some _ -> [x; Some y]
    | None -> [Some y]
                 (* ^ *)
  in
  z
;;

let f (local_ x) y =
  let z = in
  [x; Some y]
        (* ^ *)

let f x y =
  let z = in
  exclave_ [x; Some y]
                 (* ^ *)

let _ =
  let f x =
    let z = Some x in
              (* ^ *)
    10
  in
  f 20
;;

let rec is_even x foo bar =
  (* TODO: We can again confidently say that this
     is stack, but we also need the mutually
     recursive function's type to know this *)
  let z = [foo; Some bar] in
                  (* ^ *)
  match x with
  | 0 -> true
  | _ -> not @@ is_odd (x - 1) foo bar

and is_odd x foo bar =
  match x with
  | 0 -> false
  | _ -> not @@ is_even (x - 1) foo bar

let local_function x = 10
let g x = 5 + local_function (Some x)
                                (* ^ *)

let global_function x = x
let g x = global_function (Some x)
                             (* ^ *)

let f x =
  let local_ z = Some x in
                   (* ^ *)
;;
