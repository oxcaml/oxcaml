(* TEST *)

(* A [let rec]-bound generic array must not be pre-allocated as a scannable
   dummy block: if its elements are floats, [caml_update_dummy] would fill
   it with unboxed floats after the major GC may have marked it as
   scannable. The large live array keeps marking work outstanding, and the
   per-iteration major allocation keeps major cycles turning over. *)

let churn x =
  let a = Array.make 64 x in
  ignore (Sys.opaque_identity a);
  x

let mk (x : 'a) =
  let rec r = [| churn x; churn x; churn x; churn x; churn x; churn x;
                 churn x; churn x; churn x; churn x; churn x; churn x |]
  and get () = r.(0) in
  get
[@@inline never]

let big = Array.init 1_000_000 (fun i -> ref i)

let () =
  for _ = 1 to 200_000 do
    ignore (Sys.opaque_identity (Array.make 1000 0));
    let get = mk (Sys.opaque_identity 64.5) in
    assert (get () = 64.5)
  done;
  ignore (Sys.opaque_identity big)
