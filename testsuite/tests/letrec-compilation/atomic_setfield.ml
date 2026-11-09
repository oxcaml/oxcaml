(* TEST *)

type t = { mutable x : int [@atomic] }

let t = { x = 1 }

let rec r = (t.x <- 2)

let () = assert (t.x = 2)

type m =
  { mutable y : int [@atomic];
    z : float#
  }

let m = { y = 1; z = #3.14 }

let rec s = (m.y <- 2)

let () = assert (m.y = 2)

(* Extensible constructors have an extra field before the record fields. *)
type node = ..
type node += Node of
  { mutable next : node [@atomic];
    mutable count : int [@atomic]
  }

let rec node = Node { next = node; count = initial }
and initial = 42

let () =
  match node with
  | Node r ->
    assert (r.next == node && r.count = 42);
    let child = Node { next = node; count = 0 } in
    r.next <- child;
    r.count <- 43;
    (match node with
     | Node r -> assert (r.next == child && r.count = 43)
     | _ -> assert false)
  | _ -> assert false

let () = print_endline "ok"
