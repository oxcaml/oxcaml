(* Test atomics and prefetch intrinsics on both amd64 and arm64 *)

(* Atomic operations on immediate integers *)

let r = Atomic.make 1

let () = assert (Atomic.get r = 1)

let () = Atomic.set r 2

let () = assert (Atomic.get r = 2)

let () = assert (Atomic.exchange r 3 = 2)

let () = assert (Atomic.compare_and_set r 3 4 = true)

let () = assert (Atomic.get r = 4)

let () = assert (Atomic.compare_and_set r 3 (-4) = false)

let () = assert (Atomic.get r = 4)

let () = assert (Atomic.compare_and_set r 3 4 = false)

let () = assert (Atomic.fetch_and_add r 2 = 4)

let () = assert (Atomic.get r = 6)

let () = assert (Atomic.fetch_and_add r (-2) = 6)

let () = assert (Atomic.get r = 4)

let () =
  assert (
    (Atomic.incr r;
     Atomic.get r)
    = 5)

let () =
  assert (
    (Atomic.decr r;
     Atomic.get r)
    = 4)

let () =
  assert (
    (Atomic.add r 3;
     Atomic.get r)
    = 7)

let () =
  assert (
    (Atomic.sub r 3;
     Atomic.get r)
    = 4)

let () =
  assert (
    (Atomic.logand r 2;
     Atomic.get r)
    = 0)

let () =
  assert (
    (Atomic.logor r 2;
     Atomic.get r)
    = 2)

let () =
  assert (
    (Atomic.logxor r 3;
     Atomic.get r)
    = 1)

(* Test compare_exchange *)

let () =
  Atomic.set r 10;
  let old = Atomic.compare_exchange r 10 20 in
  assert (old = 10);
  assert (Atomic.get r = 20)

let () =
  let old = Atomic.compare_exchange r 999 30 in
  assert (old = 20);
  assert (Atomic.get r = 20)

(* Test atomics with non-immediate types *)

let a = ref 1

let r2 = Atomic.make a

let () = assert (Atomic.get r2 == a)

let b = ref 2

let () = Atomic.set r2 b

let () = assert (Atomic.get r2 == b)

let c = ref 3

let () = assert (Atomic.exchange r2 c == b)

let d = ref 4

let () = assert (Atomic.compare_and_set r2 c d = true)

let () = assert (Atomic.get r2 == d)

let e = ref (-4)

let () = assert (Atomic.compare_and_set r2 c e = false)

let () = assert (Atomic.get r2 == d)

let () = assert (Atomic.compare_and_set r2 c d = false)

(* Test compare_exchange with non-immediate types *)

let () =
  let old = Atomic.compare_exchange r2 d e in
  assert (old == d);
  assert (Atomic.get r2 == e)

let () =
  let old = Atomic.compare_exchange r2 d (ref 0) in
  assert (old == e);
  assert (Atomic.get r2 == e)

(* Prefetch intrinsics - just verify they don't crash *)

external prefetch_read_high
  : 'a -> unit
  = "caml_obj_tag" "caml_prefetch_read_high"
  [@@noalloc] [@@builtin]

external prefetch_read_moderate
  : 'a -> unit
  = "caml_obj_tag" "caml_prefetch_read_moderate"
  [@@noalloc] [@@builtin]

external prefetch_read_low
  : 'a -> unit
  = "caml_obj_tag" "caml_prefetch_read_low"
  [@@noalloc] [@@builtin]

external prefetch_read_none
  : 'a -> unit
  = "caml_obj_tag" "caml_prefetch_read_none"
  [@@noalloc] [@@builtin]

external prefetch_write_high
  : 'a -> unit
  = "caml_obj_tag" "caml_prefetch_write_high"
  [@@noalloc] [@@builtin]

external prefetch_write_moderate
  : 'a -> unit
  = "caml_obj_tag" "caml_prefetch_write_moderate"
  [@@noalloc] [@@builtin]

external prefetch_write_low
  : 'a -> unit
  = "caml_obj_tag" "caml_prefetch_write_low"
  [@@noalloc] [@@builtin]

external prefetch_write_none
  : 'a -> unit
  = "caml_obj_tag" "caml_prefetch_write_none"
  [@@noalloc] [@@builtin]

let data = Array.make 1024 0

let () =
  prefetch_read_high data;
  prefetch_read_moderate data;
  prefetch_read_low data;
  prefetch_read_none data;
  prefetch_write_high data;
  prefetch_write_moderate data;
  prefetch_write_low data;
  prefetch_write_none data
