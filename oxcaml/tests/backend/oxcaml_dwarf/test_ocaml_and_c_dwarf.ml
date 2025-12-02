(* Test for breakpoints in C runtime functions. This test allocates memory from
   OCaml and breaks inside C runtime allocation functions to verify backtrace
   and value display when crossing the OCaml/C boundary. *)

let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

(* Test case 1: Simple allocation that triggers caml_alloc_string. *)
let[@inline never] [@local never] f_allocate_string ((len, c) : int * char) =
  let s = String.make len c in
  String.length s

let _ = f_allocate_string (100, 'x')

(* Test case 2: Nested calls with allocation. In the backtrace, we can see how a
   tagged integer displays in an OCaml frame (e.g., x=50 : int @ value) versus
   in a C frame (e.g., init=#101L : value, which is 50*2+1 in tagged form). *)
let[@inline never] [@local never] f_inner (x : int) (y : int) =
  let arr = Array.make x y in
  Array.length arr + y

let[@inline never] [@local never] f_outer (n : int) =
  let result = f_inner n (n * 2) in
  result + 1

let _ = f_outer 50

(* Test case 3: Bytes blit operation *)
let src_bytes = Bytes.make 20 'x'

let dst_bytes = Bytes.make 20 'y'

let[@inline never] [@local never] f_bytes_blit (src : bytes) (dst : bytes)
    (len : int) =
  Bytes.blit src 0 dst 0 len;
  Bytes.length dst

let _ = f_bytes_blit src_bytes dst_bytes 10 [@nontail]

(* Test case 4: GC minor collection *)
let[@inline never] [@local never] f_gc_minor () =
  Gc.minor ();
  42

let _ = f_gc_minor ()

(* Test case 5: Polymorphic hashing via Hashtbl *)
let[@inline never] [@local never] f_hash ((key, value) : string * int) =
  let tbl = Hashtbl.create 10 in
  Hashtbl.add tbl key value;
  Hashtbl.find tbl key

let _ = f_hash ("key", 42)

(* Test case 6: Polymorphic comparison *)
let[@inline never] [@local never] f_compare (a : int array) (b : int array) =
  compare a b

let _ = f_compare [| 1; 2; 3 |] [| 1; 2; 4 |]

(* Test case 7: Marshalling *)
let[@inline never] [@local never] f_marshal () =
  let buf = Bytes.create 64 in
  let _ = Marshal.to_buffer buf 0 64 [1; 2; 3] [] in
  42

let _ = f_marshal ()

(* Test case 8: Unmarshalling *)
let[@inline never] [@local never] f_unmarshal () =
  let data = Marshal.to_bytes [1; 2; 3] [] in
  let (_ : int list) = Marshal.from_bytes data 0 in
  42

let _ = f_unmarshal ()

(* Test case 9: MD5 hashing *)
let[@inline never] [@local never] f_md5 (s : string) =
  let hash = Digest.string s in
  String.length hash

let _ = f_md5 "Hello, World!"

(* Test case 10: Environment variable access *)
let[@inline never] [@local never] f_getenv (var : string) =
  try
    let _ = Sys.getenv var in
    42
  with Not_found -> 0

let _ = f_getenv "PATH"

(* Test case 11: Bigarray creation *)
let[@inline never] [@local never] f_bigarray () =
  let arr = Bigarray.Array1.create Bigarray.Float64 Bigarray.C_layout 100 in
  Bigarray.Array1.set arr 0 3.14;
  int_of_float (Bigarray.Array1.get arr 0)

let _ = f_bigarray ()

(* Test case 12: Large tuple allocation - 500 components exceeds
   Max_young_wosize (256 words), so this should go through caml_alloc_shr for
   shared heap allocation. *)
let[@inline never] [@local never] f_large_tuple (n : int) =
  Sys.opaque_identity
    ( n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n,
      n )

let _ = ignore (f_large_tuple 42)

(* Test case 13: C-to-OCaml callback via Sys.with_async_exns. The stack trace
   will show OCaml -> C -> OCaml. *)
let[@inline never] [@local never] f_callback_inner () = 42

let[@inline never] [@local never] f_callback () =
  Sys.with_async_exns f_callback_inner

let _ = f_callback ()

(* Test case 14: Finalization callback - C calls OCaml during GC. The stack
   trace will show OCaml finalizer called from C GC code. *)
let[@inline never] [@local never] f_my_finalizer (_ : int ref) = ()

let[@inline never] [@local never] f_trigger_finalizer (value : int) =
  let r = ref value in
  Gc.finalise f_my_finalizer r;
  (* Make r unreachable by not returning it, then force GC *)
  ignore (Sys.opaque_identity r);
  Gc.full_major ();
  Gc.full_major ();
  value

let _ = f_trigger_finalizer 42
