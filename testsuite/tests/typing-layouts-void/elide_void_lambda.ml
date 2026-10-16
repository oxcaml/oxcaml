(* TEST
 flags = "-dlambda -dno-unique-ids -dno-locations";
 expect.opt;
*)

(* Test interaction of [[@elide_void]] with specialization. This is easiest to
   observe by dumping short lambda snippets (the effect on behavior is sometimes
   subtle and internal to e.g. GC state).

   Native-only since bytecode doesn't support [%peek].
*)

type w : void
external w : unit -> w = "%unbox_unit"
[%%expect{|
(makeblock 0)
type w : void
(makeblock 0)
external w : unit -> w = "%unbox_unit"
|}]

(* The [int] result is past the trailing witness, but we still expect to see
   [field_int] rather than [field_mut]. *)
external field :
  (w [@elide_void]) -> 'a ref -> (w [@elide_void]) -> 'a = "%field0"

(* Overapplying [load] must recognize the result type as an arrow (and not
   [int]). So we expect to see [atomic_load_field_ptr] and not
   [atomic_load_field_imm]. *)
external load :
  (w [@elide_void]) -> 'a Atomic.t -> (w [@elide_void]) -> 'a
  = "%atomic_load"

let f r a = field (w ()) r (w ()) + load (w ()) a (w ()) 1

[%%expect{|
(makeblock 0)
external field : (w [@elide_void]) -> 'a ref -> (w [@elide_void]) -> 'a
  = "%field0"
(makeblock 0)
external load : (w [@elide_void]) -> 'a Atomic.t -> (w [@elide_void]) -> 'a
  = "%atomic_load"
(let
  (f =
     (function {nlocal = 0} r a : int
       (%int_add (seq (unbox_unit 0) (unbox_unit 0) (field_int 0 r))
         (apply
           (seq (unbox_unit 0) (unbox_unit 0) (atomic_load_field_ptr a 0)) 1))))
  (makeblock 0 f))
val f : int ref -> (int -> int) Atomic.t -> int = <fun>
|}]

(* [set] specializes on the mode of the actual first argument, excluding leading
   elided arguments. We expect to see [setfield_ptr(maybe-stack)] (because we
   specialize on the [local_ 'a ref]) and not  [setfield_ptr] (which we would
   see if we incorrectly specialized based on the mode of [w]). *)
external set : (w [@elide_void]) -> local_ 'a ref -> 'a -> unit
  = "%setfield0"

(* As above; [%arrayblit] specializes based on the destination return mode.
   Here, this is the third non-elided argument ([local_ 'a array]) and not the
   third argument (a global elided [w]). *)
external blit :
  ('a : value non_float).
  'a array -> int -> (w [@elide_void]) -> local_ 'a array -> int -> int
  -> unit
  = "%arrayblit"

let set_local (local_ r) x = set (w ()) r x
let blit_local a (local_ b) = blit a 0 (w ()) b 0 1

[%%expect{|
(makeblock 0)
external set : (w [@elide_void]) -> 'a ref @ local -> 'a -> unit
  = "%setfield0"
(makeblock 0)
external blit :
  ('a : value non_float).
    'a array ->
    int -> (w [@elide_void]) -> 'a array @ local -> int -> int -> unit
  = "%arrayblit"
(let
  (set_local =
     (function {nlocal = 2} r[L] x : int
       (seq (unbox_unit 0) (setfield_ptr(maybe-stack) 0 r x))))
  (makeblock 0 set_local))
val set_local : 'a ref @ local -> 'a -> unit = <fun>
(let
  (blit_local =
     (function {nlocal = 1} a[value<addrarray>] b[L][value<addrarray>] : int
       (let (let =[value<int>] 1 let =[value<int>] 0)
         (seq (unbox_unit 0)
           (let (let =[value<int>] 0)
             (arrayblit[array -> addr(local)] a let b let let))))))
  (makeblock 0 blit_local))
val blit_local : ('a : value non_float). 'a array -> 'a array @ local -> unit =
  <fun>
|}]

(* Specialize [%peek] based on the true result type ([int64_u]) and not the
   e.g. the arrow [w -> int64_u]. *)

external peek :
  (w [@elide_void]) -> nativeint_u -> (w [@elide_void]) -> int64_u
  = "%peek"

let peek_int64 p = peek (w ()) p (w ())

[%%expect{|
(makeblock 0)
external peek :
  (w [@elide_void]) -> nativeint_u -> (w [@elide_void]) -> int64_u = "%peek"
(let
  (peek_int64 =
     (function {nlocal = 0} p[nativeint] : unboxed_int64
       (seq (unbox_unit 0) (unbox_unit 0) ((peek unboxed_int64) p))))
  (makeblock 0 peek_int64))
val peek_int64 : nativeint_u -> int64_u = <fun>
|}]
