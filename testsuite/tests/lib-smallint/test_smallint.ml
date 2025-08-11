module Int = Stdlib_beta.Int

module type Smallint = sig
  (** Signed {n}-bit tagged integer values.

    These integers are {n} bits wide and use two's complement representation.
    All operations are taken modulo 2{^n}. They do not fail on overflow. *)

  (** {1:ints n-bit Integers} *)

  (** The type for n-bit integer values. *)
  type t

  (** The number of bits in an integer of type {!t}. *)
  val size : int

  val zero : t

  val one : t

  val minus_one : t

  val neg : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  (** Integer division. This division rounds the real quotient of
      its arguments towards zero, as specified for {!Stdlib.(/)}.
      @raise Division_by_zero if the second argument is zero. *)
  val div : t -> t -> t

  (** Same as {!div}, except that arguments and result are interpreted as {e
      unsigned} integers. *)
  val unsigned_div : t -> t -> t

  (** Integer remainder. If [y] is not zero, [rem x y = sub x (mul (div x y)
      y)]. If [y] is zero, [rem x y] raises [Division_by_zero]. *)
  val rem : t -> t -> t

  (** Same as {!rem}, except that arguments and result are interpreted as {e
      unsigned} integers. *)
  val unsigned_rem : t -> t -> t

  (** [succ x] is [add x 1]. *)
  val succ : t -> t

  (** [pred x] is [sub x 1]. *)
  val pred : t -> t

  (** [abs x] is the absolute value of [x]. That is [x] if [x] is positive and
      [neg x] if [x] is negative. {b Warning.} This may be negative if the
      argument is {!min_int}. *)
  val abs : t -> t

  (** [max_int] is the greatest representable integer,
      [2{^[size - 1]} - 1]. *)
  val max_int : t

  (** [min_int] is the smallest representable integer,
      [-2{^[size - 1]}]. *)
  val min_int : t

  (** Bitwise logical and. *)
  val logand : t -> t -> t

  (** Bitwise logical or. *)
  val logor : t -> t -> t

  (** Bitwise logical exclusive or. *)
  val logxor : t -> t -> t

  (** Bitwise logical negation. *)
  val lognot : t -> t

  (** [shift_left x n] shifts [x] to the left by [n] bits. The result
      is unspecified if [n < 0] or [n >= ]{!size}. *)
  val shift_left : t -> int -> t

  (** [shift_right x n] shifts [x] to the right by [n] bits. This is an
      arithmetic shift: the sign bit of [x] is replicated and inserted
      in the vacated bits. The result is unspecified if [n < 0] or
      [n >=]{!size}. *)
  val shift_right : t -> int -> t

  (** [shift_right x n] shifts [x] to the right by [n] bits. This is a
      logical shift: zeroes are inserted in the vacated bits regardless
      of the sign of [x]. The result is unspecified if [n < 0] or
      [n >=]{!size}. *)
  val shift_right_logical : t -> int -> t

  (** {1:preds Predicates and comparisons} *)

  (** [equal x y] is [true] if and only if [x = y]. *)
  val equal : t -> t -> bool

  (** [compare x y] is {!Stdlib.compare}[ x y] but more efficient. *)
  val compare : t -> t -> int

  (** Same as {!compare}, except that arguments are interpreted as {e unsigned} integers. *)
  val unsigned_compare : t -> t -> int

  (** Return the lesser of the two arguments. *)
  val min : t -> t -> t

  (** Return the greater of the two arguments. *)
  val max : t -> t -> t

  (** {1:convert Converting} *)

  (** [to_int x] is [x] as an {!int}. If [size > Sys.int_size], the topmost
      bits will be lost in the conversion *)
  val to_int : t -> int

  (** [of_int x] truncates the representation of [x] to fit in {!t}. *)
  val of_int : int -> t

  (** Same as {!to_int}, but interprets the argument as an {e unsigned} integer. *)
  val unsigned_to_int : t -> int

  (** [to_float x] is [x] as a floating point number. *)
  val to_float : t -> float

  (** [of_float x] truncates [x] to an integer. The result is
      unspecified if the argument is [nan] or falls outside the range of
      representable integers. *)
  val of_float : float -> t

  (** [to_string x] is the written representation of [x] in decimal. *)
  val to_string : t -> string

  (** Convert the given string to a {!size}-bit integer.
      The string is read in decimal (by default, or if the string
      begins with [0u]) or in hexadecimal, octal or binary if the
      string begins with [0x], [0o] or [0b] respectively.

      The [0u] prefix reads the input as an unsigned integer in the range
      [[0, 2*max_int+1]].  If the input exceeds {!max_int}
      it is converted to the signed integer
      [min_int + input - max_int - 1].

      The [_] (underscore) character can appear anywhere in the string
      and is ignored.
      @raise Failure if the given string is not
      a valid representation of an integer, or if the integer represented
      exceeds the range of integers representable in type [t]. *)
  val of_string : string -> t

  (** A seeded hash function for ints, with the same output value as
      {!Hashtbl.seeded_hash}. This function allows this module to be passed as
      argument to the functor {!Hashtbl.MakeSeeded}. *)
  val seeded_hash : int -> t -> int

  (** An unseeded hash function for ints, with the same output value as
      {!Hashtbl.hash}. This function allows this module to be passed as argument
      to the functor {!Hashtbl.Make}. *)
  val hash : t -> int
end

let same_float x y = Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)

let phys_same x y = Obj.repr x == Obj.repr y

let special_floats = Float.[infinity; nan; neg_infinity; epsilon; -0.; 0.]

(** generates a random float that rounds toward zero to the same integer value *)
let nudge rng f =
  let f_pos = Float.abs f in
  if not (Float.is_finite f)
  then f
  else if f_pos < 1.
  then Random.State.float rng (Float.pred 1.0)
  else
    let lo = Float.floor f_pos in
    let hi = Float.pred (lo +. 1.) in
    if not (lo < hi)
    then f
    else
      (* the mantissa is the low bits, and we are only generating normal
         fractional values so we never need to change the exponent *)
      let lo = Int64.bits_of_float lo in
      let hi = Int64.bits_of_float hi in
      assert (Int64.shift_right lo 52 = Int64.shift_right hi 52);
      Float.copy_sign
        (Int64.float_of_bits (Random.State.int64_in_range rng ~min:lo ~max:hi))
        f

let test_cases ~int_size =
  let rng = Random.State.make [| int_size |] in
  (* sparse test cases, concentrated around 0 and the endpoints *)
  let max_int = (1 lsl (int_size - 1)) - 1 in
  List.init (int_size - 1) (fun size ->
      let bit = 1 lsl size in
      let rand () =
        Random.State.int_in_range rng ~min:(bit lsr 1) ~max:(bit - 1)
      in
      [rand (); lnot (rand ()); max_int - rand (); lnot (max_int - rand ())])
  |> List.concat |> List.sort Int.compare

(** Generate a bunch of valid strings of integer formats *)
let test_strings ~int_size ~f =
  let rec int_to_binary = function
    | 0 -> "0b0"
    | 1 -> "0b1"
    | i -> Printf.sprintf "%s%d" (int_to_binary (i lsr 1)) (i land 1)
  in
  let mask = (1 lsl int_size) - 1 in
  let prefix_formats x ~prefix =
    [ Printf.sprintf "%s0u%u" prefix (x land mask);
      Printf.sprintf "%s0x%x" prefix (x land mask);
      Printf.sprintf "%s0o%o" prefix (x land mask);
      prefix ^ int_to_binary (x land mask) ]
  in
  let arbitrary_wonky_format_that_still_parses = "-0o1___2" in
  List.iter f
    (arbitrary_wonky_format_that_still_parses
    :: ListLabels.concat_map (test_cases ~int_size) ~f:(fun x ->
           [Printf.sprintf "%#d" x; Printf.sprintf "%d" x]
           @ (if x >= 0 then [Printf.sprintf "+%d" x] else [])
           @ prefix_formats x ~prefix:""
           @ prefix_formats x ~prefix:"+"
           @ prefix_formats x ~prefix:"-"))

let run (module Smallint : Smallint) ~min_int ~max_int =
  let int_size = Smallint.size in
  assert (0 < int_size && int_size <= Int.size);
  assert (max_int = (1 lsl (int_size - 1)) - 1);
  assert (min_int = lnot max_int);
  let mask = (1 lsl int_size) - 1 in
  let to_int x : int =
    let i : int = Smallint.to_int x in
    assert (phys_same i x);
    assert (min_int <= i && i <= max_int);
    i
  in
  let of_int (i : int) =
    let x = Smallint.of_int i in
    if not (to_int x land mask = i land mask)
    then
      failwith
        (Printf.sprintf "%x (%x) <> %x (%x)"
           (to_int x land mask)
           (to_int x) (i land mask) i);
    x
  in
  let rng = Random.State.make [| int_size |] in
  let test_cases = test_cases ~int_size in
  let test1 f = ListLabels.iter test_cases ~f in
  let test2 f = test1 (fun x -> test1 (fun y -> f x y)) in
  let test_round_trip () =
    let test hi lo =
      let hi = hi lsl int_size in
      assert (phys_same lo (to_int (of_int (hi lxor lo))))
    in
    test2 (fun hi lo ->
        (* generate test cases with different hi bits *)
        test hi lo;
        test (Random.bits ()) lo)
  in
  let equal_arith x i = phys_same x (of_int i) in
  let equal_logical x i = to_int x == i in
  let assert_equal equal x y =
    let x = try Ok (x ()) with exn -> Error exn in
    let y = try Ok (y ()) with exn -> Error exn in
    match x, y with
    | Ok x, Ok y -> assert (equal x y)
    | Error exn, Error exn' -> assert (exn = exn')
    | Ok _, Error exn | Error exn, Ok _ -> raise exn
  in
  let test_conv1 int16_f int_f ~equal =
    test1 (fun x ->
        assert_equal equal (fun () -> int16_f (of_int x)) (fun () -> int_f x))
  in
  let test_conv2 ?(unsigned = false) int16_f int_f ~equal =
    test2 (fun x y ->
        assert_equal equal
          (fun () -> int16_f (of_int x) (of_int y))
          (fun () ->
            if unsigned then int_f (x land mask) (y land mask) else int_f x y))
  in
  let test_arith1 = test_conv1 ~equal:equal_arith in
  let test_arith2 ?__LINE__ = test_conv2 ~equal:equal_arith in
  let test_logical1 = test_conv1 ~equal:equal_logical in
  let test_logical2 = test_conv2 ~equal:equal_logical in
  let reference_shift_right_logical x i =
    (* we need to ensure that we shift in zero bytes, which is incompatible with
       sign-extension *)
    Int.shift_right_logical (if i > 0 then x land mask else x) i
  in
  test_round_trip ();
  assert (phys_same Smallint.zero Int.zero);
  assert (phys_same Smallint.one Int.one);
  assert (phys_same Smallint.minus_one Int.minus_one);
  assert (phys_same Smallint.max_int max_int);
  assert (phys_same Smallint.min_int min_int);
  assert (phys_same Smallint.(abs min_int) Smallint.min_int);
  assert (phys_same (Smallint.succ Smallint.max_int) Smallint.min_int);
  assert (phys_same (Smallint.pred Smallint.min_int) Smallint.max_int);
  test_arith2 Smallint.add Int.add;
  test_arith2 Smallint.sub Int.sub;
  test_arith2 Smallint.mul Int.mul;
  test_arith2 Smallint.div Int.div;
  test_arith2 Smallint.unsigned_div Int.unsigned_div ~unsigned:true;
  test_arith2 Smallint.unsigned_rem Int.unsigned_rem ~unsigned:true;
  test_arith2 Smallint.rem Int.rem;
  test_arith1 Smallint.succ Int.succ;
  test_arith1 Smallint.pred Int.pred;
  test_arith1 Smallint.abs Int.abs;
  test_arith1 Smallint.neg Int.neg;
  test_logical2 Smallint.logand Int.logand;
  test_logical2 Smallint.logor Int.logor;
  test_logical2 Smallint.logxor Int.logxor;
  test_logical1 Smallint.lognot Int.lognot;
  for shift = 0 to int_size - 1 do
    let apply_shift f x = f x shift in
    test_logical1
      (apply_shift Smallint.shift_right)
      (apply_shift Int.shift_right);
    test_logical1
      (apply_shift Smallint.shift_right_logical)
      (apply_shift reference_shift_right_logical);
    test_conv1
      (apply_shift Smallint.shift_left)
      (apply_shift Int.shift_left)
      ~equal:(if shift = 0 then equal_logical else equal_arith)
  done;
  test_conv2 Smallint.equal Int.equal ~equal:Bool.equal;
  test_conv2 Smallint.compare Int.compare ~equal:Int.equal;
  test_conv2 Smallint.unsigned_compare Int.unsigned_compare ~equal:Int.equal;
  test_conv1 Smallint.to_float Int.to_float ~equal:same_float;
  ListLabels.iter
    (special_floats @ List.map Int.to_float test_cases)
    ~f:(fun f -> assert (equal_arith (Smallint.of_float f) (Int.of_float f)));
  test1 (fun x ->
      (* test that fractional values round toward zero *)
      let f = nudge rng (Int.to_float x) in
      assert (equal_logical (Smallint.of_float f) x));
  test1 (fun x -> assert (Smallint.to_string (of_int x) = Int.to_string x));
  test_strings ~int_size ~f:(fun s ->
      assert (equal_arith (Smallint.of_string s) (int_of_string s)));
  test_logical2 Smallint.min Int.min;
  test_logical2 Smallint.max Int.max;
  ()
