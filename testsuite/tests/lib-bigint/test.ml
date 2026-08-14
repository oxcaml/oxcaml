(* TEST
   bytecode;
   native;
*)

(* Bigint is checked against independent oracles: machine [int] arithmetic
   for values in range, and a naive decimal-string implementation for large
   values.  The naive implementation is written for obviousness, not speed:
   numbers are strings of decimal digits, and the algorithms are the
   schoolbook ones, digit by digit. *)

let checks = ref 0

let check description condition =
  incr checks;
  if not condition then failwith ("Bigint check failed: " ^ description)

(* ---- The naive decimal oracle ---------------------------------------- *)

module Oracle = struct
  (* A number is a sign and a string of decimal digits without leading
     zeroes; magnitude "0" always has sign 1. *)
  type t = { negative : bool; digits : string }

  let of_string string =
    let negative = string.[0] = '-' in
    let digits =
      if negative then String.sub string 1 (String.length string - 1)
      else string
    in
    if digits = "0" then { negative = false; digits } else { negative; digits }

  let to_string { negative; digits } =
    if negative then "-" ^ digits else digits

  let digit string index = Char.code string.[index] - Char.code '0'

  let strip_leading_zeroes string =
    let length = String.length string in
    let first = ref 0 in
    while !first < length - 1 && string.[!first] = '0' do incr first done;
    String.sub string !first (length - !first)

  (* Magnitude comparison: longer means larger, otherwise lexicographic. *)
  let compare_digits left right =
    let by_length = compare (String.length left) (String.length right) in
    if by_length <> 0 then by_length else compare left right

  (* Schoolbook addition, right to left with a carry. *)
  let add_digits left right =
    let width = 1 + max (String.length left) (String.length right) in
    let at string index =
      (* index counted from the right *)
      if index < String.length string
      then digit string (String.length string - 1 - index)
      else 0
    in
    let result = Bytes.make width '0' in
    let carry = ref 0 in
    for index = 0 to width - 1 do
      let sum = at left index + at right index + !carry in
      Bytes.set result (width - 1 - index) (Char.chr (Char.code '0' + sum mod 10));
      carry := sum / 10
    done;
    strip_leading_zeroes (Bytes.to_string result)

  (* Schoolbook subtraction, [left] at least [right], with a borrow. *)
  let sub_digits left right =
    let width = String.length left in
    let at string index =
      if index < String.length string
      then digit string (String.length string - 1 - index)
      else 0
    in
    let result = Bytes.make width '0' in
    let borrow = ref 0 in
    for index = 0 to width - 1 do
      let difference = at left index - at right index - !borrow in
      let difference, next_borrow =
        if difference < 0 then difference + 10, 1 else difference, 0
      in
      Bytes.set result (width - 1 - index)
        (Char.chr (Char.code '0' + difference));
      borrow := next_borrow
    done;
    strip_leading_zeroes (Bytes.to_string result)

  (* Schoolbook multiplication: multiply by one digit at a time and add up
     the shifted partial products. *)
  let mul_digits left right =
    let mul_one_digit string d =
      if d = 0 then "0"
      else begin
        let width = String.length string + 1 in
        let result = Bytes.make width '0' in
        let carry = ref 0 in
        for index = 0 to String.length string - 1 do
          let product =
            (digit string (String.length string - 1 - index) * d) + !carry
          in
          Bytes.set result (width - 1 - index)
            (Char.chr (Char.code '0' + product mod 10));
          carry := product / 10
        done;
        Bytes.set result 0 (Char.chr (Char.code '0' + !carry));
        strip_leading_zeroes (Bytes.to_string result)
      end
    in
    let total = ref "0" in
    String.iteri
      (fun index _ ->
        let partial = mul_one_digit left (digit right index) in
        let shift = String.length right - 1 - index in
        let shifted =
          if partial = "0" then partial
          else partial ^ String.make shift '0'
        in
        total := add_digits !total shifted)
      right;
    !total

  let compare left right =
    match left.negative, right.negative with
    | false, true -> 1
    | true, false -> -1
    | false, false -> compare_digits left.digits right.digits
    | true, true -> compare_digits right.digits left.digits

  let neg value =
    if value.digits = "0" then value
    else { value with negative = not value.negative }

  let abs value = { value with negative = false }

  let add left right =
    if left.negative = right.negative
    then { negative = left.negative; digits = add_digits left.digits right.digits }
    else begin
      match compare_digits left.digits right.digits with
      | 0 -> { negative = false; digits = "0" }
      | order when order > 0 ->
        { negative = left.negative; digits = sub_digits left.digits right.digits }
      | _ ->
        { negative = right.negative; digits = sub_digits right.digits left.digits }
    end

  let sub left right = add left (neg right)

  let mul left right =
    let digits = mul_digits left.digits right.digits in
    { negative = left.negative <> right.negative && digits <> "0"; digits }
end

(* ---- Deterministic pseudo-random decimal strings ---------------------- *)

let random_state = ref 1_234_567

let random bound =
  random_state := ((!random_state * 75) + 74) mod 65_537;
  !random_state mod bound

let random_decimal max_digits =
  if random 20 = 0
  then "0"
  else begin
    let digits = 1 + random max_digits in
    let buffer = Buffer.create (digits + 1) in
    if random 2 = 0 then Buffer.add_char buffer '-';
    Buffer.add_char buffer (Char.chr (Char.code '1' + random 9));
    for _ = 2 to digits do
      Buffer.add_char buffer (Char.chr (Char.code '0' + random 10))
    done;
    Buffer.contents buffer
  end

(* ---- Machine-int oracle for values in range --------------------------- *)

(* Boundary values: around zero, around the limb boundaries for half-word
   limbs of any plausible size, and around the int bounds. *)
let interesting_ints =
  let around pivot = [ pivot - 1; pivot; pivot + 1 ] in
  List.concat
    [ around 0
    ; around (1 lsl 15)
    ; around (1 lsl 16)
    ; around (1 lsl 30)
    ; around (1 lsl 31)
    ; List.map (fun n -> -n) (around (1 lsl 30))
    ; List.map (fun n -> -n) (around (1 lsl 31))
    ; [ min_int; min_int + 1; max_int - 1; max_int ]
    ]

let check_machine_int_oracle () =
  let no_overflow_add a b =
    (* a + b representable *)
    if b >= 0 then a <= max_int - b else a >= min_int - b
  in
  List.iter
    (fun a ->
      let big_a = Bigint.of_int a in
      check "of_int round-trip" (Bigint.to_int_opt big_a = Some a);
      check "of_int/of_string agree"
        (Bigint.equal big_a (Bigint.of_string (string_of_int a)));
      check "to_string agrees with string_of_int"
        (String.equal (Bigint.to_string big_a) (string_of_int a));
      if a <> min_int then begin
        check "neg" (Bigint.to_int_opt (Bigint.neg big_a) = Some (-a));
        check "abs" (Bigint.to_int_opt (Bigint.abs big_a) = Some (Stdlib.abs a))
      end;
      List.iter
        (fun b ->
          let big_b = Bigint.of_int b in
          if no_overflow_add a b
          then
            check "add" (Bigint.to_int_opt (Bigint.add big_a big_b) = Some (a + b));
          if no_overflow_add a (-b) && b <> min_int
          then
            check "sub" (Bigint.to_int_opt (Bigint.sub big_a big_b) = Some (a - b));
          (* Small factors only, so the product is representable. *)
          let small n = -(1 lsl 31) < n && n < 1 lsl 31 in
          if small a && small b
          then
            check "mul" (Bigint.to_int_opt (Bigint.mul big_a big_b) = Some (a * b));
          check "compare"
            (Bigint.compare big_a big_b = Stdlib.compare (a : int) b))
        interesting_ints)
    interesting_ints

(* ---- Decimal oracle for large values ---------------------------------- *)

let check_decimal_oracle () =
  let agree description operation oracle_operation left right =
    let via_bigint =
      Bigint.to_string
        (operation (Bigint.of_string left) (Bigint.of_string right))
    in
    let via_oracle =
      Oracle.to_string
        (oracle_operation (Oracle.of_string left) (Oracle.of_string right))
    in
    check description (String.equal via_bigint via_oracle)
  in
  for _ = 1 to 300 do
    let left = random_decimal 60 in
    let right = random_decimal 60 in
    agree "oracle add" Bigint.add Oracle.add left right;
    agree "oracle sub" Bigint.sub Oracle.sub left right;
    agree "oracle mul" Bigint.mul Oracle.mul left right;
    check "oracle compare"
      (Bigint.compare (Bigint.of_string left) (Bigint.of_string right)
       = Oracle.compare (Oracle.of_string left) (Oracle.of_string right))
  done

(* ---- Algebraic properties --------------------------------------------- *)

let check_algebraic_properties () =
  let ( + ) = Bigint.add
  and ( - ) = Bigint.sub
  and ( * ) = Bigint.mul in
  for _ = 1 to 300 do
    let a = Bigint.of_string (random_decimal 40) in
    let b = Bigint.of_string (random_decimal 40) in
    let c = Bigint.of_string (random_decimal 40) in
    check "add commutes" (Bigint.equal (a + b) (b + a));
    check "add associates" (Bigint.equal ((a + b) + c) (a + (b + c)));
    check "mul commutes" (Bigint.equal (a * b) (b * a));
    check "mul associates" (Bigint.equal ((a * b) * c) (a * (b * c)));
    check "mul distributes" (Bigint.equal (a * (b + c)) ((a * b) + (a * c)));
    check "sub is add of neg" (Bigint.equal (a - b) (a + Bigint.neg b));
    check "abs of neg" (Bigint.equal (Bigint.abs (Bigint.neg a)) (Bigint.abs a));
    check "neg of neg" (Bigint.equal (Bigint.neg (Bigint.neg a)) a);
    check "add zero" (Bigint.equal (a + Bigint.zero) a);
    check "mul one" (Bigint.equal (a * Bigint.one) a)
  done

(* ---- Canonicity, observed through polymorphic equality ---------------- *)

let check_canonicity () =
  (* Operations that produce zero must produce the canonical zero. *)
  let a = Bigint.of_string "123456789012345678901234567890" in
  check "sub of equals is canonical zero" (Bigint.sub a a = Bigint.zero);
  check "mul by zero is canonical zero" (Bigint.mul a Bigint.zero = Bigint.zero);
  check "neg of zero is canonical zero" (Bigint.neg Bigint.zero = Bigint.zero);
  check "abs of zero is canonical zero" (Bigint.abs Bigint.zero = Bigint.zero);
  check "of_int zero is canonical zero" (Bigint.of_int 0 = Bigint.zero);
  check "is_zero of computed zero" (Bigint.is_zero (Bigint.sub a a));
  (* Equal values reached by different routes are structurally equal. *)
  for _ = 1 to 300 do
    let x = Bigint.of_string (random_decimal 40) in
    let y = Bigint.of_string (random_decimal 40) in
    check "poly equality agrees with equal"
      ((x = y) = Bigint.equal x y);
    check "add then sub is structurally the argument"
      (Bigint.sub (Bigint.add x y) y = x);
    check "different routes, same structure"
      (Bigint.add x y = Bigint.add y x)
  done

(* ---- Ordering ---------------------------------------------------------- *)

let check_ordering () =
  let bigints =
    List.map Bigint.of_string
      (List.sort_uniq Stdlib.compare
         (List.init 60 (fun _ -> random_decimal 30)))
  in
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          let order = Bigint.compare x y in
          check "compare consistent with equal"
            ((order = 0) = Bigint.equal x y);
          check "compare antisymmetric" (Bigint.compare y x = -order);
          check "lt" (Bigint.lt x y = (order < 0));
          check "le" (Bigint.le x y = (order <= 0));
          check "gt" (Bigint.gt x y = (order > 0));
          check "ge" (Bigint.ge x y = (order >= 0)))
        bigints)
    bigints;
  (* Transitivity on a small subset of triples. *)
  let subset = List.filteri (fun index _ -> index < 12) bigints in
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          List.iter
            (fun z ->
              if Bigint.le x y && Bigint.le y z
              then check "le transitive" (Bigint.le x z))
            subset)
        subset)
    subset

(* ---- Strings ----------------------------------------------------------- *)

let check_strings () =
  let round_trips =
    [ "0"; "1"; "-1"; "10"; "-10"
    ; "2147483647"; "2147483648"; "2147483649"
    ; "4611686018427387903"; "4611686018427387904"
    ; "-4611686018427387904"
    ; "9223372036854775807"; "-9223372036854775808"
    ; "123456789012345678901234567890"
    ; "-999999999999999999999999999999999999"
    ]
  in
  List.iter
    (fun string ->
      check "round-trip"
        (String.equal (Bigint.to_string (Bigint.of_string string)) string))
    round_trips;
  for _ = 1 to 300 do
    let string = random_decimal 200 in
    check "random round-trip"
      (String.equal (Bigint.to_string (Bigint.of_string string)) string)
  done;
  List.iter
    (fun string ->
      check "invalid string rejected"
        (match Bigint.of_string string with
         | _ -> false
         | exception Invalid_argument _ -> true))
    [ ""; "-"; "00"; "01"; "-0"; "-00"; "-01"; "+1"; " 1"; "1 "; "1x"; "--1" ]

(* ---- Machine bounds ---------------------------------------------------- *)

let check_int_bounds () =
  List.iter
    (fun n -> check "in range" (Bigint.to_int_opt (Bigint.of_int n) = Some n))
    [ min_int; min_int + 1; -1; 0; 1; max_int - 1; max_int ];
  check "one past max_int"
    (Bigint.to_int_opt (Bigint.add (Bigint.of_int max_int) Bigint.one) = None);
  check "one past min_int"
    (Bigint.to_int_opt (Bigint.sub (Bigint.of_int min_int) Bigint.one) = None);
  check "far out of range"
    (Bigint.to_int_opt (Bigint.of_string "123456789012345678901234567890")
     = None)

let () =
  check_machine_int_oracle ();
  check_decimal_oracle ();
  check_algebraic_properties ();
  check_canonicity ();
  check_ordering ();
  check_strings ();
  check_int_bounds ();
  Printf.printf "Bigint: %d checks passed\n" !checks
