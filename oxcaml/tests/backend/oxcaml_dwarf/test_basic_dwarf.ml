let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

let[@inline never] [@local never] f_unit (x : unit) = x

let _ = f_unit ()

let[@inline never] [@local never] f_bool (x : bool) = x

let _ = f_bool true

let _ = f_bool false

(* CR sspies: Integer signedness issue in DWARF output - negative integers are
   displayed as large unsigned values instead of their proper signed values. For
   example, (-42) shows as 9223372036854775766 and min_int shows as a large
   positive number instead of a negative value. This makes debugging confusing
   when working with negative integers. *)
let[@inline never] [@local never] f_int (x : int) = x

let _ = f_int 0

let _ = f_int 123

let _ = f_int (-42)

let _ = f_int max_int

let _ = f_int min_int

let[@inline never] [@local never] f_char (x : char) = x

let _ = f_char 'a'

let _ = f_char 'Z'

let _ = f_char '0'

let _ = f_char ' '

let _ = f_char '\n'

let _ = f_char '\255'

let[@inline never] [@local never] f_float (x : float) = x

let _ = f_float 0.0

let _ = f_float 4.1

let _ = f_float (-3.14)

let _ = f_float (0.1 +. 0.2)

let _ = f_float 0.3

let _ = f_float 1e10

let _ = f_float (-1e-10)

let _ = f_float infinity

let _ = f_float neg_infinity

let _ = f_float nan

let[@inline never] [@local never] f_float32 (x : float32) = x

let _ = f_float32 0.0s

let _ = f_float32 4.1s

let _ = f_float32 (-2.5s)

let _ = f_float32 1e5s

let[@inline never] [@local never] f_int32 (x : int32) = x

let _ = f_int32 0l

let _ = f_int32 42l

let _ = f_int32 (-123l)

let _ = f_int32 Int32.max_int

let _ = f_int32 Int32.min_int

let[@inline never] [@local never] f_int64 (x : int64) = x

let _ = f_int64 0L

let _ = f_int64 1000L

let _ = f_int64 (-5000L)

let _ = f_int64 Int64.max_int

let _ = f_int64 Int64.min_int

let[@inline never] [@local never] f_nativeint (x : nativeint) = x

let _ = f_nativeint 0n

let _ = f_nativeint 999n

let _ = f_nativeint (-777n)

let _ = f_nativeint Nativeint.max_int

let _ = f_nativeint Nativeint.min_int

let[@inline never] [@local never] f_string (x : string) = x

let _ = f_string ""

let _ = f_string "hello world!"

let _ = f_string "a"

let _ = f_string "special chars: \n\t\""
(* CR sspies: debugger shows the newline as an actual line break in output *)

let _ = f_string "unicode: αβγ"

let[@inline never] [@local never] f_bytes (x : bytes) = x

let _ = f_bytes (Bytes.create 0)

let _ = f_bytes (Bytes.of_string "abc")

let _ = f_bytes (Bytes.of_string "single")

let _ = f_bytes (Bytes.of_string "\000\001\255")

let[@inline never] [@local never] f_array (x : char array) = x

let _ = f_array [||]

let _ = f_array [| 'a' |]

let _ = f_array [| 'a'; 'b'; 'c' |]

let _ = f_array [| 'x'; 'y'; 'z'; '1'; '2'; '3' |]

let[@inline never] [@local never] f_list (x : char list) = x

let _ = f_list []

let _ = f_list ['x']

let _ = f_list ['a'; 'b'; 'c']
(* CR sspies: lists display as low-level cons cell structure rather than
   user-friendly list syntax *)

let _ = f_list ['1'; '2'; '3'; '4'; '5']

let[@inline never] [@local never] f_option (x : char option) = x

let _ = f_option None

let _ = f_option (Some 'a')

let _ = f_option (Some 'Z')

let _ = f_option (Some '\000')

let[@inline never] [@local never] f_lazy (x : char lazy_t) = x

let _ = f_lazy (lazy 'a')
(* CR sspies: lazy values show their contents as ASCII numeric codes rather than
   character representations *)

let _ = f_lazy (lazy 'X')

let _ = f_lazy (lazy '9')

let[@inline never] [@local never] f_exn (x : exn) = x

let _ = f_exn (Failure "test")

let _ = f_exn (Invalid_argument "bad arg")

let _ = f_exn Not_found

let _ = f_exn (Sys_error "io error")

let[@inline never] [@local never] f_float_array (x : float array) = x

let _ = f_float_array [||]

let _ = f_float_array [| 1.0 |]

let _ = f_float_array [| 4.1; 4.2; 4.3; 4.4; 4.5 |]

let _ =
  f_float_array
    [| 4.1;
       4.2;
       4.3;
       4.4;
       4.5;
       4.6;
       4.7;
       4.8;
       4.9;
       5.0;
       5.1;
       5.2;
       5.3;
       5.4;
       5.5
    |]

let _ = f_float_array [| 0.0; -1.5; infinity; nan |]

let[@inline never] [@local never] f_poly (x : 'a) = x

let _ = f_poly ()

let _ = f_poly true

let _ = f_poly false

let _ = f_poly (-123)

let _ = f_poly 'z'

let _ = f_poly 3.14

let _ = f_poly (0.1 +. 0.2)

let _ = f_poly 0.3

let _ = f_poly 2.7s

let _ = f_poly (-456l)

let _ = f_poly (-789L)

let _ = f_poly (-999n)

let _ = f_poly "polymorphic"

let _ = f_poly (Bytes.of_string "poly")

let _ = f_poly [| 'p'; 'o'; 'l'; 'y' |]
(* CR sspies: char arrays in polymorphic context show as ASCII codes instead of
   characters *)

let _ = f_poly ['p'; 'o'; 'l'; 'y']

let _ = f_poly (Some 'p')

let _ = f_poly (lazy 'p')

let _ = f_poly (Failure "poly error")

let _ = f_poly [| 1.0; 2.0; 3.0 |]

let _ =
  f_poly
    [| 4.1;
       4.2;
       4.3;
       4.4;
       4.5;
       4.6;
       4.7;
       4.8;
       4.9;
       5.0;
       5.1;
       5.2;
       5.3;
       5.4;
       5.5
    |]
