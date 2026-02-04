(* Module D: No stdlib, depends on A, B, C - tests cross-partition from multiple
   modules *)

external ( + ) : int -> int -> int = "%addint"

external ( * ) : int -> int -> int = "%mulint"

(* Reuse primitives from A *)
let ref = Nostdlib_a.ref

let ( ! ) = Nostdlib_a.( ! )

let incr = Nostdlib_a.incr

(* Cross-partition calls to all other modules *)
let call_a_increment () = Nostdlib_a.increment ()

let call_b_increment () = Nostdlib_b.increment ()

let call_c_increment () = Nostdlib_c.increment ()

let call_a_call_all x = Nostdlib_a.call_all x

let call_b_call_all x = Nostdlib_b.call_all x

let call_c_call_all x = Nostdlib_c.call_all x

(* Cross-partition data access *)
let get_a_element i = Nostdlib_a.array_get Nostdlib_a.arr1 i

let get_b_element i = Nostdlib_a.array_get Nostdlib_b.arr1 i

let get_c_element i = Nostdlib_a.array_get Nostdlib_c.arr1 i

(* Large immutable array literal *)
let arr1 =
  [| 300;
     301;
     302;
     303;
     304;
     305;
     306;
     307;
     308;
     309;
     310;
     311;
     312;
     313;
     314;
     315;
     316;
     317;
     318;
     319;
     320;
     321;
     322;
     323;
     324;
     325;
     326;
     327;
     328;
     329;
     330;
     331;
     332;
     333;
     334;
     335;
     336;
     337;
     338;
     339;
     340;
     341;
     342;
     343;
     344;
     345;
     346;
     347;
     348;
     349;
     350;
     351;
     352;
     353;
     354;
     355;
     356;
     357;
     358;
     359;
     360;
     361;
     362;
     363;
     364;
     365;
     366;
     367;
     368;
     369;
     370;
     371;
     372;
     373;
     374;
     375;
     376;
     377;
     378;
     379;
     380;
     381;
     382;
     383;
     384;
     385;
     386;
     387;
     388;
     389;
     390;
     391;
     392;
     393;
     394;
     395;
     396;
     397;
     398;
     399
  |]

(* State *)
let local_counter = ref 0

let increment () =
  incr local_counter;
  !local_counter

(* Many functions *)
let i00 x = x * x

let i01 x = (x * x) + 1

let i02 x = (x * x) + 2

let i03 x = (x * x) + 3

let i04 x = (x * x) + 4

let i05 x = (x * x) + 5

let i06 x = (x * x) + 6

let i07 x = (x * x) + 7

let i08 x = (x * x) + 8

let i09 x = (x * x) + 9

let i10 x = (x * x) + 10

let i11 x = (x * x) + 11

let i12 x = (x * x) + 12

let i13 x = (x * x) + 13

let i14 x = (x * x) + 14

let i15 x = (x * x) + 15

let i16 x = (x * x) + 16

let i17 x = (x * x) + 17

let i18 x = (x * x) + 18

let i19 x = (x * x) + 19

let i20 x = (x * x) + 20

let i21 x = (x * x) + 21

let i22 x = (x * x) + 22

let i23 x = (x * x) + 23

let i24 x = (x * x) + 24

let i25 x = (x * x) + 25

let i26 x = (x * x) + 26

let i27 x = (x * x) + 27

let i28 x = (x * x) + 28

let i29 x = (x * x) + 29

let i30 x = (x * x) + 30

let i31 x = (x * x) + 31

let i32 x = (x * x) + 32

let i33 x = (x * x) + 33

let i34 x = (x * x) + 34

let i35 x = (x * x) + 35

let i36 x = (x * x) + 36

let i37 x = (x * x) + 37

let i38 x = (x * x) + 38

let i39 x = (x * x) + 39

let i40 x = (x * x) + 40

let i41 x = (x * x) + 41

let i42 x = (x * x) + 42

let i43 x = (x * x) + 43

let i44 x = (x * x) + 44

let i45 x = (x * x) + 45

let i46 x = (x * x) + 46

let i47 x = (x * x) + 47

let i48 x = (x * x) + 48

let i49 x = (x * x) + 49

let call_all x =
  i00 x + i01 x + i02 x + i03 x + i04 x + i05 x + i06 x + i07 x + i08 x + i09 x
  + i10 x + i11 x + i12 x + i13 x + i14 x + i15 x + i16 x + i17 x + i18 x
  + i19 x + i20 x + i21 x + i22 x + i23 x + i24 x + i25 x + i26 x + i27 x
  + i28 x + i29 x + i30 x + i31 x + i32 x + i33 x + i34 x + i35 x + i36 x
  + i37 x + i38 x + i39 x + i40 x + i41 x + i42 x + i43 x + i44 x + i45 x
  + i46 x + i47 x + i48 x + i49 x

(* Cross-partition calls to all modules *)
let call_all_modules x =
  call_a_call_all x + call_b_call_all x + call_c_call_all x + call_all x

(* Cross-partition state manipulation *)
let increment_all () =
  let a = Nostdlib_a.increment () in
  let b = Nostdlib_b.increment () in
  let c = Nostdlib_c.increment () in
  incr local_counter;
  a, b, c, !local_counter
