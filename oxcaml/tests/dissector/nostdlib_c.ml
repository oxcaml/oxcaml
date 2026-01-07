(* Module C: No stdlib, depends on A and B - tests longer cross-partition
   chains *)

external ( + ) : int -> int -> int = "%addint"

external ( * ) : int -> int -> int = "%mulint"

(* Reuse primitives from A *)
let ref = Nostdlib_a.ref

let ( ! ) = Nostdlib_a.( ! )

let incr = Nostdlib_a.incr

(* Cross-partition calls to A *)
let call_a_increment () = Nostdlib_a.increment ()

let call_a_call_all x = Nostdlib_a.call_all x

(* Cross-partition calls to B *)
let call_b_increment () = Nostdlib_b.increment ()

let call_b_call_all x = Nostdlib_b.call_all x

(* Cross-partition data access *)
let get_a_element i = Nostdlib_a.array_get Nostdlib_a.arr1 i

let get_b_element i = Nostdlib_a.array_get Nostdlib_b.arr1 i

(* Large immutable array literal *)
let arr1 =
  [| 200;
     201;
     202;
     203;
     204;
     205;
     206;
     207;
     208;
     209;
     210;
     211;
     212;
     213;
     214;
     215;
     216;
     217;
     218;
     219;
     220;
     221;
     222;
     223;
     224;
     225;
     226;
     227;
     228;
     229;
     230;
     231;
     232;
     233;
     234;
     235;
     236;
     237;
     238;
     239;
     240;
     241;
     242;
     243;
     244;
     245;
     246;
     247;
     248;
     249;
     250;
     251;
     252;
     253;
     254;
     255;
     256;
     257;
     258;
     259;
     260;
     261;
     262;
     263;
     264;
     265;
     266;
     267;
     268;
     269;
     270;
     271;
     272;
     273;
     274;
     275;
     276;
     277;
     278;
     279;
     280;
     281;
     282;
     283;
     284;
     285;
     286;
     287;
     288;
     289;
     290;
     291;
     292;
     293;
     294;
     295;
     296;
     297;
     298;
     299
  |]

(* State *)
let local_counter = ref 0

let increment () =
  incr local_counter;
  !local_counter

(* Many functions *)
let h00 x = x + x

let h01 x = x + x + 1

let h02 x = x + x + 2

let h03 x = x + x + 3

let h04 x = x + x + 4

let h05 x = x + x + 5

let h06 x = x + x + 6

let h07 x = x + x + 7

let h08 x = x + x + 8

let h09 x = x + x + 9

let h10 x = x + x + 10

let h11 x = x + x + 11

let h12 x = x + x + 12

let h13 x = x + x + 13

let h14 x = x + x + 14

let h15 x = x + x + 15

let h16 x = x + x + 16

let h17 x = x + x + 17

let h18 x = x + x + 18

let h19 x = x + x + 19

let h20 x = x + x + 20

let h21 x = x + x + 21

let h22 x = x + x + 22

let h23 x = x + x + 23

let h24 x = x + x + 24

let h25 x = x + x + 25

let h26 x = x + x + 26

let h27 x = x + x + 27

let h28 x = x + x + 28

let h29 x = x + x + 29

let h30 x = x + x + 30

let h31 x = x + x + 31

let h32 x = x + x + 32

let h33 x = x + x + 33

let h34 x = x + x + 34

let h35 x = x + x + 35

let h36 x = x + x + 36

let h37 x = x + x + 37

let h38 x = x + x + 38

let h39 x = x + x + 39

let h40 x = x + x + 40

let h41 x = x + x + 41

let h42 x = x + x + 42

let h43 x = x + x + 43

let h44 x = x + x + 44

let h45 x = x + x + 45

let h46 x = x + x + 46

let h47 x = x + x + 47

let h48 x = x + x + 48

let h49 x = x + x + 49

let call_all x =
  h00 x + h01 x + h02 x + h03 x + h04 x + h05 x + h06 x + h07 x + h08 x + h09 x
  + h10 x + h11 x + h12 x + h13 x + h14 x + h15 x + h16 x + h17 x + h18 x
  + h19 x + h20 x + h21 x + h22 x + h23 x + h24 x + h25 x + h26 x + h27 x
  + h28 x + h29 x + h30 x + h31 x + h32 x + h33 x + h34 x + h35 x + h36 x
  + h37 x + h38 x + h39 x + h40 x + h41 x + h42 x + h43 x + h44 x + h45 x
  + h46 x + h47 x + h48 x + h49 x

(* Cross-partition chain: C -> B -> A *)
let chain_call x =
  let a = Nostdlib_a.call_all x in
  let b = Nostdlib_b.call_all x in
  a + b + call_all x
