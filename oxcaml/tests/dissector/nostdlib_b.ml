(* Module B: No stdlib, depends on A - tests cross-partition OCaml calls *)

external ( + ) : int -> int -> int = "%addint"

external ( * ) : int -> int -> int = "%mulint"

(* Reuse primitives from A *)
let ref = Nostdlib_a.ref

let ( ! ) = Nostdlib_a.( ! )

let incr = Nostdlib_a.incr

(* Cross-partition calls to A - these should go through IGOT/IPLT *)
let call_a_increment () = Nostdlib_a.increment ()

let call_a_magic () = Nostdlib_a.get_magic ()

let call_a_functions x = Nostdlib_a.call_all x

(* Cross-partition data access *)
let get_a_arr_element i = Nostdlib_a.array_get Nostdlib_a.arr1 i

(* Large immutable array literal *)
let arr1 =
  [| 100;
     101;
     102;
     103;
     104;
     105;
     106;
     107;
     108;
     109;
     110;
     111;
     112;
     113;
     114;
     115;
     116;
     117;
     118;
     119;
     120;
     121;
     122;
     123;
     124;
     125;
     126;
     127;
     128;
     129;
     130;
     131;
     132;
     133;
     134;
     135;
     136;
     137;
     138;
     139;
     140;
     141;
     142;
     143;
     144;
     145;
     146;
     147;
     148;
     149;
     150;
     151;
     152;
     153;
     154;
     155;
     156;
     157;
     158;
     159;
     160;
     161;
     162;
     163;
     164;
     165;
     166;
     167;
     168;
     169;
     170;
     171;
     172;
     173;
     174;
     175;
     176;
     177;
     178;
     179;
     180;
     181;
     182;
     183;
     184;
     185;
     186;
     187;
     188;
     189;
     190;
     191;
     192;
     193;
     194;
     195;
     196;
     197;
     198;
     199
  |]

(* State *)
let local_counter = ref 0

let increment () =
  incr local_counter;
  !local_counter

(* Many functions *)
let g00 x = x * 2

let g01 x = x * 3

let g02 x = x * 4

let g03 x = x * 5

let g04 x = x * 6

let g05 x = x * 7

let g06 x = x * 8

let g07 x = x * 9

let g08 x = x * 10

let g09 x = x * 11

let g10 x = x * 12

let g11 x = x * 13

let g12 x = x * 14

let g13 x = x * 15

let g14 x = x * 16

let g15 x = x * 17

let g16 x = x * 18

let g17 x = x * 19

let g18 x = x * 20

let g19 x = x * 21

let g20 x = x * 22

let g21 x = x * 23

let g22 x = x * 24

let g23 x = x * 25

let g24 x = x * 26

let g25 x = x * 27

let g26 x = x * 28

let g27 x = x * 29

let g28 x = x * 30

let g29 x = x * 31

let g30 x = x * 32

let g31 x = x * 33

let g32 x = x * 34

let g33 x = x * 35

let g34 x = x * 36

let g35 x = x * 37

let g36 x = x * 38

let g37 x = x * 39

let g38 x = x * 40

let g39 x = x * 41

let g40 x = x * 42

let g41 x = x * 43

let g42 x = x * 44

let g43 x = x * 45

let g44 x = x * 46

let g45 x = x * 47

let g46 x = x * 48

let g47 x = x * 49

let g48 x = x * 50

let g49 x = x * 51

let call_all x =
  g00 x + g01 x + g02 x + g03 x + g04 x + g05 x + g06 x + g07 x + g08 x + g09 x
  + g10 x + g11 x + g12 x + g13 x + g14 x + g15 x + g16 x + g17 x + g18 x
  + g19 x + g20 x + g21 x + g22 x + g23 x + g24 x + g25 x + g26 x + g27 x
  + g28 x + g29 x + g30 x + g31 x + g32 x + g33 x + g34 x + g35 x + g36 x
  + g37 x + g38 x + g39 x + g40 x + g41 x + g42 x + g43 x + g44 x + g45 x
  + g46 x + g47 x + g48 x + g49 x

(* Cross-partition state manipulation *)
let increment_both () =
  incr local_counter;
  let a = Nostdlib_a.increment () in
  !local_counter, a
