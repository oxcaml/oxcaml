(* Module A: No stdlib, defines basic primitives *)

external ( + ) : int -> int -> int = "%addint"

external ( - ) : int -> int -> int = "%subint"

external ( * ) : int -> int -> int = "%mulint"

type 'a ref = { mutable contents : 'a }

external ref : 'a -> 'a ref = "%makemutable"

let ( ! ) r = r.contents

let incr r = r.contents <- r.contents + 1

(* Array access *)
external array_get : 'a array -> int -> 'a = "%array_safe_get"

(* Large immutable array literal to increase module size *)
let arr1 =
  [| 0;
     1;
     2;
     3;
     4;
     5;
     6;
     7;
     8;
     9;
     10;
     11;
     12;
     13;
     14;
     15;
     16;
     17;
     18;
     19;
     20;
     21;
     22;
     23;
     24;
     25;
     26;
     27;
     28;
     29;
     30;
     31;
     32;
     33;
     34;
     35;
     36;
     37;
     38;
     39;
     40;
     41;
     42;
     43;
     44;
     45;
     46;
     47;
     48;
     49;
     50;
     51;
     52;
     53;
     54;
     55;
     56;
     57;
     58;
     59;
     60;
     61;
     62;
     63;
     64;
     65;
     66;
     67;
     68;
     69;
     70;
     71;
     72;
     73;
     74;
     75;
     76;
     77;
     78;
     79;
     80;
     81;
     82;
     83;
     84;
     85;
     86;
     87;
     88;
     89;
     90;
     91;
     92;
     93;
     94;
     95;
     96;
     97;
     98;
     99
  |]

(* State *)
let counter = ref 0

let magic = 42

let increment () =
  incr counter;
  !counter

let get_magic () = magic

(* Many functions to increase code size *)
let f00 x = x + 1

let f01 x = x + 2

let f02 x = x + 3

let f03 x = x + 4

let f04 x = x + 5

let f05 x = x + 6

let f06 x = x + 7

let f07 x = x + 8

let f08 x = x + 9

let f09 x = x + 10

let f10 x = x + 11

let f11 x = x + 12

let f12 x = x + 13

let f13 x = x + 14

let f14 x = x + 15

let f15 x = x + 16

let f16 x = x + 17

let f17 x = x + 18

let f18 x = x + 19

let f19 x = x + 20

let f20 x = x + 21

let f21 x = x + 22

let f22 x = x + 23

let f23 x = x + 24

let f24 x = x + 25

let f25 x = x + 26

let f26 x = x + 27

let f27 x = x + 28

let f28 x = x + 29

let f29 x = x + 30

let f30 x = x + 31

let f31 x = x + 32

let f32 x = x + 33

let f33 x = x + 34

let f34 x = x + 35

let f35 x = x + 36

let f36 x = x + 37

let f37 x = x + 38

let f38 x = x + 39

let f39 x = x + 40

let f40 x = x + 41

let f41 x = x + 42

let f42 x = x + 43

let f43 x = x + 44

let f44 x = x + 45

let f45 x = x + 46

let f46 x = x + 47

let f47 x = x + 48

let f48 x = x + 49

let f49 x = x + 50

let call_all x =
  f00 x + f01 x + f02 x + f03 x + f04 x + f05 x + f06 x + f07 x + f08 x + f09 x
  + f10 x + f11 x + f12 x + f13 x + f14 x + f15 x + f16 x + f17 x + f18 x
  + f19 x + f20 x + f21 x + f22 x + f23 x + f24 x + f25 x + f26 x + f27 x
  + f28 x + f29 x + f30 x + f31 x + f32 x + f33 x + f34 x + f35 x + f36 x
  + f37 x + f38 x + f39 x + f40 x + f41 x + f42 x + f43 x + f44 x + f45 x
  + f46 x + f47 x + f48 x + f49 x
