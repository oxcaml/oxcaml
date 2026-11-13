(* The counters in Cmm, which must be those of the Flambda translated to it. *)

let[@inline never] side x = x * 3

(* Each case of a jump table has the counters of its arm; arms going to the same
   handler share a case, which has all their counters. *)
type t =
  | A
  | B
  | C
  | D
  | E
  | F

let[@inline never] jump_table v =
  match v with A -> 1 | B -> 3 | C -> 7 | D -> 2 | E | F -> side 4

let[@inline never] if_then_else b x = if b then side x else x + 1

(* The counter of an indirect call is on its call to [caml_apply]. *)
let[@inline never] indirect f x = f x x + 1

(* So is the counter of a call of an unknown function with one argument, which
   goes straight to the code pointer of the closure. *)
let[@inline never] one_argument f x = f x

(* A call with too many arguments for a single [caml_apply] is split: only the
   first [caml_apply] has the counter. *)
let[@inline never] too_many_arguments f =
  f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
    29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53
    54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78
    79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102
    103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121
    122 123 124 125 126 127 128 129

(* Method calls go through [caml_send]. *)
let[@inline never] method_call o = o#m 1 + 1

let rec loop n = if n > 0 then loop (side n - n - n - n - 1) else n
