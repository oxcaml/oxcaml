[run <file> <pos>] queries <file> at position <pos>. The multiline
json that merlin produces can't be parsed by jq, as they include
escape characters in string literals, so we use the revert-newlines script.

  $ run_with_verbosity () {
  >   file=$1
  >   position=$2
  >   verbosity=$3
  >   echo -n "With verbosity $verbosity: "
  >   $MERLIN single type-enclosing -position "$position" -verbosity "$verbosity" \
  >     -filename "$file" < "$file" |
  >     revert-newlines |
  >     jq '.value[0].type' |
  >     sed 's/\\n/\n/g'
  > }

  $ run () {
  >   file=$1
  >   position=$2
  >   line=$(echo "$position" | cut -d ':' -f 1)
  >   col=$(echo "$position" | cut -d ':' -f 2)
  >   sed -n "${line}p" "$file"
  >   printf "%*s^\n" "$(expr $col - 1)" ''
  >   run_with_verbosity "$file" "$position" 0
  >   run_with_verbosity "$file" "$position" 1
  >   echo
  > }


(I) Type declarations

  $ run layouts.ml 1:5
  > run layouts.ml 2:5
  > run layouts.ml 3:5
  > run layouts.ml 4:5
  > run layouts.ml 5:5
  > run layouts.ml 6:5
  > run layouts.ml 7:5
  type t0 = int
      ^
  With verbosity 0: "type t0 = int"
  With verbosity 1: "type t0 : immediate = int"
  
  type t1 : immediate = int
      ^
  With verbosity 0: "type t1 = int"
  With verbosity 1: "type t1 : immediate = int"
  
  type t2 : immediate = A | B | C
      ^
  With verbosity 0: "type t2 = A | B | C"
  With verbosity 1: "type t2 : immediate = A | B | C"
  
  type t3 : value = D | E of int
      ^
  With verbosity 0: "type t3 = D | E of int"
  With verbosity 1: "type t3 : immutable_data = D | E of int"
  
  type t4 : immediate64 = A
      ^
  With verbosity 0: "type t4 = A"
  With verbosity 1: "type t4 : immediate = A"
  
  type t5 : value
      ^
  With verbosity 0: "type t5"
  With verbosity 1: "type t5"
  
  type t6 : value = { v : int } [@@unboxed]
      ^
  With verbosity 0: "type t6 = { v : int; } [@@unboxed]"
  With verbosity 1: "type t6 : immediate = { v : int; } [@@unboxed]"
  

(II) Functions

- definition
  $ run layouts.ml 9:5
  > run layouts.ml 10:5
  > run layouts.ml 11:5
  > run layouts.ml 12:5
  > run layouts.ml 13:5
  > run layouts.ml 14:5
  > run layouts.ml 15:5
  > run layouts.ml 16:5
  let f_int (x : int) = x
      ^
  With verbosity 0: "int -> int"
  With verbosity 1: "int -> int"
  
  let f0 (x : t0) = x
      ^
  With verbosity 0: "t0 -> t0"
  With verbosity 1: "int -> int"
  
  let f1 (x : t1) = x
      ^
  With verbosity 0: "t1 -> t1"
  With verbosity 1: "int -> int"
  
  let f2 (x : t2) = x
      ^
  With verbosity 0: "t2 -> t2"
  With verbosity 1: "t2 -> t2"
  
  let f3 (x : t3) = x
      ^
  With verbosity 0: "t3 -> t3"
  With verbosity 1: "t3 -> t3"
  
  let f4 (x : t4) = x
      ^
  With verbosity 0: "t4 -> t4"
  With verbosity 1: "t4 -> t4"
  
  let f5 (x : t5) = x
      ^
  With verbosity 0: "t5 -> t5"
  With verbosity 1: "t5 -> t5"
  
  let f6 (x : t6) = x
      ^
  With verbosity 0: "t6 -> t6"
  With verbosity 1: "t6 -> t6"
  

- parameter
  $ run layouts.ml 9:12
  > run layouts.ml 10:9
  > run layouts.ml 11:9
  > run layouts.ml 12:9
  > run layouts.ml 13:9
  > run layouts.ml 14:9
  > run layouts.ml 15:9
  > run layouts.ml 16:9
  let f_int (x : int) = x
             ^
  With verbosity 0: "int"
  With verbosity 1: "int"
  
  let f0 (x : t0) = x
          ^
  With verbosity 0: "t0"
  With verbosity 1: "int"
  
  let f1 (x : t1) = x
          ^
  With verbosity 0: "t1"
  With verbosity 1: "int"
  
  let f2 (x : t2) = x
          ^
  With verbosity 0: "t2"
  With verbosity 1: "type t2 = A | B | C"
  
  let f3 (x : t3) = x
          ^
  With verbosity 0: "t3"
  With verbosity 1: "type t3 = D | E of int"
  
  let f4 (x : t4) = x
          ^
  With verbosity 0: "t4"
  With verbosity 1: "type t4 = A"
  
  let f5 (x : t5) = x
          ^
  With verbosity 0: "t5"
  With verbosity 1: "t5"
  
  let f6 (x : t6) = x
          ^
  With verbosity 0: "t6"
  With verbosity 1: "type t6 = { v : int; } [@@unboxed]"
  

- type annotation
  $ run layouts.ml 9:16
  > run layouts.ml 10:13
  > run layouts.ml 11:13
  > run layouts.ml 12:13
  > run layouts.ml 13:13
  > run layouts.ml 14:13
  > run layouts.ml 15:13
  > run layouts.ml 16:13
  let f_int (x : int) = x
                 ^
  With verbosity 0: "type int : immediate"
  With verbosity 1: "type int : immediate"
  
  let f0 (x : t0) = x
              ^
  With verbosity 0: "type t0 = int"
  With verbosity 1: "type t0 : immediate = int"
  
  let f1 (x : t1) = x
              ^
  With verbosity 0: "type t1 = int"
  With verbosity 1: "type t1 : immediate = int"
  
  let f2 (x : t2) = x
              ^
  With verbosity 0: "type t2 = A | B | C"
  With verbosity 1: "type t2 : immediate = A | B | C"
  
  let f3 (x : t3) = x
              ^
  With verbosity 0: "type t3 = D | E of int"
  With verbosity 1: "type t3 : immutable_data = D | E of int"
  
  let f4 (x : t4) = x
              ^
  With verbosity 0: "type t4 = A"
  With verbosity 1: "type t4 : immediate = A"
  
  let f5 (x : t5) = x
              ^
  With verbosity 0: "type t5"
  With verbosity 1: "type t5"
  
  let f6 (x : t6) = x
              ^
  With verbosity 0: "type t6 = { v : int; } [@@unboxed]"
  With verbosity 1: "type t6 : immediate = { v : int; } [@@unboxed]"
  

(III) Polymorphic functions
- definition
  $ run layouts.ml 18:5
  > run layouts.ml 19:5
  > run layouts.ml 20:5
  > run layouts.ml 21:5
  let poly1 (type a)           (x : a) = x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a"
  
  let poly2 (type a : value)   (x : a) = x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a"
  
  let poly3 (type a : float64) (x : a) = x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a (* 'a : float64 *)"
  
  let poly4 (type (a : immediate) (b : value)) (f : a -> b -> _) = f
      ^
  With verbosity 0: "('a -> 'b -> 'c) -> 'a -> 'b -> 'c"
  With verbosity 1: "('a -> 'b -> 'c) -> 'a -> 'b -> 'c (* 'a : immediate, 'c : any *)"
  

-parameter
  $ run layouts.ml 18:30
  > run layouts.ml 19:30
  > run layouts.ml 20:30
  > run layouts.ml 21:46
  let poly1 (type a)           (x : a) = x
                               ^
  With verbosity 0: "a"
  With verbosity 1: "a"
  
  let poly2 (type a : value)   (x : a) = x
                               ^
  With verbosity 0: "a"
  With verbosity 1: "a"
  
  let poly3 (type a : float64) (x : a) = x
                               ^
  With verbosity 0: "a"
  With verbosity 1: "a"
  
  let poly4 (type (a : immediate) (b : value)) (f : a -> b -> _) = f
                                               ^
  With verbosity 0: "a -> b -> 'a"
  With verbosity 1: "a -> b -> 'a (* 'a : any *)"
  
-type annotation
  $ run layouts.ml 18:35
  > run layouts.ml 19:35
  > run layouts.ml 20:35
  let poly1 (type a)           (x : a) = x
                                    ^
  With verbosity 0: "type a"
  With verbosity 1: "type a"
  
  let poly2 (type a : value)   (x : a) = x
                                    ^
  With verbosity 0: "type a"
  With verbosity 1: "type a"
  
  let poly3 (type a : float64) (x : a) = x
                                    ^
  With verbosity 0: "type a : float64"
  With verbosity 1: "type a : float64"
  

(IV) Polymorphic function client
- definition
  $ run layouts.ml 23:5
  > run layouts.ml 24:5
  > run layouts.ml 25:5
  > run layouts.ml 26:5
  let poly_client1 x = poly1 x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a"
  
  let poly_client2 x = poly2 x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a"
  
  let poly_client3 x = poly3 x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a (* 'a : float64 *)"
  
  let poly_client4 x = poly4 x
      ^
  With verbosity 0: "('a -> 'b -> 'c) -> 'a -> 'b -> 'c"
  With verbosity 1: "('a -> 'b -> 'c) -> 'a -> 'b -> 'c (* 'a : immediate, 'c : any *)"
  

-parameter
  $ run layouts.ml 23:18
  > run layouts.ml 24:18
  > run layouts.ml 25:18
  > run layouts.ml 26:18
  let poly_client1 x = poly1 x
                   ^
  With verbosity 0: "'a"
  With verbosity 1: "'a"
  
  let poly_client2 x = poly2 x
                   ^
  With verbosity 0: "'a"
  With verbosity 1: "'a"
  
  let poly_client3 x = poly3 x
                   ^
  With verbosity 0: "'a"
  With verbosity 1: "'a (* 'a : float64 *)"
  
  let poly_client4 x = poly4 x
                   ^
  With verbosity 0: "'a -> 'b -> 'c"
  With verbosity 1: "'a -> 'b -> 'c (* 'a : immediate, 'c : any *)"
  
(V) Parameterized type
- definition
  $ run layouts.ml 28:22
  > run layouts.ml 29:22
  > run layouts.ml 30:22
  type _                p0 = A0
                       ^
  With verbosity 0: "type _ p0 = A0"
  With verbosity 1: "type _ p0 : immediate = A0"
  
  type 'a               p1 = A1 of 'a
                       ^
  With verbosity 0: "type 'a p1 = A1 of 'a"
  With verbosity 1: "type 'a p1 : immutable_data with 'a = A1 of 'a"
  
  type ('a : immediate) p2 = A2 of 'a [@@unboxed]
                       ^
  With verbosity 0: "type ('a : immediate) p2 = A2 of 'a [@@unboxed]"
  With verbosity 1: "type ('a : immediate) p2 : immediate = A2 of 'a [@@unboxed]"
  

- parameter
  $ run layouts.ml 28:6
  > run layouts.ml 29:6
  > run layouts.ml 30:6
  type _                p0 = A0
       ^
  With verbosity 0: "_"
  With verbosity 1: "_"
  
  type 'a               p1 = A1 of 'a
       ^
  With verbosity 0: "'a"
  With verbosity 1: "'a"
  
  type ('a : immediate) p2 = A2 of 'a [@@unboxed]
       ^
  With verbosity 0: "'a"
  With verbosity 1: "'a (* 'a : immediate *)"
  

(V) Parameterized type client
- definition
  $ run layouts.ml 32:5
  > run layouts.ml 33:5
  > run layouts.ml 34:5
  let param_client1 (x : 'a p0) (a : 'a) = x, a
      ^
  With verbosity 0: "'a p0 -> 'a -> 'a p0 * 'a"
  With verbosity 1: "'a p0 -> 'a -> 'a p0 * 'a"
  
  let param_client2 (x : 'a p1) (a : 'a) = x, a
      ^
  With verbosity 0: "'a p1 -> 'a -> 'a p1 * 'a"
  With verbosity 1: "'a p1 -> 'a -> 'a p1 * 'a"
  
  let param_client2 (x : 'a p2) (a : 'a) = x, a
      ^
  With verbosity 0: "'a p2 -> 'a -> 'a p2 * 'a"
  With verbosity 1: "'a p2 -> 'a -> 'a p2 * 'a (* 'a : immediate *)"
  

- parameter
  $ run layouts.ml 32:19
  > run layouts.ml 33:19
  > run layouts.ml 34:19
  let param_client1 (x : 'a p0) (a : 'a) = x, a
                    ^
  With verbosity 0: "'a p0"
  With verbosity 1: "'a p0
  
  type _ p0 = A0"
  
  let param_client2 (x : 'a p1) (a : 'a) = x, a
                    ^
  With verbosity 0: "'a p1"
  With verbosity 1: "'a p1
  
  type 'a p1 = A1 of 'a"
  
  let param_client2 (x : 'a p2) (a : 'a) = x, a
                    ^
  With verbosity 0: "'a p2"
  With verbosity 1: "'a p2 (* 'a : immediate *)
  
  type ('a : immediate) p2 = A2 of 'a [@@unboxed]"
  
(VI) Long type
  $ run layouts.ml 36:4
  let long_type a b c d e f g =
     ^
  With verbosity 0: "'a ->
  'b ->
  'c ->
  'd ->
  'e -> 'f -> 'g -> 'a p2 * 'b p2 * 'c p2 * 'd p2 * 'e p2 * 'f p2 * 'g p2 * 'h"
  With verbosity 1: "'a ->
  'b ->
  'c ->
  'd ->
  'e -> 'f -> 'g -> 'a p2 * 'b p2 * 'c p2 * 'd p2 * 'e p2 * 'f p2 * 'g p2 * 'h 
  (* 'a : immediate, 'b : immediate, 'c : immediate, 'd : immediate, 'e : 
                                                                     immediate, 
     'f : immediate, 'g : immediate *)"
  

(VII) Non-constant sort variable
  $ run layouts.ml 46:5
  let unconstrained_f x = x
      ^
  With verbosity 0: "'a -> 'a"
  With verbosity 1: "'a -> 'a"
  
