(* TEST
 flags = "-I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

module Utf8 = Misc.Utf8_lexeme
type 'a mismatch = { output: 'a; expected: 'a }
type test_result = (unit, (Utf8.t, Utf8.t) Result.t mismatch) result
let test f input expected : test_result =
  let output = f input in
  if output = expected then Ok ()
  else Error { output; expected }
[%%expect {|
module Utf8 = Misc.Utf8_lexeme
type 'a mismatch = { output : 'a; expected : 'a; }
type test_result = (unit, (Utf8.t, Utf8.t) Result.t mismatch) result
val test :
  ('a -> (Utf8.t, Utf8.t) Result.t) ->
  'a -> (Utf8.t, Utf8.t) Result.t -> test_result = <fun>
|}];;


(* empty string *)

test Utf8.normalize "" (Ok "");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "" (Ok "");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "" (Ok "");;
[%%expect {|
- : test_result = Ok ()
|}];;



(* ascii-only fast path *)

test Utf8.normalize "hello" (Ok "hello");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "hello" (Ok "Hello");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "Hello" (Ok "Hello");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "hello" (Ok "hello");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "Hello" (Ok "hello");;
[%%expect {|
- : test_result = Ok ()
|}];;


(* non-ascii-only, no normalization *)

test Utf8.normalize "helloÀ" (Ok "helloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "helloÀ" (Ok "HelloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "HelloÀ" (Ok "HelloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "helloÀ" (Ok "helloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "HelloÀ" (Ok "helloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

(* non-ascii-only, normalization on first char *)
test Utf8.normalize "A\xcc\x80" (Ok "À");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "A\xcc\x80" (Ok "À");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "a\xcc\x80" (Ok "À");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "A\xcc\x80" (Ok "à");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "a\xcc\x80" (Ok "à");;
[%%expect {|
- : test_result = Ok ()
|}];;


(* non-ascii-only, normalization on non-first char *)

test Utf8.normalize "helloA\xcc\x80" (Ok "helloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "helloA\xcc\x80" (Ok "HelloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "HelloA\xcc\x80" (Ok "HelloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "helloA\xcc\x80" (Ok "helloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "HelloA\xcc\x80" (Ok "helloÀ");;
[%%expect {|
- : test_result = Ok ()
|}];;



(* outside the ascii-only fast path: error *)

test Utf8.normalize "hello\255" (Error "hello\u{FFFD}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "hello\255" (Error "Hello\u{FFFD}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "Hello\255" (Error "Hello\u{FFFD}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "hello\255" (Error "hello\u{FFFD}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "Hello\255" (Error "hello\u{FFFD}");;
[%%expect {|
- : test_result = Ok ()
|}];;


(* Not upstream. Boundary cases for the match-based tables of
   ocaml/ocaml#14618; keep on a 5.5 merge.

   [get_known_pair] inspects its first argument with [Uchar.unsafe_to_char],
   which is [%identity], so for a base character above U+00FF the resulting
   "char" holds a value above 255 and every branch must simply miss. (For
   U+0080-U+00FF it is an ordinary Latin-1 character, which also misses: no
   arm is a non-ASCII letter.) The two cases below are the discriminating
   ones: U+0141 and U+0161 have low bytes 0x41 ('A') and 0x61 ('a'), so a
   truncating conversion would wrongly fold them with the following combining
   character. *)

test Utf8.normalize "\u{0141}\u{0300}" (Ok "\u{0141}\u{0300}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.normalize "\u{0161}\u{0301}" (Ok "\u{0161}\u{0301}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.normalize "\u{0100}\u{0300}" (Ok "\u{0100}\u{0300}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.normalize "\u{898B}\u{0300}" (Ok "\u{898B}\u{0300}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "\u{0141}\u{0300}" (Ok "\u{0141}\u{0300}");;
[%%expect {|
- : test_result = Ok ()
|}];;

(* [capitalize] does upcase the first character here; what matters is that
   the combining acute is not folded into it. *)
test Utf8.capitalize "\u{0161}\u{0301}" (Ok "\u{0160}\u{0301}");;
[%%expect {|
- : test_result = Ok ()
|}];;

(* A cased non-ASCII first character followed by a combining character that
   does compose with a *later* ASCII base character: exercises the slow path
   with a transformed first character. *)

test Utf8.uncapitalize "\u{0160}eE\u{0301}" (Ok "\u{0161}e\u{00C9}");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "\u{0161}eE\u{0301}" (Ok "\u{0160}e\u{00C9}");;
[%%expect {|
- : test_result = Ok ()
|}];;

(* [uchar_lowercase]/[uchar_uppercase] round-trip through the int-carrying
   [case] constructors introduced by #14618. *)

test Utf8.uncapitalize "\u{1E9E}x" (Ok "\u{00DF}x");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "\u{00DF}x" (Ok "\u{1E9E}x");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "\u{00FF}x" (Ok "\u{0178}x");;
[%%expect {|
- : test_result = Ok ()
|}];;

(* Single-character ASCII input: [restlen] is 0, so the fast path performs a
   zero-length blit. *)

test Utf8.capitalize "a" (Ok "A");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "A" (Ok "a");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "A" (Ok "A");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "_" (Ok "_");;
[%%expect {|
- : test_result = Ok ()
|}];;

(* Invalid first byte: the [first] transformation is applied to U+FFFD, and
   the error tag must survive it. *)

test Utf8.normalize "\xffa" (Error "\u{FFFD}a");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.capitalize "\xffa" (Error "\u{FFFD}a");;
[%%expect {|
- : test_result = Ok ()
|}];;

test Utf8.uncapitalize "\x80A" (Error "\u{FFFD}A");;
[%%expect {|
- : test_result = Ok ()
|}];;
