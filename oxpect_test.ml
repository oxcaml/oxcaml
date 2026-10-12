let x = 42.0
[%%expect.type {|
val x : float
|}]


let f x =
  match x with
  | Either.Left (a, b) -> a + b
[%%expect.warn {|
File "_", lines 1-2, characters 2-31:
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: Right _
|}]


let f = function None -> 1
[%%expect.warn {|
File "_", line 1, characters 8-26:
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: Some _
|}]

let f (Some x) = x + 10
[%%expect.warn {|
File "_", line 1, characters 6-14:
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: None
|}]
[%%expect.type {|
val f : int option -> int
|}]
[%%expect.lambda {|
f =
  (function {nlocal = 0}
    param[value<(consts (0)) (non_consts ([0: value<int>]))>] : int
    (if (isint param)
      (raise (makeblock 0 (getpredef Match_failure!!) [0: "<phrase_3>" 0 6]))
      (%int_add (field_imm 0 param) 10)))
|}]

let p a b c = a + b + c
[%%expect.lambda {|
p =
  (function {nlocal = 0} a[value<int>] b[value<int>] c[value<int>] : int
    (%int_add (%int_add a b) c))
|}]

let () =
  Gc.print_stat stdout
[%%expect.output {|
minor_collections:      6
major_collections:      3
compactions:            0
forced_major_collections: 1

minor_words:    90
promoted_words: 86
major_words:    152

top_heap_words: 24576
heap_words:     1048576
live_words:       152
free_words:     1048368
largest_free:       0
fragments:         56

live_blocks: 28
free_blocks: 0
heap_chunks: 1
|}]

let () =
  Printf.printf "wat %5.2f\n" 42e1
[%%expect.lambda {|
*match* =
  (apply (field_imm 1 (global Stdlib__Printf!))
    [0:
     [11: "wat " [8: [0: 0 0] [0: 1 5] [0: 2] [12: '\n' 0]]] "wat %5.2f\n"]
    42e1)
|}]
[%%expect.output {|
wat 420.00
|}]

let () =
  Printf.printf "Runtime params: %s\n" (Sys.runtime_parameters ())
[%%expect.output {|
Runtime params: b=0,c=0,d=1,e=16,H=0,i=15,l=134217728,m=400,M=44,n=10,o=80,O=500,p=0,s=1048576,t=0,v=0,V=0,W=0
|}]
