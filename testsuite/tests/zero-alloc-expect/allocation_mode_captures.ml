(* TEST
 flags = "-extension mode_alpha -extension mode_polymorphism_alpha -dlambda";
 expect.opt;
*)

let make f = fun () -> f ()
[%%expect{|
(let
  (make/0 =
     (function {nlocal = 0} f/0
       (function {nlocal = 1} param/0[L][value<int>]
         assert_zero_alloc_strict customer_error_message "Backend verification of inferred noalloc_strict mode failed."
         (apply[yielding] f/0 0))))
  (makeblock 0 make/0))
val make : (unit -> 'a) -> unit -> 'a = <fun>
|}]

let make_relaxed (f @ noalloc) = fun () -> f ()
[%%expect{|
(let
  (make_relaxed/0 =
     (function {nlocal = 0} f/1
       (function {nlocal = 1} param/1[L][value<int>]
         assert_zero_alloc customer_error_message "Backend verification of inferred noalloc mode failed."
         (apply[yielding] f/1 0))))
  (makeblock 0 make_relaxed/0))
val make_relaxed : (unit -> 'a) @ noalloc -> unit -> 'a = <fun>
|}]

let make_allocating (f @ alloc) = fun () -> f ()
[%%expect{|
(let
  (make_allocating/0 =
     (function {nlocal = 0} f/2
       (function {nlocal = 1} param/2[L][value<int>] (apply[yielding] f/2 0))))
  (makeblock 0 make_allocating/0))
val make_allocating : (unit -> 'a) -> unit -> 'a = <fun>
|}]

let make_with_allocation f = fun () -> ref (f ())
[%%expect{|
(let
  (make_with_allocation/0 =
     (function {nlocal = 0} f/3
       (function {nlocal = 1} param/3[L][value<int>]
         (makemutable 0 (apply[yielding] f/3 0)))))
  (makeblock 0 make_with_allocation/0))
val make_with_allocation : (unit -> 'a) -> unit -> 'a ref = <fun>
|}]
