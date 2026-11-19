(* TEST
expect;
*)

(* Values of layout void have no runtime representation, so they cannot point
   into the region: passing one to a TMC call, even at the local mode. 
   This should not block TMC, but do to current conservatism in the check, it does 
*)

let[@inline never] use_local (local_ _ : int ref) = ()

let[@inline never] make_void () : unit# = exclave_ #()

let[@tail_mod_cons] rec map_void (local_ u : unit#) f = function
  | [] -> []
  | x :: xs ->
    let local_ r = ref x in
    use_local r;
    let v = make_void () in
    f x :: map_void v f xs

[%%expect{|
val use_local : int ref @ local -> unit = <fun>
val make_void : unit -> unit# @ local = <fun>
Line 11, characters 11-26:
11 |     f x :: map_void v f xs
                ^^^^^^^^^^^^^^^
Warning 222 [tmc-breaks-tailcall-region]: This call is in tail-modulo-cons
  position in a TMC function, but it is enclosed in a local-allocation region
  that cannot be closed prior to the call, preventing the function from
  running in constant stack space.
  This happens when either the callee or an argument is at the "local" mode and
  may point to a value allocated in the surrounding region. Recursive calls,
  parameters, non-allocating projections of parameters, immediates, and
  values at the "global" mode are allowed.

val map_void : unit# @ local -> (int -> 'a) -> int list -> 'a list = <fun>
|}]

