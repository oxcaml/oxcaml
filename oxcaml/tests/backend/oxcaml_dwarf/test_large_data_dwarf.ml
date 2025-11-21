let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

(* CR sspies: We are currently inconsistent in how much data we print. We should
   adjust this based on LLDB configuration variables. *)

(* Large string - 16KB *)
let[@inline never] [@local never] f_large_string (x : string) = x

let large_string = String.make (16 * 1024) 'A'

let _ = f_large_string large_string

(* Large bytes - 16KB *)
let[@inline never] [@local never] f_large_bytes (x : bytes) = x

let large_bytes = Bytes.make (16 * 1024) 'B'

let _ = f_large_bytes large_bytes

(* Array with 1000 int entries *)
let[@inline never] [@local never] f_large_int_array (x : int array) = x

let large_int_array = Array.init 1000 (fun i -> i * 2)

let _ = f_large_int_array large_int_array

(* Float array with 1000 entries *)
let[@inline never] [@local never] f_large_float_array (x : float array) = x

let large_float_array = Array.init 1000 (fun i -> Float.of_int i *. 0.1)

let _ = f_large_float_array large_float_array

(* List with 1000 entries *)
let[@inline never] [@local never] f_large_list (x : int list) = x

let large_list = List.init 1000 (fun i -> i * 3)

let _ = f_large_list large_list

(* Polymorphic function called with large list *)
let[@inline never] [@local never] f_polymorphic_large (x : 'a) = x

let _ = f_polymorphic_large large_list

(* Large Bigarray - 100x100 *)
let[@inline never] [@local never] f_large_bigarray
    (x : (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array2.t) =
  x

let large_bigarray =
  Bigarray.Array2.create Bigarray.int Bigarray.c_layout 100 100

let _ =
  for i = 0 to 99 do
    for j = 0 to 99 do
      Bigarray.Array2.set large_bigarray i j ((i * 100) + j)
    done
  done

let _ = f_large_bigarray large_bigarray
