
(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to caml_apply19.byte
    (run bytetop %{dep:caml_apply19.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to caml_apply19.nat
    (run nattop %{dep:caml_apply19.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to caml_apply19.jit
    (run jittop %{dep:caml_apply19.ml})))))

(rule
 (alias bytetest)
 (action
  (diff caml_apply19.expected caml_apply19.byte)))

(rule
 (alias nattest)
 (action
  (diff caml_apply19.byte caml_apply19.nat)))

(rule
 (alias jittest)
 (action
  (diff caml_apply19.nat caml_apply19.jit)))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to hello_world.byte
    (run bytetop %{dep:hello_world.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to hello_world.nat
    (run nattop %{dep:hello_world.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to hello_world.jit
    (run jittop %{dep:hello_world.ml})))))

(rule
 (alias bytetest)
 (action
  (diff hello_world.expected hello_world.byte)))

(rule
 (alias nattest)
 (action
  (diff hello_world.byte hello_world.nat)))

(rule
 (alias jittest)
 (action
  (diff hello_world.nat hello_world.jit)))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to let_underscore.byte
    (run bytetop %{dep:let_underscore.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to let_underscore.nat
    (run nattop %{dep:let_underscore.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to let_underscore.jit
    (run jittop %{dep:let_underscore.ml})))))

(rule
 (alias bytetest)
 (action
  (diff let_underscore.expected let_underscore.byte)))

(rule
 (alias nattest)
 (action
  (diff let_underscore.byte let_underscore.nat)))

(rule
 (alias jittest)
 (action
  (diff let_underscore.nat let_underscore.jit)))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to local_exc.byte
    (run bytetop %{dep:local_exc.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to local_exc.nat
    (run nattop %{dep:local_exc.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to local_exc.jit
    (run jittop %{dep:local_exc.ml})))))

(rule
 (alias bytetest)
 (action
  (diff local_exc.expected local_exc.byte)))

(rule
 (alias nattest)
 (action
  (diff local_exc.byte local_exc.nat)))

(rule
 (alias jittest)
 (action
  (diff local_exc.nat local_exc.jit)))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to one_plus_one.byte
    (run bytetop %{dep:one_plus_one.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to one_plus_one.nat
    (run nattop %{dep:one_plus_one.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to one_plus_one.jit
    (run jittop %{dep:one_plus_one.ml})))))

(rule
 (alias bytetest)
 (action
  (diff one_plus_one.expected one_plus_one.byte)))

(rule
 (alias nattest)
 (action
  (diff one_plus_one.byte one_plus_one.nat)))

(rule
 (alias jittest)
 (action
  (diff one_plus_one.nat one_plus_one.jit)))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to preserve_type_vars.byte
    (run bytetop %{dep:preserve_type_vars.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to preserve_type_vars.nat
    (run nattop %{dep:preserve_type_vars.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to preserve_type_vars.jit
    (run jittop %{dep:preserve_type_vars.ml})))))

(rule
 (alias bytetest)
 (action
  (diff preserve_type_vars.expected preserve_type_vars.byte)))

(rule
 (alias nattest)
 (action
  (diff preserve_type_vars.byte preserve_type_vars.nat)))

(rule
 (alias jittest)
 (action
  (diff preserve_type_vars.nat preserve_type_vars.jit)))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to rwo_segfault.byte
    (run bytetop %{dep:rwo_segfault.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to rwo_segfault.nat
    (run nattop %{dep:rwo_segfault.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to rwo_segfault.jit
    (run jittop %{dep:rwo_segfault.ml})))))

(rule
 (alias bytetest)
 (action
  (diff rwo_segfault.expected rwo_segfault.byte)))

(rule
 (alias nattest)
 (action
  (diff rwo_segfault.byte rwo_segfault.nat)))

(rule
 (alias jittest)
 (action
  (diff rwo_segfault.nat rwo_segfault.jit)))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to simple_fun.byte
    (run bytetop %{dep:simple_fun.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to simple_fun.nat
    (run nattop %{dep:simple_fun.ml})))))

(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to simple_fun.jit
    (run jittop %{dep:simple_fun.ml})))))

(rule
 (alias bytetest)
 (action
  (diff simple_fun.expected simple_fun.byte)))

(rule
 (alias nattest)
 (action
  (diff simple_fun.byte simple_fun.nat)))

(rule
 (alias jittest)
 (action
  (diff simple_fun.nat simple_fun.jit)))

(alias
 (name runtest)
 (deps
  (alias bytetest)
  (alias nattest)
  (alias jittest)))
