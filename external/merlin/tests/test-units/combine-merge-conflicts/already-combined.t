  $ cat > input.ml <<EOF
  > <<<<<<<<<<<<<< Merlin:current
  > <<<<<<< current
  >       let error = Submode_failed(failure_reason, reason) in
  >       let new_arg2 = failwith "todo" in
  >       raise (error(loc, env, error, new_arg2))
  > ||||||| merge-base
  >       let err = Submode_failed(failure_reason, reason) in
  >       raise (error(loc, env, err))
  > =======
  >       let error = Submode_failed(failure_reason, reason) in
  >       let new_arg1 = failwith "todo" in
  >       raise (error(loc, env, error, new_arg1))
  > >>>>>>> incoming
  > |||||||||||||| Compiler:last-imported
  >       let error = Submode_failed(failure_reason, reason) in
  > <<<<<<< current
  >       let new_arg2 = failwith "todo" in
  >       raise (Error(loc, env, error, new_arg2))
  > ||||||| merge-base
  >       raise (Error(loc, env, error))
  > =======
  >       let new_arg1 = failwith "todo" in
  >       raise (Error(loc, env, error, new_arg1))
  > >>>>>>> incoming
  > ==============
  >       let error = Submode_failed(failure_reason, reason) in
  >       let new_arg1 = failwith "todo" in
  >       let new_arg2 = failwith "todo" in
  >       raise (Error(loc, env, error, new_arg1, new_arg2))
  > >>>>>>>>>>>>>> Compiler:incoming
  > EOF

  $ combine-merge-conflicts input.ml -o output.ml
  $ diff input.ml output.ml
