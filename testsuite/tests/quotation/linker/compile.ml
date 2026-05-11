#syntax quotations on

let get_result () =
  let abc = <[fun () -> Dep.as_int (Dep.foo ())]> in
  let num = Eval.eval abc in
  num ()
