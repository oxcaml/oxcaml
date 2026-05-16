#syntax quotations on

let get_result () =
  let abc = <[fun () -> Dep.foo ()]> in
  let num = Eval.eval abc in
  num ()
