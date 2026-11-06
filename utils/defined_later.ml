module Fun = struct
  type 'f t =
    { name : string;
      mutable f : 'f;
      mutable defined : bool
    }
    constraint 'f = _ -> _

  let create ~name =
    { name;
      f =
        (fun _ -> Misc.fatal_errorf "Defined_later: %s is not yet defined" name);
      defined = false
    }

  let define_exn t f =
    if t.defined
    then Misc.fatal_errorf "Defined_later: %s is already defined" t.name;
    t.f <- f;
    t.defined <- true

  let get t x = t.f x
end
