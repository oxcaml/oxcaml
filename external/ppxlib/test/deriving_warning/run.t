Ppxlib driver has a variety of ways to disable warnings that can be triggered
when using `[@@deriving ...]`. These are all enabled by default but we added
flags to let driver users disable them. To allow smooth transition from always
adding them to never do so and let individual ppx-es do what they must to avoid
triggering warnings, we also added optional arguments to `Deriving.make` so that
the ppx themselves can declare whether they need the driver to disable warnings
or not.

The following tests describe the behaviour of flags and features used to control
the emission of such warning silencing features.

One such flag and optional argument pair is the `-unused-code-warnings` flag and
`?unused_code_warning` `Deriving.V2.make` argument. Both of those default to
`false` and control whether we disable warnings 32, 34 and 60 for generated code
and behave as described by the following table:

 Deriver arg | Driver Flag | Unused Code Warnings
-------------|-------------|----------------------
 true        | true        | Enabled
 true        | false       | Disabled*
 true        | force       | Enabled
 false       | true        | Disabled
 false       | false       | Disabled
 false       | force       | Enabled
* By adding warning silencers like [@@@ocaml.waring "-60"] or producing code like
`let _ = zero in...` or `let _ = fun (_ : t) -> ()`.

We have a driver with 4 derivers linked in:
- zero_do_warn
- one_no_warn
- two_do_warn
- alias_warn

--------------------------------------------------------------------------------

Let's consider the following ocaml source file using the zero_do_warn deriver

  $ cat > zero_do_warn.ml << EOF
  > type t = int [@@deriving zero_do_warn]
  > EOF

Zero_do_warn is registered with unused_code_warning set to true meaning it allows
the driver not to silence unused code and unused module warnings if the
-unused-code-warning flag is set to true.

Let's call the driver with -unused-code-warnings=false:

  $ ./driver.exe -unused-code-warnings=false -impl zero_do_warn.ml
  type t = int[@@deriving zero_do_warn]
  include struct let _ = fun (_ : t) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include
    struct
      [@@@ocaml.warning "-60"]
      module Zero = struct type t =
                             | T0  end
      let zero = Zero.T0
      let _ = zero
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

The generated code is wrapped in an include struct ... end to disable unused module
warnings, as expected. The derived value zero is followed by a let _ = zero to
disable unused value warning, and the type is used by `let _ = fun (_ : t) -> ()`.

Now if we use -unused-code-warnings=true:

  $ ./driver.exe -unused-code-warnings=true -impl zero_do_warn.ml
  type t = int[@@deriving zero_do_warn]
  include struct module Zero = struct type t =
                                        | T0  end
                 let zero = Zero.T0 end[@@ocaml.doc "@inline"][@@merlin.hide ]

Here the warning silencing was disabled as it was both allowed by the driver
invocation and the deriver itself. No include wrapping, no warning disabled, no
let _.

Note that this also applies to .mli files.

Consider:

  $ cat > zero_do_warn.mli << EOF
  > type t = int [@@deriving zero_do_warn]
  > EOF

and compare the result of both driver invocations:

  $ ./driver.exe -unused-code-warnings=false -intf zero_do_warn.mli
  type t = int[@@deriving zero_do_warn]
  include
    sig
      [@@@ocaml.warning "-32-60"]
      module Zero : sig type t end
      val zero : Zero.t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

  $ ./driver.exe -unused-code-warnings=true -intf zero_do_warn.mli
  type t = int[@@deriving zero_do_warn]
  include sig module Zero : sig type t end val zero : Zero.t end[@@ocaml.doc
                                                                  "@inline"]
  [@@merlin.hide ]

--------------------------------------------------------------------------------

The default value of the -unused-code-warnings should be false:

  $ ./driver.exe -impl zero_do_warn.ml
  type t = int[@@deriving zero_do_warn]
  include struct let _ = fun (_ : t) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include
    struct
      [@@@ocaml.warning "-60"]
      module Zero = struct type t =
                             | T0  end
      let zero = Zero.T0
      let _ = zero
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

As we can see here, the warnings were disabled by the driver, as is expected
with -unused-code-warnings=false.

--------------------------------------------------------------------------------

There is another value possible for the -unused-code-warnings flag: "force".
This allows the warnings to be enabled even if the deriver does not allow it. In
this example though, using `force` or `true` results in the same output, since
the deriver `zero_do_warn` already allows the warning to be enabled.

  $ ./driver.exe -unused-code-warnings=force -impl zero_do_warn.ml
  type t = int[@@deriving zero_do_warn]
  include struct module Zero = struct type t =
                                        | T0  end
                 let zero = Zero.T0 end[@@ocaml.doc "@inline"][@@merlin.hide ]

We'll see below other examples where the `force` flag is actually useful.

--------------------------------------------------------------------------------

Let's consider the following ocaml source file using the one_no_warn deriver

  $ cat > one_no_warn.ml << EOF
  > type t = int [@@deriving one_no_warn]
  > EOF

One_no_warn is registered with unused_code_warning set to false, meaning the driver
should disable warnings for the generated code, even when the value of the
-unused-code-warning is set to true. The following driver invocations have the
same output:

  $ ./driver.exe -unused-code-warnings=false -impl one_no_warn.ml
  type t = int[@@deriving one_no_warn]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      module One = struct type 'a t =
                            | T1 of 'a  end
      let one = One.T1 zero
      let _ = one
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

  $ ./driver.exe -unused-code-warnings=true -impl one_no_warn.ml
  type t = int[@@deriving one_no_warn]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      module One = struct type 'a t =
                            | T1 of 'a  end
      let one = One.T1 zero
      let _ = one
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Same goes for .mli files:

  $ cat > one_no_warn.mli << EOF
  > type t = int [@@deriving one_no_warn]
  > EOF

  $ ./driver.exe -unused-code-warnings=false -intf one_no_warn.mli
  type t = int[@@deriving one_no_warn]
  include
    sig
      [@@@ocaml.warning "-32-60"]
      module One : sig type 'a t end
      val one : Zero.t One.t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

  $ ./driver.exe -unused-code-warnings=true -intf one_no_warn.mli
  type t = int[@@deriving one_no_warn]
  include
    sig
      [@@@ocaml.warning "-32-60"]
      module One : sig type 'a t end
      val one : Zero.t One.t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

--------------------------------------------------------------------------------

When using a deriving that does not allow the warning to be enabled (such as
`one_no_warn` here), it is still possible to force it from the user side. That's
what the `force` argument for the driver flag is for. See below:

  $ ./driver.exe -unused-code-warnings=force -impl one_no_warn.ml
  type t = int[@@deriving one_no_warn]
  include
    struct
      module One = struct type 'a t =
                            | T1 of 'a  end
      let one = One.T1 zero
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Same goes for .mli files:

  $ ./driver.exe -unused-code-warnings=force -intf one_no_warn.mli
  type t = int[@@deriving one_no_warn]
  include sig module One : sig type 'a t end val one : Zero.t One.t end
  [@@ocaml.doc "@inline"][@@merlin.hide ]

--------------------------------------------------------------------------------

The alias_warn deriver is in fact an alias for two derivers:
- alias_do_warn, which is registered with unused_code_warnings=true
and derives a single `unit_one : unit` value
- alias_no_warn, which is registered with unused_code_warnings=false
and derives a single `unit_two : unit` value

For the following code:

  $ cat > alias_warn.ml << EOF
  > type t = int [@@deriving alias_warn]
  > EOF

We expect that the driver will do the right thing and disable the warning only for
unit_one:

  $ ./driver.exe -unused-code-warnings=true -impl alias_warn.ml
  type t = int[@@deriving alias_warn]
  include struct let _ = fun (_ : t) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include struct let unit_one = () end[@@ocaml.doc "@inline"][@@merlin.hide ]
  include struct let unit_two = unit_one
                 let _ = unit_two end[@@ocaml.doc "@inline"][@@merlin.hide ]

As expected, there is a let _ = unit_two but nothing for unit_one. Since unit_one does
not allow the warning 34 to be enabled, you can see that the combination of both derivers
still keeps the `let _ = fun (_ : t) -> ()` construct.

If we turn off the unused-code-warnings flag, there will be a `let _ = ...` for both:

  $ ./driver.exe -unused-code-warnings=false -impl alias_warn.ml
  type t = int[@@deriving alias_warn]
  include struct let _ = fun (_ : t) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include struct let unit_one = ()
                 let _ = unit_one end[@@ocaml.doc "@inline"][@@merlin.hide ]
  include struct let unit_two = unit_one
                 let _ = unit_two end[@@ocaml.doc "@inline"][@@merlin.hide ]

As expected, if we force the unused-code-warnings, there will be no let _ for
any of the two values, and no construct to use the type t:

  $ ./driver.exe -unused-code-warnings=force -impl alias_warn.ml
  type t = int[@@deriving alias_warn]
  include struct let unit_one = () end[@@ocaml.doc "@inline"][@@merlin.hide ]
  include struct let unit_two = unit_one end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                     ]

Same goes for .mli:

  $ cat > alias_warn.mli << EOF
  > type t = int [@@deriving alias_warn]
  > EOF

  $ ./driver.exe -unused-code-warnings=true -intf alias_warn.mli
  type t = int[@@deriving alias_warn]
  include sig val unit_one : unit end[@@ocaml.doc "@inline"][@@merlin.hide ]
  include sig [@@@ocaml.warning "-32"] val unit_two : unit end[@@ocaml.doc
                                                                "@inline"]
  [@@merlin.hide ]

  $ ./driver.exe -unused-code-warnings=false -intf alias_warn.mli
  type t = int[@@deriving alias_warn]
  include sig [@@@ocaml.warning "-32"] val unit_one : unit end[@@ocaml.doc
                                                                "@inline"]
  [@@merlin.hide ]
  include sig [@@@ocaml.warning "-32"] val unit_two : unit end[@@ocaml.doc
                                                                "@inline"]
  [@@merlin.hide ]

  $ ./driver.exe -unused-code-warnings=force -intf alias_warn.mli
  type t = int[@@deriving alias_warn]
  include sig val unit_one : unit end[@@ocaml.doc "@inline"][@@merlin.hide ]
  include sig val unit_two : unit end[@@ocaml.doc "@inline"][@@merlin.hide ]

--------------------------------------------------------------------------------

Whenever a set of types has a [@@deriving ...] attached, ppxlib's driver always
generates structure items meant to disable unused type warnings (warning 34) for
any of those types.

Let's consider the following piece of OCaml code:

  $ cat > unused_types.ml << EOF
  > type t = int
  > and u = string
  > [@@deriving zero_do_warn]
  > EOF

If we run the driver:

  $ ./driver.exe -impl unused_types.ml
  type t = int
  and u = string[@@deriving zero_do_warn]
  include struct let _ = fun (_ : t) -> ()
                 let _ = fun (_ : u) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include
    struct
      [@@@ocaml.warning "-60"]
      module Zero = struct type t =
                             | T0  end
      let zero = Zero.T0
      let _ = zero
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

We can see that the driver generated two `let _ = fun (_ : ...`, one for each type
in the set.

As we mentioned before, the driver flag (`-unused-code-warnings`) allows the
user to disable all warnings. In addition to this more general flag, we have a
flag that disables only this part, and allows unused type warnings to be reported
properly. Passing that flag to the driver should remove the two previously
mentioned items, without affecting the rest of the generated anti-warning items:

  $ ./driver.exe -unused-type-warnings=true -impl unused_types.ml
  type t = int
  and u = string[@@deriving zero_do_warn]
  include
    struct
      [@@@ocaml.warning "-60"]
      module Zero = struct type t =
                             | T0  end
      let zero = Zero.T0
      let _ = zero
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Similarly to `-unused-code-warnings`, it is possible to force disabling the generation
of this construct even when the ppx generator does not allow it.

For example, consider:

  $ ./driver.exe -impl one_no_warn.ml
  type t = int[@@deriving one_no_warn]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      module One = struct type 'a t =
                            | T1 of 'a  end
      let one = One.T1 zero
      let _ = one
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

See how `-unused-type-warnings=true` does not affect the generated code:

  $ ./driver.exe -unused-type-warnings=true -impl one_no_warn.ml
  type t = int[@@deriving one_no_warn]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      module One = struct type 'a t =
                            | T1 of 'a  end
      let one = One.T1 zero
      let _ = one
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

But if we force it, the driver omits the `let _ = fun (_ : t) -> ()`:

  $ ./driver.exe -unused-type-warnings=force -impl one_no_warn.ml
  type t = int[@@deriving one_no_warn]
  include
    struct
      [@@@ocaml.warning "-60"]
      module One = struct type 'a t =
                            | T1 of 'a  end
      let one = One.T1 zero
      let _ = one
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
