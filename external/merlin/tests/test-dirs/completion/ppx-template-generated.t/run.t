  $ $OCAMLC -c foo.mli
  $ $OCAMLC -c foo.ml
  $ $OCAMLC -c bar.ml
  $ $OCAMLC -c baz_intf.ml
  $ $OCAMLC -c baz.mli
  $ $OCAMLC -c baz.ml

Function to list all the completion candidates from a module
  $ complete_in_module () {
  >   module="$1"
  >   file="usage.ml"
  > 
  >   echo "let foo = ${module}." > "$file"
  >   col=$(cat "$file" | wc -c)
  > 
  >   $MERLIN single complete-prefix -prefix "${module}." -position "1:$col" -filename usage.ml < usage.ml \
  >     | jq '.value.entries | map({"name":.name, "ppx_template_generated":.ppx_template_generated})'
  > }

  $ complete_in_module Foo
  [
    {
      "name": "func",
      "ppx_template_generated": null
    },
    {
      "name": "func_gen",
      "ppx_template_generated": true
    },
    {
      "name": "value",
      "ppx_template_generated": null
    },
    {
      "name": "value_gen",
      "ppx_template_generated": true
    },
    {
      "name": "Module",
      "ppx_template_generated": null
    },
    {
      "name": "Module_gen",
      "ppx_template_generated": true
    },
    {
      "name": "t_abstract",
      "ppx_template_generated": null
    },
    {
      "name": "t_abstract_gen",
      "ppx_template_generated": true
    },
    {
      "name": "t_concrete",
      "ppx_template_generated": null
    },
    {
      "name": "t_concrete_gen",
      "ppx_template_generated": true
    }
  ]

# CR-someday: The attribute doesn't propogate through when it comes from an .ml file and
# isn't on a type. The same is true with the [@@deprecated] attribute.
  $ complete_in_module Bar
  [
    {
      "name": "func",
      "ppx_template_generated": null
    },
    {
      "name": "func_gen",
      "ppx_template_generated": null
    },
    {
      "name": "value",
      "ppx_template_generated": null
    },
    {
      "name": "value_gen",
      "ppx_template_generated": null
    },
    {
      "name": "Foo",
      "ppx_template_generated": null
    },
    {
      "name": "Foo_gen",
      "ppx_template_generated": null
    },
    {
      "name": "Module",
      "ppx_template_generated": null
    },
    {
      "name": "Module_gen",
      "ppx_template_generated": true
    },
    {
      "name": "t_abstract",
      "ppx_template_generated": null
    },
    {
      "name": "t_abstract_gen",
      "ppx_template_generated": true
    },
    {
      "name": "t_concrete",
      "ppx_template_generated": null
    },
    {
      "name": "t_concrete_gen",
      "ppx_template_generated": true
    }
  ]

  $ complete_in_module Bar.Foo
  [
    {
      "name": "func",
      "ppx_template_generated": null
    },
    {
      "name": "func_gen",
      "ppx_template_generated": true
    },
    {
      "name": "value",
      "ppx_template_generated": null
    },
    {
      "name": "value_gen",
      "ppx_template_generated": true
    },
    {
      "name": "Module",
      "ppx_template_generated": null
    },
    {
      "name": "Module_gen",
      "ppx_template_generated": true
    },
    {
      "name": "t_abstract",
      "ppx_template_generated": null
    },
    {
      "name": "t_abstract_gen",
      "ppx_template_generated": true
    },
    {
      "name": "t_concrete",
      "ppx_template_generated": null
    },
    {
      "name": "t_concrete_gen",
      "ppx_template_generated": true
    }
  ]

  $ complete_in_module Baz
  [
    {
      "name": "func",
      "ppx_template_generated": null
    },
    {
      "name": "func_gen",
      "ppx_template_generated": true
    },
    {
      "name": "value",
      "ppx_template_generated": null
    },
    {
      "name": "value_gen",
      "ppx_template_generated": true
    },
    {
      "name": "Module",
      "ppx_template_generated": null
    },
    {
      "name": "Module_gen",
      "ppx_template_generated": true
    },
    {
      "name": "t_abstract",
      "ppx_template_generated": null
    },
    {
      "name": "t_abstract_gen",
      "ppx_template_generated": true
    },
    {
      "name": "t_concrete",
      "ppx_template_generated": null
    },
    {
      "name": "t_concrete_gen",
      "ppx_template_generated": true
    }
  ]
