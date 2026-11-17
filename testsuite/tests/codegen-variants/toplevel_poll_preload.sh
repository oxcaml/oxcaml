#!/bin/sh

# usage: sh toplevel_poll_preload.sh
#
# Makes a library root out of the default library tree and the tree of the
# variant with the other poll-insertion setting, each with a plugin
# which.cmxs that says which of the two it is in. Runs the native toplevel
# of the default variant there with the flag of the other setting, and
# gives it the plugin by name.

set -e

. "$(dirname "$0")/poll_pair.sh"
flag=$other_flag
if [ "$flag" = -disable-poll-insertion ]; then
  # A fully polled toplevel loads code without polls with one domain only.
  OCAMLRUNPARAM=d=1
  export OCAMLRUNPARAM
fi

root=$(pwd)/root
mkdir -p "$root/variants/$other"
for entry in "$stdlib"/*; do
  case $entry in
    */variants) ;;
    *) ln -s "$entry" "$root/";;
  esac
done
for entry in "$stdlib/variants/$other"/*; do
  ln -s "$entry" "$root/variants/$other/"
done
OCAMLLIB=$root
export OCAMLLIB

echo 'let () = print_endline "the tree of the default setting"' > default.ml
echo 'let () = print_endline "the tree of the other setting"' > other.ml
echo 'let () = print_endline "only in the tree of the other setting"' \
  > only.ml
$ocamlopt -shared -o "$root/which.cmxs" default.ml
$ocamlopt $flag -shared -o "$root/variants/$other/which.cmxs" other.ml
$ocamlopt $flag -shared -o "$root/variants/$other/only.cmxs" only.ml

toplevel="$ocamlsrcdir/ocamlnat -noinit -no-version -noprompt"
echo "by name, default setting: $($toplevel which.cmxs < /dev/null 2>&1)"
echo "by name: $($toplevel $flag which.cmxs < /dev/null 2>&1)"
# A file that only the selected tree has is not found by a search of the
# default tree whose result is then looked up in the selected one.
echo "by name, in one tree: $($toplevel $flag only.cmxs < /dev/null 2>&1)"
# Without the directory of the standard library in the load path, a name is
# not found there.
nostdlib="-nostdlib -nopervasives"
echo "by name, -nostdlib:" \
  "$($toplevel $flag $nostdlib which.cmxs < /dev/null 2>&1)"
# The same for the current directory.
echo 'let () = print_endline "the current directory"' > here.ml
$ocamlopt -shared -o here.cmxs here.ml
echo "by name, in the current directory:" \
  "$($toplevel here.cmxs < /dev/null 2>&1)"
echo "by name, -nocwd: $($toplevel -nocwd here.cmxs < /dev/null 2>&1)"
echo "by path, -nocwd: $($toplevel -nocwd ./here.cmxs < /dev/null 2>&1)"
# The directory that the toplevel adds for camlp4 is one of the standard
# library, which -nostdlib leaves out.
mkdir "$root/variants/$other/camlp4"
echo 'let () = print_endline "the directory for camlp4"' > extension.ml
$ocamlopt $flag -shared -o "$root/variants/$other/camlp4/extension.cmxs" \
  extension.ml
echo "by name, in +camlp4: $($toplevel $flag extension.cmxs < /dev/null 2>&1)"
echo "by name, in +camlp4, -nostdlib:" \
  "$($toplevel $flag $nostdlib extension.cmxs < /dev/null 2>&1)"
