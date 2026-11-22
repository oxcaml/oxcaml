#!/bin/sh

# usage: sh toplevel_poll_preload.sh
#
# Makes a library root out of the default library tree and the tree of the
# variant with the other poll-insertion setting, each with a plugin
# which.cmxs that says which of the two it is in. Runs the native toplevel
# of the default variant there with the flag of the other setting, and
# gives it the plugin by name and by path.

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
echo "by path: $($toplevel $flag "$root/which.cmxs" < /dev/null 2>&1)"
# A file that only the selected tree has is not found by a search of the
# default tree whose result is then looked up in the selected one.
echo "by name, in one tree: $($toplevel $flag only.cmxs < /dev/null 2>&1)"
# Without the directory of the standard library in the load path, a name is
# not found there, and a path still names its file.
nostdlib="-nostdlib -nopervasives"
echo "by name, -nostdlib:" \
  "$($toplevel $flag $nostdlib which.cmxs < /dev/null 2>&1)"
echo "by path, -nostdlib:" \
  "$($toplevel $flag $nostdlib "$root/which.cmxs" < /dev/null 2>&1)"
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

# In a directory of the library root that the selected tree also has, the
# current directory is its counterpart in that tree: for a file of the
# command line as for a directive. So a file that only the counterpart has
# is found, and one that only the directory itself has is not.
here=$(pwd)
mkdir "$root/pkg" "$root/variants/$other/pkg"
echo 'let () = print_endline "in the counterpart of the directory"' \
  > counterpart.ml
$ocamlopt $flag -shared -o "$root/variants/$other/pkg/counterpart.cmxs" \
  counterpart.ml
cp here.cmxs "$root/pkg/original.cmxs"
for file in counterpart.cmxs original.cmxs; do
  echo "in a directory of the library root, $file by name:" \
    "$(cd "$root/pkg" && $toplevel $flag $file < /dev/null 2>&1)"
  echo "in a directory of the library root, $file by directive:" \
    "$(cd "$root/pkg" \
       && echo "#load \"$file\";;" | $toplevel $flag 2>&1)"
done
# Outside the library root the entry is the current directory itself,
# whichever that is: after #cd, the one changed to. Inside, it is what it
# was when the toplevel started, and #directory names the directory.
mkdir elsewhere
echo "after #cd between directories outside the library root:" \
  "$(cd elsewhere \
     && printf '#cd "%s";;\n#load "here.cmxs";;\n' "$here" \
        | $toplevel $flag 2>&1)"
# The other entries stay what they are: started in the library root with a
# script elsewhere, the toplevel still finds a file of the standard library
# after the script has left the root.
printf '#cd "%s";;\n#load "which.cmxs";;\n' "$here" > leave.ml
echo "after #cd out of the library root, by a script:" \
  "$(cd "$root" && $toplevel $flag "$here/leave.ml" 2>&1)"
echo "after #cd into the library root:" \
  "$(printf '#cd "%s";;\n#directory ".";;\n#load "counterpart.cmxs";;\n' \
       "$root/pkg" \
     | $toplevel $flag 2>&1)"

# The toplevel of the other variant, started in the library root: its
# current directory and its standard library are then one directory, and
# -nocwd only takes away the entry for the first.
host="$ocamlsrcdir/testsuite/tools/variants/$other/ocamlnat"
echo "-nocwd in the directory of the standard library:" \
  "$(cd "$root" && echo 'let () = print_endline "still found";;' \
     | $host -noinit -no-version -noprompt -nocwd 2>&1)"
