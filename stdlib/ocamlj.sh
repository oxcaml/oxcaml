#!/usr/bin/env bash
set -euo pipefail

# input to tsort
dep_pairs=""

# Associative array
declare -A deps

# Parse ocamldep output and build dependency pairs for tsort
while IFS=': ' read -r target deps_line; do
    [[ $target == camlinternal* ]] || target="stdlib__${target^}"
    module=${target%.cmx}
    deps[$module]=""

    # Generate dependency pairs for tsort
    for dep in ${deps_line}; do
        [[ $module != {camlInternalFormatBasics,stdlib} ]] || deps_line="${deps_line//stdlib.cmx/}"
        [[ $dep == camlinternal* ]] || dep="stdlib__${dep^}"
        deps[$module]+=" $dep"

        dep_pairs+="$target $dep"$'\n'
    done
done < <(
    ocamldep -one-line -native *.ml | grep -v '^stdlib\.cmx'
    ocamldep -one-line -native -pp "awk -f ./remove_module_aliases.awk" stdlib.ml)

# Get topological order using tsort
readarray -t topo_order < <(tsort <<<"${dep_pairs}" | grep '.*[.]cmx$')

# Generate individual compilation rules
for module in "${!deps[@]}"; do
    echo "(rule"
    echo " (targets $module.cmj $module.cmjx)"
    echo " (deps"

    for dep in ${deps[$module]}; do
        case $dep in
            *.cmx)
                module=${dep%%.*}
                if [[ $module == camlInternal* ]]; then
                    module="${module}.cmj"
                fi

                echo -n " $module.cmj $module.cmjx"
                ;;
        esac
    done

    echo ")"
    echo " (action (run %{bin:ocamlj} -nopervasives -nostdlib %{deps} -c $module.ml)))"
    echo
done

# Generate archive rule in topological order
archive_modules=()
for module in "${topo_order[@]}"; do
    if [[ -n ${all_modules[$module]:-} && $module != "std_exit" ]]; then
        archive_modules+=("$module")
    fi
done

echo "(rule"
echo " (target stdlib.cmja)"
echo " (deps"
for module in "${archive_modules[@]}"; do
    echo "  $module.cmj"
done
echo " )"
echo -n " (action (run %{bin:ocamlj} -a -o stdlib.cmja"
for module in "${archive_modules[@]}"; do
    echo -n " $module.cmj"
done
echo ")))"
