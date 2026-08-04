#!/usr/bin/env python3
"""Filter zdiff3 conflict diffs to show inner diffs between base and incoming.

Usage: git diff HEAD~1 | python3 zdiff3-diff-filter.py
"""

import signal
import subprocess
import sys
import os
import re
import tempfile

signal.signal(signal.SIGPIPE, signal.SIG_DFL)

ANSI_ESCAPE = re.compile(r'\033\[[0-9;]*m')
# Prefix in git-diff-via-patdiff output: \033[0;XX;30m<char>\033[0m
PREFIX_RE = re.compile(r'^\033\[0;(\d+);30m(.)\033\[0m')
# Prefix in standalone patdiff output: \033[XX;30m<char>\033[49;39m
PATDIFF_PREFIX_RE = re.compile(r'^\033\[(\d+);30m(.)\033\[49;39m')

NORMAL, IN_OXCAML, IN_BASE, IN_NEW = range(4)

PREFIX_ANSI = {
    '-': '\033[0;41;30m-\033[0m',
    '+': '\033[0;42;30m+\033[0m',
    '!': '\033[0;43;30m!\033[0m',
    ' ': '\033[0;100;30m \033[0m',
}


def extract_prefix(line):
    """Extract the patdiff prefix char and the rest of the line."""
    m = PREFIX_RE.match(line)
    if m:
        return m.group(2), line[m.end():]
    return None, line


def extract_patdiff_prefix(line):
    """Extract the prefix char and rest from standalone patdiff output."""
    m = PATDIFF_PREFIX_RE.match(line)
    if m:
        return m.group(2), line[m.end():]
    return None, line


def strip_ansi(s):
    """Remove all ANSI escape sequences."""
    return ANSI_ESCAPE.sub('', s)


def is_marker(content, marker):
    """Check if stripped content starts with a conflict marker."""
    return strip_ansi(content).startswith(marker)


def run_patdiff(base_lines, new_lines):
    """Run patdiff on base vs new content, return output lines."""
    with tempfile.NamedTemporaryFile(
        mode='w', suffix='.base', delete=False
    ) as bf, tempfile.NamedTemporaryFile(
        mode='w', suffix='.new', delete=False
    ) as nf:
        bf.write('\n'.join(base_lines))
        if base_lines:
            bf.write('\n')
        nf.write('\n'.join(new_lines))
        if new_lines:
            nf.write('\n')
        bf.flush()
        nf.flush()
        try:
            result = subprocess.run(
                ['patdiff', '-context', '99999', bf.name, nf.name],
                stdout=subprocess.PIPE, stderr=subprocess.PIPE
            )
            return result.stdout.decode('utf-8', errors='replace').splitlines()
        finally:
            os.unlink(bf.name)
            os.unlink(nf.name)


def output_transformed(conflict_lines, oxcaml_lines, base_content, new_content,
                       consistent_prefix):
    """Output the transformed conflict block with inner diff via patdiff."""
    our_prefix = PREFIX_ANSI[consistent_prefix]
    # 1. Original <<<<<<< line
    print(conflict_lines[0])
    # 2. Oxcaml content lines, indented with space to align with diff lines
    for line in oxcaml_lines:
        m = PREFIX_RE.match(line)
        if m:
            print(f'{line[:m.end()]} {line[m.end():]}')
        else:
            print(f' {line}')
    # 3. ======= separator
    print(f'{our_prefix}\033[0;1;31m=======\033[0m')
    # 4. Inner diff via patdiff (patience diff with word-level refinement)
    diff_lines = run_patdiff(base_content, new_content)
    # Skip 2 file headers + 1 hunk header (patdiff always emits these when
    # files differ; when identical, diff_lines is empty)
    for dl in diff_lines[3:]:
        pc, rest = extract_patdiff_prefix(dl)
        if pc == '-':
            print(f'{our_prefix}-{rest}')
        elif pc == '+':
            print(f'{our_prefix}+{rest}')
        elif pc == ' ':
            print(f'{our_prefix} {rest}')
        else:
            # Unexpected line (e.g. another hunk header) — pass through
            print(f'{our_prefix} {dl}')
    # 5. Original >>>>>>> line
    print(conflict_lines[-1])


def main():
    state = NORMAL
    conflict_lines = []
    oxcaml_lines = []
    base_content = []
    new_content = []
    prefixes = set()

    for raw_line in sys.stdin:
        line = raw_line.rstrip('\n')

        if state == NORMAL:
            prefix_char, rest = extract_prefix(line)
            if prefix_char in ('-', '!', '+') and is_marker(rest, '<<<<<<<'):
                state = IN_OXCAML
                conflict_lines = [line]
                oxcaml_lines = []
                base_content = []
                new_content = []
                prefixes = set()
            else:
                print(line)

        elif state == IN_OXCAML:
            conflict_lines.append(line)
            prefix_char, rest = extract_prefix(line)
            if prefix_char is not None and is_marker(rest, '|||||||'):
                prefixes.add(prefix_char)
                state = IN_BASE
            else:
                oxcaml_lines.append(line)

        elif state == IN_BASE:
            conflict_lines.append(line)
            prefix_char, rest = extract_prefix(line)
            if prefix_char is not None and is_marker(rest, '======='):
                prefixes.add(prefix_char)
                state = IN_NEW
            else:
                prefixes.add(prefix_char)
                base_content.append(strip_ansi(rest))

        elif state == IN_NEW:
            conflict_lines.append(line)
            prefix_char, rest = extract_prefix(line)
            if prefix_char is not None and is_marker(rest, '>>>>>>>'):
                prefixes.add(prefix_char)
                # Check prefix consistency
                if len(prefixes) == 1:
                    p = next(iter(prefixes))
                    if p in PREFIX_ANSI:
                        output_transformed(
                            conflict_lines, oxcaml_lines,
                            base_content, new_content, p
                        )
                    else:
                        for cl in conflict_lines:
                            print(cl)
                else:
                    for cl in conflict_lines:
                        print(cl)
                state = NORMAL
            else:
                prefixes.add(prefix_char)
                new_content.append(strip_ansi(rest))

    # Flush any incomplete conflict block at EOF
    if state != NORMAL:
        for cl in conflict_lines:
            print(cl)


if __name__ == '__main__':
    main()
