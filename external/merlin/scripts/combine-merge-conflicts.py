#!/usr/bin/env python3
"""
This script makes nested merge conflicts easier to read by merging together
outer conflicts such that the inner conflicts are not broken up between them.

The result is written to the file given by --output, or back over the input
file if there isn't one given. This script is meant to be invoked by
./import-ocaml-source.sh.

If any merge conflicts are malformed, the script will exit with code 21 without
writing to file.

See the tests ../tests/test-units/combine-merge-conflicts for examples.
"""

import argparse
from enum import Enum
import enum
import functools
import io
import re
import sys
from typing import (
    Iterable,
    List,
    NamedTuple,
    Optional,
    Pattern,
    TextIO,
    Tuple,
    Union,
)


def malformed_merge_conflict() -> None:
    exit(21)


class ConflictKind(Enum):
    OUTER = enum.auto()
    """
    An outer merge conflict. This is a merge conflict originating from the
    import script, not from git.
    """

    INNER = enum.auto()
    """
    An outer merge conflict. This is a merge conflict originating from git, not
    from the import script.
    """

    def marker_length(self: "ConflictKind") -> int:
        if self is ConflictKind.OUTER:
            return 14
        elif self is ConflictKind.INNER:
            return 7
        else:
            raise ValueError(f"unknown conflict kind: {self}")


@functools.lru_cache(maxsize=None)
def conflict_marker_re(char: str, conflict_kind: ConflictKind) -> Pattern[str]:
    """
    Matches a conflict marker of the given character and kind, capturing the
    marker's label (if any) in the `label` group.
    """
    # The newline at the end of the line is optional so that a marker on the
    # last line of a file with no trailing newline is still recognized.
    length = conflict_kind.marker_length()
    return re.compile(rf"{re.escape(char)}{{{length}}}(?: (?P<label>[^\n]*))?\n?")


class MergeConflict(NamedTuple):
    current_lines: List[str]
    merge_base_lines: List[str]
    incoming_lines: List[str]

    current_label: str
    merge_base_label: str
    incoming_label: str


File = List[Union[str, MergeConflict, None]]


class ConflictState(Enum):
    OUTSIDE_CONFLICT = enum.auto()
    IN_CURRENT = enum.auto()
    IN_MERGE_BASE = enum.auto()
    IN_INCOMING = enum.auto()


class SawStartLabel(NamedTuple):
    current_label: str


class SawMergeBaseLabel(NamedTuple):
    merge_base_label: str


class SawIncomingLabel(NamedTuple):
    pass


class SawEndLabel(NamedTuple):
    incoming_label: str


ConflictStateTransition = Union[
    SawStartLabel, SawMergeBaseLabel, SawIncomingLabel, SawEndLabel
]


def get_transition(
    conflict_kind: ConflictKind, line: str
) -> Optional[ConflictStateTransition]:
    match = conflict_marker_re("<", conflict_kind).fullmatch(line)
    if match is not None:
        current_label = match["label"] or ""
        return SawStartLabel(current_label)

    match = conflict_marker_re("|", conflict_kind).fullmatch(line)
    if match is not None:
        merge_base_label = match["label"] or ""
        return SawMergeBaseLabel(merge_base_label)

    match = conflict_marker_re("=", conflict_kind).fullmatch(line)
    if match is not None:
        return SawIncomingLabel()

    match = conflict_marker_re(">", conflict_kind).fullmatch(line)
    if match is not None:
        incoming_label = match["label"] or ""
        return SawEndLabel(incoming_label)

    return None


def transition_conflict_state(
    old_state: ConflictState, conflict_kind: ConflictKind, line: str
) -> Optional[Tuple[ConflictState, ConflictStateTransition]]:
    transition = get_transition(conflict_kind, line)
    if transition is None:
        return None
    else:
        if (
            isinstance(transition, SawStartLabel)
            and old_state is ConflictState.OUTSIDE_CONFLICT
        ):
            new_state = ConflictState.IN_CURRENT
        elif (
            isinstance(transition, SawMergeBaseLabel)
            and old_state is ConflictState.IN_CURRENT
        ):
            new_state = ConflictState.IN_MERGE_BASE
        elif isinstance(transition, SawIncomingLabel) and (
            old_state is ConflictState.IN_MERGE_BASE
            or (
                old_state is ConflictState.IN_CURRENT
                and conflict_kind is ConflictKind.INNER
            )
        ):
            new_state = ConflictState.IN_INCOMING
        elif (
            isinstance(transition, SawEndLabel)
            and old_state is ConflictState.IN_INCOMING
        ):
            new_state = ConflictState.OUTSIDE_CONFLICT
        else:
            malformed_merge_conflict()

        return new_state, transition


def parse_file(raw_file: Iterable[str]) -> File:
    file = []

    current_state: ConflictState = ConflictState.OUTSIDE_CONFLICT

    current_lines = []
    merge_base_lines = []
    incoming_lines = []
    current_label = ""
    merge_base_label = ""

    for line in raw_file:
        transition = transition_conflict_state(current_state, ConflictKind.OUTER, line)
        if transition is None:
            if current_state is ConflictState.OUTSIDE_CONFLICT:
                file.append(line)
            elif current_state is ConflictState.IN_CURRENT:
                current_lines.append(line)
            elif current_state is ConflictState.IN_MERGE_BASE:
                merge_base_lines.append(line)
            elif current_state is ConflictState.IN_INCOMING:
                incoming_lines.append(line)
            else:
                raise ValueError(f"unknown state: {current_state}")
        else:
            current_state, transition = transition
            if isinstance(transition, SawStartLabel):
                current_label = transition.current_label
            elif isinstance(transition, SawMergeBaseLabel):
                merge_base_label = transition.merge_base_label
            elif isinstance(transition, SawIncomingLabel):
                pass
            elif isinstance(transition, SawEndLabel):
                incoming_label = transition.incoming_label
                file.append(
                    MergeConflict(
                        current_lines=current_lines,
                        merge_base_lines=merge_base_lines,
                        incoming_lines=incoming_lines,
                        current_label=current_label,
                        merge_base_label=merge_base_label,
                        incoming_label=incoming_label,
                    )
                )

                current_lines = []
                merge_base_lines = []
                incoming_lines = []
                current_label = ""
                merge_base_label = ""

    if current_state is not ConflictState.OUTSIDE_CONFLICT:
        malformed_merge_conflict()

    return file


def combine_conflicts(file: File) -> File:
    new_file = []

    def transition_inner(old_state: ConflictState, line: str) -> ConflictState:
        transition = transition_conflict_state(old_state, ConflictKind.INNER, line)
        if transition is None:
            return old_state
        else:
            return transition[0]

    # These are the states of the inner conflicts within the
    # current/merge-base/incoming sides of the outer conflicts.
    outer_current_state = ConflictState.OUTSIDE_CONFLICT
    outer_merge_base_state = ConflictState.OUTSIDE_CONFLICT
    outer_incoming_state = ConflictState.OUTSIDE_CONFLICT

    # An outer merge conflict whose inner conflicts haven't yet been terminated.
    # When it's non-none, we add lines we see into this conflict until the inner
    # conflict(s) is terminated.
    current_outer_conflict = None

    # If there is no current outer conflict, we add the lines of an inner
    # conflict here. That way if we hit an outer conflict before the inner
    # conflict closes, we can move the inner conflict into the outer conflict.
    current_inner_conflict = []

    def maybe_flush_current_outer_conflict():
        nonlocal current_outer_conflict
        nonlocal outer_current_state
        nonlocal outer_merge_base_state
        nonlocal outer_incoming_state
        nonlocal new_file
        if (
            current_outer_conflict is not None
            and outer_current_state is ConflictState.OUTSIDE_CONFLICT
            and outer_merge_base_state is ConflictState.OUTSIDE_CONFLICT
            and outer_incoming_state is ConflictState.OUTSIDE_CONFLICT
        ):
            new_file.append(current_outer_conflict)
            current_outer_conflict = None

    for element in file:
        if isinstance(element, str):
            line = element

            outer_current_state = transition_inner(outer_current_state, line)
            outer_merge_base_state = transition_inner(outer_merge_base_state, line)
            outer_incoming_state = transition_inner(outer_incoming_state, line)

            if current_outer_conflict is None:
                if outer_current_state is ConflictState.OUTSIDE_CONFLICT:
                    new_file += current_inner_conflict
                    current_inner_conflict = []
                    new_file.append(line)
                else:
                    current_inner_conflict.append(line)
            else:
                current_outer_conflict.current_lines.append(line)
                current_outer_conflict.merge_base_lines.append(line)
                current_outer_conflict.incoming_lines.append(line)

            maybe_flush_current_outer_conflict()

        elif isinstance(element, MergeConflict):
            next_conflict = element
            if current_outer_conflict is None:
                current_outer_conflict = MergeConflict(
                    current_lines=current_inner_conflict[:],
                    merge_base_lines=current_inner_conflict[:],
                    incoming_lines=current_inner_conflict[:],
                    current_label=next_conflict.current_label,
                    merge_base_label=next_conflict.merge_base_label,
                    incoming_label=next_conflict.incoming_label,
                )
                current_inner_conflict = []

            for line in next_conflict.current_lines:
                outer_current_state = transition_inner(outer_current_state, line)
                current_outer_conflict.current_lines.append(line)
            for line in next_conflict.merge_base_lines:
                outer_merge_base_state = transition_inner(outer_merge_base_state, line)
                current_outer_conflict.merge_base_lines.append(line)
            for line in next_conflict.incoming_lines:
                outer_incoming_state = transition_inner(outer_incoming_state, line)
                current_outer_conflict.incoming_lines.append(line)

            maybe_flush_current_outer_conflict()

        elif element is None:
            pass

        else:
            raise ValueError(f"unexpected file element: {element}")

    if current_outer_conflict is not None:
        new_file.append(current_outer_conflict)

    if (
        outer_current_state is not ConflictState.OUTSIDE_CONFLICT
        or outer_merge_base_state is not ConflictState.OUTSIDE_CONFLICT
        or outer_incoming_state is not ConflictState.OUTSIDE_CONFLICT
    ):
        malformed_merge_conflict()

    return new_file


def print_marker(
    out: TextIO, conflict_kind: ConflictKind, char: str, label: Optional[str]
) -> None:
    length = conflict_kind.marker_length()
    maybe_label = "" if not label else f" {label}"
    out.write(f"{char * length}{maybe_label}\n")


def print_file(out: TextIO, file: File) -> None:
    for element in file:
        if isinstance(element, str):
            line = element
            out.write(line)
        elif isinstance(element, MergeConflict):
            conflict = element
            print_marker(out, ConflictKind.OUTER, "<", conflict.current_label)
            for line in conflict.current_lines:
                out.write(line)
            print_marker(out, ConflictKind.OUTER, "|", conflict.merge_base_label)
            for line in conflict.merge_base_lines:
                out.write(line)
            print_marker(out, ConflictKind.OUTER, "=", None)
            for line in conflict.incoming_lines:
                out.write(line)
            print_marker(out, ConflictKind.OUTER, ">", conflict.incoming_label)
        elif element is None:
            pass
        else:
            raise ValueError(f"unexpected file element: {element}")


def read_file(path: str) -> List[str]:
    with open(path, encoding="utf-8") as file:
        return file.readlines()


def write_file(path: str, text: str) -> None:
    with open(path, "w", encoding="utf-8", newline="\n") as file:
        file.write(text)


def combine_lines(raw_file: Iterable[str], out: TextIO) -> None:
    file = parse_file(raw_file)
    file = combine_conflicts(file)
    print_file(out, file)


def main(path: str, output_path: Optional[str] = None) -> None:
    output_path = path if output_path is None else output_path
    out = io.StringIO()
    combine_lines(read_file(path), out)
    write_file(output_path, out.getvalue())


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "file",
        help="the file with nested merge conflicts to combine",
    )
    parser.add_argument(
        "-o",
        "--output",
        help="write the result here instead of overwriting the input file",
    )
    args = parser.parse_args()

    main(args.file, output_path=args.output)
