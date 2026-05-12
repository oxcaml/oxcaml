#!/usr/bin/env python3
"""Append a single line-anchored comment to a PENDING review.

Usage:
    post-pending-comment.py
        --review-id  <GraphQL node id of the pending review>
        --path       <path to a file in the PR>
        --line       <line number, must be part of the PR diff>
        --side       RIGHT | LEFT
        --body       <comment body; must start with "**Claude:**">

The body must start with `**Claude:**` so review comments authored by Claude
are visually distinct from human comments. Use `side: RIGHT` for additions
and context lines; use `side: LEFT` for deleted lines. Lines outside the PR's
diff hunks are rejected by the GitHub API.

Calls the GraphQL `addPullRequestReviewThread` mutation. Does nothing that
could submit, approve, or request changes on a review.
"""

import argparse
import subprocess
import sys

MUTATION = """
mutation($reviewId: ID!, $path: String!, $body: String!, $line: Int!, $side: DiffSide!) {
  addPullRequestReviewThread(input: {
    pullRequestReviewId: $reviewId,
    path: $path,
    body: $body,
    line: $line,
    side: $side
  }) {
    thread { id }
  }
}
"""

REQUIRED_BODY_PREFIX = "**Claude:**"


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--review-id", required=True)
    parser.add_argument("--path", required=True)
    parser.add_argument("--line", type=int, required=True)
    parser.add_argument("--side", choices=["RIGHT", "LEFT"], required=True)
    parser.add_argument("--body", required=True)
    args = parser.parse_args()

    if not args.body.startswith(REQUIRED_BODY_PREFIX):
        sys.stderr.write(
            f"Refusing: --body must start with {REQUIRED_BODY_PREFIX!r}.\n"
        )
        sys.exit(2)
    if args.line <= 0:
        sys.stderr.write(f"Refusing non-positive --line value: {args.line}\n")
        sys.exit(2)

    result = subprocess.run(
        [
            "gh", "api", "graphql",
            "-f", f"query={MUTATION}",
            "-f", f"reviewId={args.review_id}",
            "-f", f"path={args.path}",
            "-F", f"line={args.line}",
            "-f", f"side={args.side}",
            "-f", f"body={args.body}",
        ],
        capture_output=True,
        check=False,
    )
    if result.returncode != 0:
        sys.stderr.write(result.stderr.decode("utf-8", errors="replace"))
        sys.exit(result.returncode)
    sys.stdout.write(result.stdout.decode("utf-8", errors="replace"))


if __name__ == "__main__":
    main()
