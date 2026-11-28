#!/usr/bin/env python3
"""List comments on a specific review of a PR (read-only).

Usage:
    list-review-comments.py --pr N --review-id REST_ID [--repo OWNER/REPO]

Arguments:
    --pr         PR number (positive integer).
    --review-id  REST numeric id of the review (from find-or-create-pending-review.py).
    --repo       GitHub repo in OWNER/REPO form. Defaults to oxcaml/oxcaml.

Output:
    JSON list (as returned by GitHub) of comments belonging to the review,
    forwarded to stdout. Use to verify that comments posted by
    post-pending-comment.py landed on the expected pending review.
"""

import argparse
import re
import subprocess
import sys

REPO_RE = re.compile(r"^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pr", type=int, required=True)
    parser.add_argument("--review-id", type=int, required=True)
    parser.add_argument("--repo", default="oxcaml/oxcaml")
    args = parser.parse_args()

    if not REPO_RE.match(args.repo):
        sys.stderr.write(f"Refusing suspicious --repo value: {args.repo!r}\n")
        sys.exit(2)
    if args.pr <= 0:
        sys.stderr.write(f"Refusing non-positive --pr value: {args.pr}\n")
        sys.exit(2)
    if args.review_id <= 0:
        sys.stderr.write(
            f"Refusing non-positive --review-id value: {args.review_id}\n"
        )
        sys.exit(2)

    endpoint = f"repos/{args.repo}/pulls/{args.pr}/reviews/{args.review_id}/comments"
    result = subprocess.run(
        ["gh", "api", endpoint],
        capture_output=True,
        check=False,
    )
    if result.returncode != 0:
        sys.stderr.write(result.stderr.decode("utf-8", errors="replace"))
        sys.exit(result.returncode)
    sys.stdout.write(result.stdout.decode("utf-8", errors="replace"))


if __name__ == "__main__":
    main()
