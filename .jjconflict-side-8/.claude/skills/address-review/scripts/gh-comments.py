#!/usr/bin/env python3
"""Fetch PR/Issue comments and reviews from GitHub REST API.

Supports fetching:
- PR review comments (line-level): gh api repos/{owner}/{repo}/pulls/{N}/comments
- Issue/PR comments (general):     gh api repos/{owner}/{repo}/issues/{N}/comments
"""

import argparse
import subprocess
import sys

ALLOWED_REPOS = {"oxcaml/oxcaml", "ocaml/ocaml"}


def fetch(endpoint: str) -> None:
    """Fetch from a GitHub REST API endpoint and print result."""
    cmd = ["gh", "api", "--paginate", endpoint]
    result = subprocess.run(cmd)
    sys.exit(result.returncode)


def main():
    parser = argparse.ArgumentParser(
        description="Fetch PR/Issue comments from GitHub REST API"
    )
    parser.add_argument(
        "--pr", type=int, help="PR number (fetches review comments from pulls/N/comments)"
    )
    parser.add_argument(
        "--issue", type=int, help="Issue/PR number (fetches general comments from issues/N/comments)"
    )
    parser.add_argument(
        "--repo",
        required=True,
        help="Repository in owner/repo format (e.g., oxcaml/oxcaml or ocaml/ocaml)",
    )
    args = parser.parse_args()

    if args.repo not in ALLOWED_REPOS:
        print(f"Error: repo must be one of: {', '.join(sorted(ALLOWED_REPOS))}", file=sys.stderr)
        sys.exit(1)

    if not args.pr and not args.issue:
        print("Error: must specify --pr or --issue", file=sys.stderr)
        sys.exit(1)

    if args.pr and args.pr <= 0:
        print("Error: PR number must be positive", file=sys.stderr)
        sys.exit(1)

    if args.issue and args.issue <= 0:
        print("Error: Issue number must be positive", file=sys.stderr)
        sys.exit(1)

    owner, repo = args.repo.split("/", 1)
    base = f"repos/{owner}/{repo}"

    if args.pr:
        fetch(f"{base}/pulls/{args.pr}/comments")
    elif args.issue:
        fetch(f"{base}/issues/{args.issue}/comments")


if __name__ == "__main__":
    main()
