---
name: oxcaml-pr-review
description: Review an oxcaml GitHub PR by posting line-anchored comments to a pending review, without submitting it.
allowed-tools: Bash(gh pr view:*), Bash(gh pr diff:*), Bash(python3 .claude/skills/oxcaml-pr-review/scripts/find-or-create-pending-review.py:*), Bash(python3 .claude/skills/oxcaml-pr-review/scripts/post-pending-comment.py:*), Bash(python3 .claude/skills/oxcaml-pr-review/scripts/list-review-comments.py:*), AskUserQuestion(*)
argument-hint: [PR number]
---

# OxCaml PR Review Skill

This skill walks through reviewing a pull request on `oxcaml/oxcaml`. The output is a set of line-anchored comments attached to a **pending** review on GitHub. The user reads those comments in place on GitHub and decides what to keep, edit, or submit.

The repository is `oxcaml/oxcaml` unless the user says otherwise.

The skill uses three helper scripts under `.claude/skills/oxcaml-pr-review/scripts/`. They are the only way this skill writes to GitHub — direct `gh api` calls are not allowed by this skill's permissions, so use the scripts.

## Step 1: Identify the PR

The PR number may be clear from conversation context, explicitly provided as an argument, or unknown. If unknown, use `AskUserQuestion` to ask for it.

## Step 2: Gather

Fetch the PR metadata and diff. If the diff is large, write it locally (using a distinctive filename like `pr-NNNN.diff`) so it stays out of your context window.

```bash
gh pr view NNNN --repo oxcaml/oxcaml
gh pr diff NNNN --repo oxcaml/oxcaml          # or redirect to claude-ws if large
```

## Step 3: Read source at HEAD

For each location you plan to comment on, read the affected file at HEAD so you have correct line numbers and surrounding context. The diff alone is not enough — line numbers shift, and you need to see what's actually around the change. Check that the local repo is checked out to the PR branch, ensuring that HEAD lines up with the PR.

## Step 4: Compose

Draft the review as a list of **line-anchored** comments rather than a wall of prose. Each comment should be:

- Tied to a specific file and line that appears in the PR diff
- Specific enough that the user can act on it without re-reading the diff
- Focused on a single point — split unrelated observations into separate comments

It's fine to have few comments, or none, if the diff is clean. Quality over quantity.

## Step 5: Post as pending

### Find or create the pending review

```bash
python3 .claude/skills/oxcaml-pr-review/scripts/find-or-create-pending-review.py \
  --pr NNNN
```

Output is a JSON object `{"node_id": "...", "rest_id": ...}`. The script reuses an existing PENDING review if one exists, or creates a fresh one in PENDING state. It never submits.

### Append each comment

Use `post-pending-comment.py` once per comment. The script enforces that every body starts with `**Claude:**` (the convention that lets the user tell your comments apart from theirs) and that `--line` is positive and `--side` is `RIGHT` or `LEFT`.

```bash
python3 .claude/skills/oxcaml-pr-review/scripts/post-pending-comment.py \
  --review-id "<node_id>" \
  --path "<file>" \
  --line <line> \
  --side RIGHT \
  --body '**Claude:** <comment>'
```

Notes:
- `--line` must be a line that's part of the PR diff. Use `--side RIGHT` for additions and context lines; use `--side LEFT` for deleted lines. Lines outside the diff hunks are rejected by the GitHub API.
- Independent comment posts can run in parallel — issue them in one batch of tool calls.

### Verify

```bash
python3 .claude/skills/oxcaml-pr-review/scripts/list-review-comments.py \
  --pr NNNN --review-id <rest_id>
```

Read-only; confirms that the comments you posted are all attached to the same pending review.

## Step 6: Do NOT submit

**Leave the review in PENDING state.** This skill provides no way to submit, approve, or request changes on a review, and it should not. The user reads the comments in place on GitHub and submits — or edits/deletes them — themselves.

When you finish, tell the user how many comments you posted and the URL of the PR. Do not write a long prose summary of the review; the comments themselves are the deliverable.
