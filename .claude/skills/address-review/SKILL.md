---
name: address-review
description: If prompted, address review comments on a GitHub PR by gathering context, categorizing by complexity, and preparing a plan.
allowed-tools: Bash(gh pr view:*), AskUserQuestion(*)
argument-hint: [PR number]
---

# PR Review Address Skill

This skill helps address review comments on a GitHub PR. It gathers context, categorizes comments by complexity, and prepares a plan for addressing them.

**Step 1: Determine the PR**

Identify which PR to work on. The PR number may be:
- Clear from the conversation context (e.g., user mentioned it earlier)
- Explicitly provided by the user
- Unknown, in which case use AskUserQuestion to ask the user for the PR number

The repository is assumed to be `oxcaml/oxcaml` unless otherwise specified.

**Step 2: Gather Context**

First, fetch high-level PR information:
```bash
gh pr view <PR> --repo oxcaml/oxcaml --json title,body,state,author,baseRefName,headRefName,commits,files | jq '{title, body, state, author: .author.login, base: .baseRefName, head: .headRefName, commits: (.commits | length), files: [.files[].path]}'
```

Then, fetch unresolved review threads:
```bash
python3 scripts/pr-comment-threads.py --pr <PR> --repo oxcaml/oxcaml
```

For each unresolved thread, understand the context by:
- Reading the `diffHunk` to see what code is being discussed
- Reading the relevant file(s) if more context is needed
- Understanding what the reviewer is asking for

**Step 3: Produce Summary and Todo Lists**

Create a brief summary of the PR (what it does, key changes).

Classify each unresolved comment thread into one of two categories:
- **Trivial**: Simple fixes like local refactorings, typos, adding comments, renaming variables, small formatting changes, or straightforward clarifications
- **Involved**: Requires deeper thought, architectural decisions, significant code changes, or discussion with the reviewer

**Step 4: Plan for Trivial Items**

For each trivial item, draft a brief plan for how to address it. This may involve:
- The specific edit to make
- Any files that need to be read first
- Revising the classification if the item turns out to be more involved than initially thought

**Step 5: Present to User**

Present the following to the user:
1. The PR summary from Step 3
2. An overview of the unresolved review threads from Step 3
3. A todo list of involved comments with explanations why they are involved.
4. A todo list of trivial comments with a plan for each item

An example of the output is provided below.

**Do NOT make any changes yet.** Wait for explicit user confirmation before addressing even the trivial items. The user may:
- Disagree with the trivial/involved classification
- Want to skip certain items
- Have additional context that changes the approach

After confirmation, implement the trivial fixes. The user will then collaborate with Claude to address the more involved items.



## Tool Use


**pr-comment-threads.py**: Fetches review comment threads (line-level discussions) using GitHub GraphQL API.
```bash
python3 scripts/pr-comment-threads.py --pr <PR> --repo oxcaml/oxcaml [--all]
```
- `--all`: Include resolved threads (default: only unresolved)

**gh-comments.py**: Fetches PR/Issue comments using GitHub REST API.
```bash
# PR review comments (line-level)
python3 scripts/gh-comments.py --pr <PR> --repo oxcaml/oxcaml

# Issue/PR general comments
python3 scripts/gh-comments.py --issue <N> --repo oxcaml/oxcaml
```

## Example Overview

Below is an example of the output to show to the user at the end of the analysis.

<summary>

## PR Summary

<!-- PR summary goes here -->

## Unresolved Review Threads

| # | File | Line | Comment | Notes |
| --- | --- | --- | --- | --- |
| 1 | file1.py | 10 | Fix typo |  |
<!-- More trivial todos go here -->

## Analysis

#### Involved Changes

**Thread 1**: Clarify the use of `foo` in `bar`. Involved, because it requires a larger refactoring of the code base.

<!-- More involved todos go here -->

#### Trivial Changes

**Thread 17**: Fix a small typo in `baz.py`. Trivial, because it's a simple edit.

My suggestion is to change pss into pass:
```python
def foo():
    pss
```
into
```python
def foo():
    pass
```

<!-- More trivial todos go here -->

## Summary

I've identified N nontrivial threads that need to be addressed together with the user. I have also identified M trivial threads that can be addressed by me right away.

- Trivial change 1
- Trivial change 2
<!-- More trivial todos go here -->

</summary>
