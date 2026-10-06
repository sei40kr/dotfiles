---
name: git-commit
description: Create a git commit with hunk-level staging, diff-scope verification, and sandbox-bypassed signed commit execution. Use whenever the user asks for a commit, especially when the working tree contains a mix of related and unrelated changes.
allowed-tools:
  - Bash(git status:*)
  - Bash(git diff:*)
  - Bash(git log:*)
  - Bash(git-surgeon hunks:*)
  - Bash(git-surgeon stage:*)
  - Bash(git-surgeon unstage:*)
  - Bash(git-surgeon split:*)
  - Bash(git-surgeon amend:*)
  - Bash(git-surgeon squash:*)
  - Bash(git-surgeon fold:*)
  - Bash(git-surgeon reword:*)
  - Bash(git commit:*)
---

Keep unrelated changes out of the commit, and make sure it is signed.

## 1. Survey the working tree

`git status`, `git diff`, `git diff --staged` — enumerate everything that could land in the commit.

## Fold into unpushed commits

If the change belongs with a commit that is not pushed yet (`git log @{upstream}..`), amend or squash it in with the `git-surgeon` skill instead of stacking a new commit on top — and revisit that commit's message, since the fold may change what it does.

## 2. Stage hunks and verify

Stage with the `git-surgeon` skill. `git add .` / `git add -A` are denied by the harness, and plain `git add <file>` is too coarse when a file mixes related and unrelated hunks. Re-run `git diff --staged` and compare against the user's intent; if anything unrelated crept in, surface it and confirm before continuing.

## 3. Write the message

Use the `good-writing` skill. Describe the staged diff, not the request. The reader has no access to the conversation — drop task framing and context that lives only in the prompt.

## 4. Commit outside the sandbox

Run `git commit` with `dangerouslyDisableSandbox: true`. Signing needs gpg-agent, which the sandbox blocks — inside it the commit fails or silently lands unsigned.

## 5. Confirm

`git status`, to verify the commit landed and the tree is in the expected state.
