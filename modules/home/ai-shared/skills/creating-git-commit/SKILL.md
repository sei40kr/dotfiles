---
name: creating-git-commit
description: Create a git commit with hunk-level staging, diff-scope verification, and sandbox-bypassed signed commit execution. Use whenever the user asks for a commit, especially when the working tree contains a mix of related and unrelated changes.
---

Create a git commit while keeping unrelated changes out of it and ensuring the commit is signed.

## Workflow

### 1. Survey the working tree

Run `git status` and `git diff --staged` (and `git diff` for unstaged changes) to enumerate everything that could end up in the commit.

### 2. Stage hunks with `git-surgeon` and verify the diff

Use the `git-surgeon` skill for hunk-level staging/unstaging/splitting. Avoid `git add .` / `git add -A` (the harness denies them) and avoid coarse `git add <file>` when a file contains a mix of related and unrelated hunks. After staging, re-run `git diff --staged` and compare against the user's stated intent — if any hunks are unrelated, surface them and confirm with the user before continuing instead of bundling unrelated work into the commit.

### 3. Commit outside the sandbox

Execute `git commit` with `dangerouslyDisableSandbox: true`. Commit signing requires access to gpg-agent / signing keys that the sandbox blocks, so a sandboxed commit would either fail or silently produce an unsigned commit.

### 4. Confirm

Run `git status` to verify the commit landed and the working tree is in the expected state.
