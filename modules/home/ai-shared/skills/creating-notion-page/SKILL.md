---
name: creating-notion-page
description: Create a Notion page via the Notion MCP, defaulting the destination to the user's private space and reviewing the final page from a first-time reader's perspective. Use whenever the user asks to create, draft, or write a Notion page.
allowed-tools:
  - mcp__claude_ai_Notion__notion-create-pages
---

Create a Notion page using `mcp__claude_ai_Notion__notion-create-pages`, then review the result as if reading it for the first time.

## Workflow

### 1. Decide the parent

Default to the user's private space — set `parent` to `null` in the create call. Only pick a non-null parent when the user explicitly names a destination (a specific page, database, or team space). If the request is ambiguous, ask before placing the page in a shared location; creating a page privately and moving it later is cheaper than pulling a misfiled page back from a shared space.

### 2. Shape the content into a document

Source material is often conversational — pasted Slack threads, chat logs, meeting notes. Don't preserve that shape in the page. Distill it into a proper document: drop turn-taking, attributions, and chatter; reorganize by topic; rewrite as prose, headings, and lists that a reader can scan.

For references inside the page:

- People → use Notion's user mention (not plain-text names)
- Other ADRs / Notion pages → use Notion's page mention (not pasted URLs or plain-text titles)

### 3. Self-review the draft as a first-time reader

Before posting, re-read the draft from the perspective of someone who has never seen the original request. The instructions used to create the page are invisible to readers, so context that "feels obvious" from the prompt side is missing for them.

Check that the draft itself supplies, without relying on the prompt:

- Background a fresh reader needs to follow the topic
- Definitions for terms or acronyms introduced
- The "why" behind decisions or recommendations, not just the "what"
- Anything implied by the request but never written down in the page
- Source material has been distilled into document form — no leftover chat/Slack shape
- People and page references use Notion mentions, not plain text

Revise the draft until it passes. Don't post a page you'd need to immediately fix.

### 4. Create the page

Call `mcp__claude_ai_Notion__notion-create-pages` with the chosen parent and the reviewed content.
