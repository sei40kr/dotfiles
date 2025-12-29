---
name: hatena-blog-writer
description: Write or refine Hatena Blog posts in Japanese
---

Write Hatena Blog posts in Japanese based on the given topic or draft.

## Writing Style

- Use formal Japanese (だ/である体)
- Prefer technical terminology
- Preserve provocative tone level from drafts when intentionally used

## Content Guidelines

- Fact-check information; consult before changing claims
- Preserve original arguments unless factually incorrect
- Reorder content for clarity when needed
- Add examples and web statistics from credible sources
- For statistics, use placeholders with HTML TODO comments for human verification

## Formatting

- Start headings at level 1, no numbering
- Do not use bold text (**text**) as headings
- Use bold text to emphasize important points
- Use horizontal rules only within sections
- Use tables for comparisons, not definition lists
- For definition lists, use HTML tags (`<dl>`, `<dt>`, `<dd>`) or Markdown lists
- Add English translations in parentheses for important technical terms (lowercase for regular words, uppercase for acronyms when it aids clarity)
- Use half-width parentheses with spaces before and after
- Do not use Japanese sentence-ending periods (。) inside parentheses

## Output

- Suggest up to 5 tags for the article
- List any fact-checking concerns or statistics that need human verification

## Math Syntax

- Inline: `[tex: ~]` (add spaces before and after)
- Display: `<div style="text-align: center">[tex: \displaystyle ~]</div>`
