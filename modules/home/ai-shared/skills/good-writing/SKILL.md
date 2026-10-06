---
name: good-writing
description: Write concise, precise prose. Use when authoring or revising text a person will read — docs, PR descriptions, commit messages, blog posts.
---

## Draft

- **Don't let the request shape the text.** You will default to echoing the prompt and answering it. The reader never saw the prompt — write for someone meeting the subject cold.
- **Lead with the conclusion.** Readers stop early. Put the answer first, the support after.
- **Use the right term, and the same one throughout.** Work out the concept, then look up what the codebase or the field already calls it — its ubiquitous language. The request may name it loosely, and alternating between synonyms reads as two different things.
- **Don't invent your own translation.** When the source and the output are in different languages, terms and proper nouns usually have an established form — look it up rather than rendering it yourself.
- **Shorter wins at equal information.** Cut filler, hedging, and restatement. Never pad to look thorough.

### When the output is Japanese

If the `yomiyasu` skill is available, write under it from the first sentence rather than polishing afterward — Japanese AI prose fails in ways the rules above don't name.

- **Return the prose alone.** Its 変えたところ and 書き手に確かめたい点 sections belong to critiquing a draft someone handed over, not to text you are authoring.

## Then cut

Re-read and delete every sentence that fails a rule — if removing it loses no understanding, it was noise.
