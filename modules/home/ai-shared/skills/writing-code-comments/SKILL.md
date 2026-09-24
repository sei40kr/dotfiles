---
name: writing-code-comments
description: Write code comments and docstrings. Use when adding or revising explanatory text inside source files.
---

Apply `good-writing`.

- **Explain why, not what.** `// increment i by one`, or a docstring that lists the parameters, is pure cost. Why a non-obvious implementation works the way it does is not recoverable from reading it.
- **Document the contract, not the implementation.** Callers need preconditions, invariants, ordering requirements, and failure behavior. They do not need the algorithm.
- **Don't document your callers.** Where the architecture forbids a dependency, documentation must respect it too — a domain docstring must not explain how the application layer uses the function.
- **Don't mirror what can change.** A comment restating a condition, a constant, or another module's behavior becomes a lie the moment that thing changes. Describe intent, which survives refactors.
