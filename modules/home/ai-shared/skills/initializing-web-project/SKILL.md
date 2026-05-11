---
name: initializing-web-project
description: Scaffold a new web/frontend project boilerplate with a Nix devshell (Bun + Biome by default, plus optional React/Next.js, Panda CSS, Chakra UI, react-hook-form, Vitest, Playwright, and Storybook). Use when starting a new web project or bootstrapping a React/Next.js app from scratch.
---

Scaffold a new web project boilerplate. Default to Bun + React + TypeScript + Biome, layered on top of a Nix devshell. Add other libraries only when the project needs them.

## Workflow

### 1. Confirm project shape

Ask the user (group into one or two short questions):

- **Framework**: React + Vite (SPA) or Next.js (App Router)?
- **Styling**: none, Panda CSS, or Chakra UI v3 (uses Panda CSS internally)?
- **Forms (react-hook-form)?**
- **Testing (Vitest + Playwright)?**
- **Storybook?**

Defaults: Bun (runtime + package manager), TypeScript, Biome (lint + format). No ESLint, no Prettier.

### 2. Set up the Nix devshell

In an empty project directory, run:

```bash
nix flake init -t github:numtide/blueprint#treefmt-and-git-hooks
```

Then customize:

- `nix/devshell.nix` — add `pkgs.bun` (and `pkgs.nodejs_22` only if a tool requires Node).
- `nix/treefmt.nix` — enable `biome` for JS/TS/JSON; keep `nixfmt` for Nix files.

Example `nix/treefmt.nix`:

```nix
_: {
  projectRootFile = "flake.nix";
  programs.nixfmt.enable = true;
  programs.biome.enable = true;
}
```

Verify with `nix develop`.

### 3. Initialize the JS/TS project

Run inside the project directory:

- **Next.js**: `bunx --bun create-next-app@latest . --ts --app --src-dir --use-bun --no-tailwind --no-eslint --import-alias "@/*"`
- **React + Vite**: `bun create vite . --template react-ts && bun install`

Delete any generated ESLint config (`.eslintrc*`, `eslint.config.*`) and Prettier config — Biome replaces both.

### 4. Configure Biome

```bash
bun add -D --exact @biomejs/biome
bunx biome init
```

In `biome.json`, set `vcs.enabled = true` and `vcs.useIgnoreFile = true`. Add `lint` and `format` scripts to `package.json`.

### 5. Install optional dependencies

Install only what the user picked.

| Feature | Commands |
| --- | --- |
| Panda CSS | `bun add -D @pandacss/dev` && `bunx panda init --postcss` |
| Chakra UI v3 | `bun add @chakra-ui/react @emotion/react @chakra-ui/preset-panda` (then configure `panda.config.ts` with the Chakra preset) |
| react-hook-form | `bun add react-hook-form` |
| Vitest | `bun add -D vitest @vitest/ui happy-dom @testing-library/react @testing-library/jest-dom @testing-library/user-event` |
| Playwright | `bun add -D @playwright/test` && `bunx playwright install --with-deps` |
| Storybook | `bunx storybook@latest init --package-manager bun` |

### Next.js + Chakra UI v3 gotchas

See https://chakra-ui.com/docs/get-started/frameworks/next-app. Key points:

- Add a `"use client"` `Provider` component (wrapping `ChakraProvider` + `next-themes` `ThemeProvider`) and use it in `app/layout.tsx`.
- Put `suppressHydrationWarning` on the `<html>` element — required by `next-themes` to silence the hydration warning.
- Turbopack can cause Emotion hydration mismatches. If they appear, switch `dev` and `build` scripts to use the `--webpack` flag.
- Enable `experimental.optimizePackageImports: ['@chakra-ui/react']` in `next.config.mjs` to shrink the bundle and suppress serialization warnings.
- Requires Node.js 20+ (use `pkgs.nodejs_22` in the devshell if any tool needs Node directly).

### 6. Hook up git pre-commit

The blueprint template provides `pre-commit-check`. Add a `biome check` hook in `nix/checks/pre-commit-check.nix` so JS/TS files are checked alongside Nix.

### 7. Verify

- `nix flake check`
- Inside `nix develop`: run the build (`bun run build` or `next build`) and any test scripts to confirm the project compiles.

## Notes

- Use `bun add`, `bun run`, `bunx` — never `npm`/`yarn`/`pnpm`.
- Don't pre-install Tailwind, ESLint, or Prettier; they're displaced by these defaults.
- Add to `.gitignore`: `node_modules/`, `.next/`, `dist/`, `styled-system/` (Panda output), `test-results/`, `playwright-report/`, `.bun/`.
