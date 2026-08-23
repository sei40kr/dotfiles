---
name: next-dev
description: Verify Next.js code changes without colliding with the user's running dev server. Use when checking that an edit works in a Next.js project — visual checks, type checks, or static-export verification — while `next dev` is (or may be) running.
---

Verify a change in a Next.js project without breaking the dev server the user already has running.

## The core hazard

`next dev` and `next build` both own `.next` (and, with `output: "export"`, the export dir `out/`). Whichever starts second clobbers the other's `.next`, and the dev server then serves 500s until restarted. **A different port does not help.** A custom `distDir` isolates them **only when the project is not a static export** — with `output: "export"`, Next forces `distDir` back to `.next`, so the two collide no matter what.

So the user's dev server (usually on `localhost:3000`) and any `next build`/`next dev` you launch in the repo root cannot coexist. Never launch either in the repo root while their server may be up.

Rules, always:

- **Never kill or restart the user's dev server.** If it's down or erroring, tell them and let them restart it.
- **Never run `next build` or `next dev` in the repo root.** Use one of the paths below instead.

## Choose the lightest sufficient check

### 1. Visual / behavioral check — use the running dev server

Edits hot-reload, so just hit the server the user already has up. First confirm it's listening:

```sh
curl -s http://localhost:3000/<the-route-you-changed> | head
```

If it's not listening or returns 500, stop — tell the user and let them (re)start it. Don't start your own.

For anything needing a real browser (rendered DOM, client interaction, screenshots), drive that same running server with the `agent-browser` skill — never spin up a competing server.

### 2. Build verification — isolate the build's output

When you must actually run `next build` (export-only failures like `generateStaticParams`, `dynamicParams`, non-ASCII route directories), keep its output away from the running server's `.next`. How depends on whether the project is a static export:

**Not a static export** — give the build its own `distDir`. `distDir` is a `next.config` option, not a built-in env var, and it can't leave the project dir — so have the config read an env var and point the build at a sibling folder:

```js
// next.config.js
module.exports = { distDir: process.env.NEXT_DISTDIR ?? ".next" };
```

```sh
NEXT_DISTDIR=.next-verify next build   # via the project's package runner
```

The running dev server was started without the var, so it keeps `.next`; your build lands in `.next-verify`. Gitignore that folder. If the config doesn't already read an env var, adding it edits `next.config` — which restarts the dev server once — so prefer this only when the config is already set up for it; otherwise use the isolated-copy method below.

**`output: "export"`** — a custom `distDir` can't help (export forces it back to `.next`), so build from a copy outside the repo instead:

```sh
SRC=$PWD
DIR=<scratchpad>/build-check           # any dir outside the repo
mkdir -p "$DIR"
rsync -a --delete --exclude node_modules --exclude .next --exclude out \
      --exclude .git "$SRC/" "$DIR/"
cp -al "$SRC/node_modules" "$DIR/node_modules"   # hardlink, not symlink:
                                                 # Turbopack rejects a symlinked node_modules
(cd "$DIR" && next build)                         # via the project's package runner
```

Keep the dir between runs — `rsync --delete` re-syncs it and Next reuses its cache. To confirm the isolation held, check `.next/required-server-files.json`: its `appDir` must point at the copy, not the repo.
