---
name: nh-switch
description: "Build and apply this flake's NixOS or home-manager configuration with nh (`nh os` / `nh home`), asking for the root password through a GUI polkit dialog. Use when asked to switch, apply, rebuild, or activate the configuration."
allowed-tools:
  - Bash(hostname)
  - Bash(whoami)
  - Bash(test -f:*)
  - Bash(echo:*)
  - Bash(.claude/skills/nh-switch/scripts/nh-filtered:*)
  - Bash(journalctl -u earlyoom:*)
  - Bash(free:*)
---

# nh switch

## Targets on this host

!`h=$(hostname); u=$(whoami); test -f "hosts/$h/configuration.nix" && echo "os: $h" || echo "os: none (hosts/$h/configuration.nix missing)"; test -f "hosts/$h/users/$u/home-configuration.nix" && echo "home: $u@$h" || echo "home: none (hosts/$h/users/$u/home-configuration.nix missing)"`

Pick the target:

- `os` when the host has one. Blueprint embeds the host's home-manager users into the NixOS configuration, so `os` applies home changes too.
- `home` when the host has no `os` target, or when the user asks for home only.
- Neither: stop and tell the user.

## Run

Run every command from the repository root with the sandbox disabled; nix needs the daemon socket and network. `scripts/nh-filtered` (next to this file) appends `--no-nom`, strips nh's forced ANSI colors, and prints only nh's `>`/`!` step lines, `warning:` lines, and whole `error:`/`Error:` blocks. It ends with `nh exit=<code> full-log=<path>`; judge success by the exit code, and read the full log when the filtered output is not enough to debug.

1. Build first. It needs no user interaction, so start it right away and run it in the background:

   ```bash
   # os
   .claude/skills/nh-switch/scripts/nh-filtered os build . --diff never -o "$TMPDIR/nh-os-result"
   # home
   .claude/skills/nh-switch/scripts/nh-filtered home build . --diff never
   ```

   On failure, fix the cause and rebuild. Do not switch.

2. Switch once the build passes:

   ```bash
   # os: switch the exact closure built in step 1, without re-evaluating
   .claude/skills/nh-switch/scripts/nh-filtered --as-root os switch "$TMPDIR/nh-os-result" --diff never -R
   # home: no root needed; the build is cached, so only evaluation re-runs
   .claude/skills/nh-switch/scripts/nh-filtered home switch . --diff never
   ```

## Out of memory

Nix builds use a lot of memory, especially at high parallelism, and earlyoom is set to kill `nix` first. When a build dies abnormally (signal 9, `Killed`, or a builder or nh exiting with no `error:` explaining why), suspect memory: check `journalctl -u earlyoom --since -10min --no-pager` and `free -h`.

If memory is short, either ask the user to close processes they do not need, or rebuild with lower parallelism, e.g. `-j 1 --cores 4` (`-j` caps concurrent derivations, `--cores` caps each builder's threads).

Activation output is captured by nh and only appears in the error report on failure.

Never pass `-Q`/`--no-build-output` or `-q`: the first is not a `nix build` flag in Lix, and the second hides the step lines.
