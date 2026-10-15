---
name: update-nixpkgs
description: "Update nixpkgs: either bump the lock to the latest commit on the current branch, or upgrade to a new NixOS release (e.g. 25.11 -> 26.05) along with home-manager and nix-darwin. Use when asked to update, bump, or upgrade nixpkgs or the NixOS release."
---

# Update nixpkgs

Two kinds of update, chosen by what the user asks for:

- **Bump**: move `nixpkgs` to the latest commit of the branch `flake.nix` already tracks. Only `flake.lock` changes.
- **Release upgrade**: switch to a new release branch. `flake.nix`, `flake.lock`, and every module hit by a breaking change change.

`nixpkgs-unstable` is a separate input. Touch it only when the user names it.

## Bump

1. `nix flake update nixpkgs`
2. Build (see [Build](#build)).
3. Commit with the `commit` skill: `feat: update nixpkgs`.
4. [Drop obsolete workarounds](#drop-obsolete-workarounds).

## Release upgrade

1. In `flake.nix`, move these refs to the new release together; they must match:
   - `nixpkgs`: `nixos-YY.MM`. Use `release-YY.MM` only when a fix you need has not reached the channel yet, and say why in the commit body.
   - `home-manager`: `release-YY.MM`
   - `nix-darwin`: `nix-darwin-YY.MM`
2. `nix flake update nixpkgs home-manager nix-darwin`
3. Build and fix every error and evaluation warning until the build passes. Typical breakage:
   - packages renamed or moved to top level (`nixfmt-rfc-style` -> `nixfmt`, `nodePackages.*` -> top level, `xorg.xset` -> `xset`)
   - packages removed or marked insecure (`nodejs_20`)
   - options renamed or removed (`services.ollama.acceleration`, `programs.aerospace.userSettings` -> `settings`)

   Find the replacement in the error message, the nixpkgs release notes, or the home-manager news rather than guessing.
4. Commit with the `commit` skill: `feat: update nixpkgs and home-manager to YY.MM`. List each compat fix in the body as `old -> new`, with the reason when the rename is not self-explanatory:

   ```
   Compat fixes pulled in by the bump:

   - nodePackages.* -> top-level (removed in 26.05)
   - nodejs_20 -> nodejs_22 (20 marked insecure)
   ```

5. [Drop obsolete workarounds](#drop-obsolete-workarounds).

## Drop obsolete workarounds

The new nixpkgs may already ship what a local workaround provides. Check each one against the updated nixpkgs:

- `packages/*`: nixpkgs now packages it, at the same or a newer version.
- `overlays/*`: the patch, fix, or version bump is in nixpkgs now.
- `overrideAttrs`, `.override`, and the other `override*` functions, wherever they appear: same as overlays. Search the whole repository, since modules and packages call them inline: `git grep -n -e overrideAttrs -e '\.override' -e overridePythonAttrs -e overrideDerivation -- '*.nix'`. Skip overrides that only pick options (`google-fonts.override { fonts = [ ... ]; }`).
- Substitutes for a broken package, usually marked `FIXME` or `TODO` (e.g. `julia-stable-bin` in `modules/home/jupyter/ijulia.nix`): the original builds again.

A workaround's comment often names the upstream issue or PR the workaround waits for; check whether it has landed in the new revision. Remove what is obsolete, build again, and commit each removal separately with the `commit` skill, e.g. `chore(overlays): remove certmgr-selfsigned overlay as upstream issue is fixed`. Keep a workaround when you cannot confirm the fix is in, and tell the user which ones you left.

## Build

Build every configuration the current platform can build:

```bash
# x86_64-linux
nix build .#nixosConfigurations.torrent.config.system.build.toplevel
nix build '.#homeConfigurations."sei40kr@torrent".activationPackage'

# aarch64-darwin
nix build '.#homeConfigurations."yongju-seong@MAC26010003".activationPackage'
```

Report which configurations you could not build on this platform.
