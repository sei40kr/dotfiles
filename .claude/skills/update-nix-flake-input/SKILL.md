---
name: update-nix-flake-input
description: "Update a specific Nix flake input to its latest version. Use when asked to update a flake input, bump a dependency, or upgrade a specific package/input in the flake. Handles the full workflow: verify input exists, update flake.lock, build to verify, and commit the change."
---

# Update Nix Flake Input

Update a specific flake input to its latest version, verify the build, and commit.

## Workflow

1. **Verify the input exists in flake.nix**:
   Read `flake.nix` and confirm the user-specified input name corresponds to an actual input. The user may refer to it by a partial name or alias - find the correct input name in the `inputs` attribute set.

2. **Update flake.lock** for the specified input:

   ```bash
   nix flake update <input-name>
   ```

3. **Build NixOS configuration to verify**:

   ```bash
   nixos-rebuild build --flake ".#torrent"
   ```

   If the build fails, investigate and fix the issue before proceeding.

4. **Stage and commit**:

   ```bash
   git add flake.lock && git commit -m "feat(<scope>): update <input-name>"
   ```

   The `<scope>` should reflect the affected experience/module area, not the input name itself.
   Examples:
   - `llm-agents.nix` → `feat(ai): update llm-agents.nix`
   - `lazyvim` → `feat(lazyvim): update lazyvim`
   - `anyrun` → `feat(anyrun): update anyrun`
   - `fenix` → `feat(rust): update fenix`
