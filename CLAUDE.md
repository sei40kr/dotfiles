# dotfiles

This is a NixOS dotfiles repository using the **Blueprint framework** for automatic module discovery. It manages system-level (NixOS) and user-level (home-manager) configurations.

## Build and Development Commands

To build and apply the configuration, use the `nh-switch` skill. Once a change is done, apply it without waiting for the user to ask.

```bash
# Development
nix develop                     # Enter devshell with pre-commit hooks
nix flake update               # Update flake.lock dependencies

# Linting and formatting (runs automatically via pre-commit)
nix flake check                # Run all checks
treefmt                        # Format all files
```

## Architecture

### Module Discovery (Blueprint)

Blueprint automatically discovers `.nix` files in `modules/` subdirectories. The flake generates `nixosModules.*` and `homeModules.*` from the directory structure. Manual imports are still required within shared modules.

### Directory Structure

- `hosts/<hostname>/` - Per-host system configurations
  - `configuration.nix` - Main NixOS config (imports `host-shared.nix`)
  - `users/<user>/home-configuration.nix` - User home-manager config (imports `home-shared.nix`)
- `modules/nixos/` - NixOS modules (auto-discovered)
- `modules/home/` - Home-manager modules (auto-discovered)
- `packages/` - Custom Nix packages
- `lib/` - Shared utilities and type definitions
- `checks/` - CI checks (pre-commit, schema validation)
- `devshells/` - Development shell environments

### Configuration Flow

1. `flake.nix` → defines inputs and uses Blueprint for module discovery
2. `hosts/torrent/configuration.nix` → imports `modules/nixos/host-shared.nix`
3. `hosts/torrent/users/sei40kr/home-configuration.nix` → imports `modules/home/home-shared.nix`
4. Modules activated via `modules.<namespace>.enable = true` options

### Shared Module Pattern

- `host-shared.nix` - Imports all NixOS modules, provides common system settings
- `home-shared.nix` - Imports all home-manager modules
- `editor-shared.nix`, `shell-shared.nix`, `term-shared.nix`, `browser-shared.nix` - Category-specific shared configs

### Module Structure Convention

```nix
{ lib, config, pkgs, inputs, ... }:
let
  inherit (lib) mkEnableOption mkIf mkOption types;
  cfg = config.modules.<namespace>;
in
{
  options.modules.<namespace> = {
    enable = mkEnableOption "<description>";
    # ... additional options
  };
  # Everything about the program comes first, then related modules.
  # Separate groups with one blank line; keep each group contiguous.
  config = mkIf cfg.enable {
    # The main program's module, followed by overrides of what it generates
    # (e.g. a file an activation writes instead)
    programs.<name> = { ... };
    home.file.<generated-path>.enable = false;

    home.packages = [ ... ];

    # Settings: environment and files
    home.sessionVariables = { ... };
    home.shellAliases = { ... };
    home.file.<path>.text = "...";
    xdg.configFile.<path>.text = "...";

    # Services
    services.<name> = { ... };
    launchd.agents.<name> = { ... };

    home.activation.<name> = lib.hm.dag.entryAfter [ "linkGeneration" ] ''...'';
    home.activation.<other> = lib.hm.dag.entryAfter [ "<name>" ] ''...'';

    # Related modules and programs
    modules.<other>.enable = true;
    programs.<other>.enable = true;
  };
}
```

A comment explaining a group goes directly above it.

### Prefer Existing Module Options

Before writing custom logic (activation scripts, hand-written files, wrappers), check whether the upstream NixOS, home-manager, or nix-darwin module already provides it. Search the locked input's module source first, then upstream master: if the option exists only upstream, say so and leave a `TODO:` to switch once the input is updated.

## Key Technologies

- **Blueprint** - Automatic module discovery (numtide/blueprint)
- **Lix** - Nix implementation (performance/safety improvements)
- **Agenix** - Encrypted secrets management (age encryption)
- **Lanzaboote** - Secure Boot support

## Secrets Management

Secrets encrypted with age via agenix. Configuration in `secrets.nix`. Encrypted files have `.age` extension in `modules/nixos/wireguard/` and `hosts/torrent/secrets/`.

Before committing anything personal or work-related, follow [docs/security-policy.md](docs/security-policy.md).

## Code Quality

- **Formatter**: `nixfmt` via treefmt
- **Linting**: Statix (with `repeated_keys` disabled)
- **Type checking**: Nil LSP
- Pre-commit hooks run automatically in devshell

## Commit Convention

Uses conventional commits: `feat`, `fix`, `refactor`, `chore`, `docs`, `test`, `style`

Example: `feat(torrent): add keyboard remapping for caps lock`
