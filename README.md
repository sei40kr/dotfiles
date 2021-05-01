# dotfiles

![Made with Doom Emacs](https://img.shields.io/github/tag/hlissner/doom-emacs.svg?style=flat-square&label=release&color=58839b)

![Screenshot](https://raw.githubusercontent.com/sei40kr/dotfiles/master/screenshots/main.png)

## Install

### Install Nix

#### macOS

Install Nix via **multi-user installation**:

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

### Setup Environment

#### macOS

1. Enter nix shell (for the first time only):

   ```sh
   nix-shell
   ```

1. Then, build the configuration (for the first time only):

   ```sh
   cd ~/.dotfiles
   nix build '.#darwinConfigurations.<hostname>.system'
   ```

1. Then, switch to the new configuration:

   ```sh
   ./result/sw/bin/darwin-rebuild switch --flake '.#<hostname>'
   ```

#### NixOS

1. Enter nix shell (for the first time only):

   ```sh
   nix-shell
   ```

1. Then, switch to the new configuration:

   ```sh
   nixos-rebuild switch --flake '.#<hostname>'
   ```

## Special Thanks

- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [berbiche/dotfiles](https://github.com/berbiche/dotfiles)
