# dotfiles

![Made with Doom Emacs](https://img.shields.io/github/tag/hlissner/doom-emacs.svg?style=flat-square&label=release&color=58839b)

## Get Started

### Darwin

1. Install Nix via **multi-user installation**:

   ```sh
   sh <(curl -L https://nixos.org/nix/install) --daemon
   ```

1. Enter Nix shell (for the first time only):

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

### NixOS

1. Enter Nix shell (for the first time only):

   ```sh
   nix-shell
   ```

1. Then, switch to the new configuration:

   ```sh
   nixos-rebuild switch --flake '.#<hostname>'
   ```

## Acknowledgements

- [Flakes - NixOS Wiki](https://nixos.wiki/wiki/Flakes)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [berbiche/dotfiles](https://github.com/berbiche/dotfiles)
- [Regolith](https://regolith-linux.org)
- [Archcraft OS](https://archcraft.io)
- [adi1090x/rofi](https://github.com/adi1090x/rofi)
- [adi1090x/polybar-themes](https://github.com/adi1090x/polybar-themes)
- [Sway - NixOS Wiki](https://nixos.wiki/wiki/Sway)
- [Sway - ArchWiki](https://wiki.archlinux.org/title/Sway)
- [Running programs natively under Wayland](https://github.com/swaywm/sway/wiki/Running-programs-natively-under-wayland)
- [Awesome Wayland](https://github.com/natpen/awesome-wayland)
- [polybar/polybar-scripts](https://github.com/polybar/polybar-scripts)
