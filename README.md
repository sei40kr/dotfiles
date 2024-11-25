# dotfiles

![Made with Doom Emacs](https://img.shields.io/github/tag/hlissner/doom-emacs.svg?style=flat-square&label=release&color=58839b)

## Install

1. Clone this repository into `/etc/dotfiles`:

   ```sh
   sudo git clone https://github.com/sei40kr/dotfiles.git /etc/dotfiles
   sudo chown -R $USER /etc/dotfiles
   ln -fs /etc/dotfiles ~/.dotfiles
   ```

1. Move to `/etc/dotfiles`

   ```sh
   cd /etc/dotfiles
   ```

1. Build and switch to the configuration:

   ```sh
   sudo nixos-rebuild switch --flake ".#${HOST}"
   ```

1. Once you switch to the configuration, you can use `nh` to update the system
   (you may need to re-login before using `nh`):

   ```sh
   nh os switch
   ```

## Acknowledgements

- [Flakes - NixOS Wiki](https://nixos.wiki/wiki/Flakes)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [berbiche/dotfiles](https://github.com/berbiche/dotfiles)
- [Regolith](https://regolith-linux.org)
- [Archcraft OS](https://archcraft.io)
- [Awesome Wayland](https://github.com/natpen/awesome-wayland)
