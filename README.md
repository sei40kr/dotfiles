# dotfiles

![Made with Doom Emacs](https://img.shields.io/github/tag/hlissner/doom-emacs.svg?style=flat-square&label=release&color=58839b)

## Install

1. Clone this repository:

   ```sh
   sudo git clone https://github.com/sei40kr/dotfiles.git /etc/dotfiles
   sudo chown -R $USER /etc/dotfiles
   ln -fs /etc/dotfiles ~/.dotfiles
   ```

### Darwin

1. Install Nix via **[multi-user installation](https://nixos.org/manual/nix/stable/installation/multi-user.html)**
   (see [Download Nix / NixOS](https://nixos.org/download.html)):

   ```sh
   sh <(curl -L https://nixos.org/nix/install) --daemon
   ```

1. Create a symlink from `/run` to `/var/run`:

   ```sh
   printf 'run\tprivate/var/run\n' | sudo tee -a /etc/synthetic.conf
   /System/Library/Filesystems/apfs.fs/Contents/Resources/apfs.util -B # For Catalina
   /System/Library/Filesystems/apfs.fs/Contents/Resources/apfs.util -t # For Big Sur and later
   ```

1. Build the configuration with `darwin-rebuild`
   (if `./result/sw/bin/darwin-rebuild` does not exist):

   ```sh
   cd ~/.dotfiles
   nix --extra-experimental-features 'nix-command flakes' build '.#darwinConfigurations.<host>.system'
   ```

1. Switch to the new configuration:

   ```sh
   ./result/sw/bin/darwin-rebuild switch --flake '.#<host>'
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

## Things to Do Manually

### Darwin

#### Install Homebrew

Some packages are installed via Homebrew.
You need to install Homebrew manually first; otherwise, the installation of
those packages will be skipped.

```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

- Generate a pair of SSH keys and add it to your GitHub account:

  ```sh
  ssh-keygen -t ecdsa
  ```
- Install [Google Chrome](https://www.google.com/intl/ja/chrome)
- Install [Doom Emacs](https://github.com/doomemacs/doomemacs):

  ```sh
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
  ~/.emacs.d/bin/doom install
  ```

## Acknowledgements

- [Flakes - NixOS Wiki](https://nixos.wiki/wiki/Flakes)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [berbiche/dotfiles](https://github.com/berbiche/dotfiles)
- [Regolith](https://regolith-linux.org)
- [Archcraft OS](https://archcraft.io)
- [Awesome Wayland](https://github.com/natpen/awesome-wayland)
