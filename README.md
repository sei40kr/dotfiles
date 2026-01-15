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

### Secure Boot Setup (for hosts with Lanzaboote)

Some hosts (e.g., `torrent`) have Secure Boot enabled via Lanzaboote. For these hosts:

1. Before installation, **disable Secure Boot** in your UEFI/BIOS settings.

1. Complete the standard installation steps above.

1. After the first boot, verify Secure Boot keys are generated:

   ```sh
   sudo sbctl status
   ```

1. Reboot into UEFI/BIOS settings and enable **Setup Mode** (this clears existing Secure Boot keys).

   **Note:** Some BIOS menus may not have a "Setup Mode" option. In this case, delete all existing Secure Boot keys (PK, KEK, db, dbx) manually to enter Setup Mode.

1. Reboot the system. Lanzaboote will automatically enroll the generated keys.

1. Verify Secure Boot is working:

   ```sh
   sudo sbctl status
   ```

   You should see `Secure Boot: ✓ Enabled`.

> [!IMPORTANT]
> When Secure Boot is enabled, GRUB cannot be used. If you have a dual-boot environment, you need to select the OS from the boot menu (typically accessible via F8, F11, or F12 during boot).

## Acknowledgements

- [Flakes - NixOS Wiki](https://nixos.wiki/wiki/Flakes)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [berbiche/dotfiles](https://github.com/berbiche/dotfiles)
- [Regolith](https://regolith-linux.org)
- [Archcraft OS](https://archcraft.io)
- [Awesome Wayland](https://github.com/natpen/awesome-wayland)
