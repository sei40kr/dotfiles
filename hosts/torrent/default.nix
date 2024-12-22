{ nixosSystem }:

nixosSystem "x86_64-linux" (
  { inputs, pkgs, ... }:
  {
    imports = [
      "${inputs.nixpkgs}/nixos/modules/profiles/hardened.nix"
      ./_hardware-configuration.nix
    ];

    # Use the systemd-boot EFI boot loader.
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.systemd-boot.enable = true;

    environment.memoryAllocator.provider = "libc";
    security.unprivilegedUsernsClone = true;

    # Set your time zone.
    time.timeZone = "Asia/Tokyo";

    networking.hostName = "torrent"; # Define your hostname.
    networking.networkmanager.enable = true;
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    networking.interfaces.enp0s31f6.useDHCP = true;

    # Enable CUPS to print documents
    services.printing.enable = true;

    # Enable sound
    hardware.pulseaudio = {
      enable = false;
      extraConfig = ''
        set-default-sink alsa_output.pci-0000_01_00.1.hdmi-stereo
        set-default-source alsa_input.usb-Razer_Inc_Razer_Seiren_Mini_UC2203L03500294-00.mono-fallback
      '';
    };

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "23.11";

    user.name = "sei40kr";

    modules.desktop.gdm.enable = true;
    modules.desktop.wm.sway.enable = true;
    modules.desktop.wm.xmonad.enable = true;
    modules.desktop.theme.active = "whitesur";

    modules.desktop.apps.bitwarden.enable = true;
    modules.desktop.apps.discord.enable = true;
    modules.desktop.apps.gnome.pomodoro.enable = true;
    modules.desktop.apps.slack.enable = true;
    modules.desktop.apps.steam.enable = true;
    modules.desktop.apps.zeal.enable = true;

    modules.desktop.browsers.chrome.enable = true;

    modules.desktop.media.documents.ebook.enable = true;
    modules.desktop.media.video.vlc.enable = true;

    modules.dev.lang.cc.enable = true;
    modules.dev.lang.go.enable = true;
    modules.dev.lang.java.enable = true;
    modules.dev.lang.lua.enable = true;
    modules.dev.lang.r.enable = true;
    modules.dev.lang.nix.enable = true;
    modules.dev.lang.python.enable = true;
    modules.dev.lang.rust.enable = true;
    modules.dev.lang.scala = {
      enable = true;
      bloop.enable = true;
    };
    modules.dev.lang.shell.enable = true;
    modules.dev.lang.sql.enable = true;
    modules.dev.lang.web.enable = true;

    modules.dev.tools.ansible.enable = true;
    modules.dev.tools.aws.enable = true;
    modules.dev.tools.jupyter.enable = true;

    modules.editors.fonts.code = {
      package = pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; };
      name = "Iosevka NFM";
      size = 17;
    };
    modules.editors.datagrip.enable = true;
    modules.editors.dataspell.enable = true;
    modules.editors.emacs = {
      enable = true;
      doom.enable = true;
    };
    modules.editors.idea.enable = true;
    modules.editors.lazyvim.enable = true;

    modules.i18n.japanese.enable = true;

    modules.services.docker = {
      enable = true;
      compose.enable = true;
    };
    modules.services.google-drive.enable = true;
    modules.services.jellyfin = {
      enable = true;
      openFirewall = true;
    };
    modules.services.ssh.enable = true;

    modules.shell.apps.fastfetch.enable = true;
    modules.shell.bottom.enable = true;
    modules.shell.ghq.enable = true;
    modules.shell.strace.enable = true;
    modules.shell.tcpdump.enable = true;
    modules.shell.git.enable = true;
    modules.shell.hugo.enable = true;
    modules.shell.oj.enable = true;
    modules.shell.tmux = {
      enable = true;
      autoRun = true;
    };
    modules.shell.zsh.enable = true;

    modules.term.font = {
      package = pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; };
      name = "Iosevka Nerd Font";
      size = 17;
    };
    modules.term.colorschemes.active = "tokyo-night";
    modules.term.kitty.enable = true;
    modules.term.wezterm.enable = true;
  }
)
