{ nixosSystem }:

nixosSystem "x86_64-linux" (
  { inputs, pkgs, ... }:
  {
    imports = [
      "${inputs.nixpkgs}/nixos/modules/profiles/hardened.nix"
      ./_hardware-configuration.nix
    ];

    # Use the systemd-boot EFI boot loader.
    boot.loader.efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";
    };
    boot.loader.grub = {
      enable = true;
      devices = [ "nodev" ];
      extraEntries = ''
        menuentry "Windows 11" {
          insmod part_gpt
          insmod fat
          insmod search_fs_uuid
          insmod chain
          search --fs-uuid --set=root 3448-B644
          chainloader /EFI/Microsoft/Boot/bootmgfw.efi
        }
      '';
      default = "saved";
      efiSupport = true;
    };

    environment.memoryAllocator.provider = "libc";
    security.unprivilegedUsernsClone = true;

    # Set your time zone.
    time.timeZone = "Asia/Tokyo";
    time.hardwareClockInLocalTime = true;

    networking.hostName = "torrent"; # Define your hostname.
    networking.networkmanager.enable = true;
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    networking.interfaces.enp0s31f6.useDHCP = true;

    services.autorandr = {
      enable = true;
      profiles = {
        default = {
          fingerprint = {
            DP-0 = "00ffffffffffff0052623f0201010181ff220103806e3e780a0dc9a05747982712484c2108008180010101010101010101010101010108e80030f2705a80b0588a0040846300001e023a801871382d40582c450040846300001e000000fc00544f53484942412d54560a2020000000fd00173d0f793c000a20202020202001d7020364f34e6110202204040302040401625f5d3b097f070f7f071507503507483e1fc04d0200570600677e005f54016d030c002300b83c2f006001030467d85dc401788003e305c301e200d9e3060f01e30f0100eb0100000000000000000000835f0000565e00a0a0a029503020350040846300001a00000000000000000044";
            HDMI-0 = "00ffffffffffff0009d13580455400002b1f0103803c22782a3355ac524ea026105054a56b80d1c0b300a9c08180810081c001010101565e00a0a0a029503020350055502100001a000000ff005a414d30303431353031390a20000000fd00184c1e873c000a202020202020000000fc0042656e5120504432373035510a015b020344f14f5d5e5f6061101f22212004131203012309070783010000e200cf6d030c001000383c20006001020367d85dc401788003e305c301e30f1800e6060501575748565e00a0a0a029503020350055502100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000007c";
          };
          config = {
            DP-0.enable = false;
            HDMI-0 = {
              enable = true;
              primary = true;
              mode = "2560x1440";
            };
          };
        };
      };
    };

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

    programs.nix-ld.enable = true;

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

    modules.desktop.browsers.vivaldi.enable = true;

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
    modules.dev.lang.web = {
      enable = true;
      bun.enable = true;
    };

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
      package = pkgs.my.julia-mono-nf;
      name = "JuliaMono Nerd Font";
      size = 18;
    };
    modules.term.colorschemes.active = "tokyo-night";
    modules.term.kitty.enable = true;
    modules.term.wezterm.enable = true;
  }
)
