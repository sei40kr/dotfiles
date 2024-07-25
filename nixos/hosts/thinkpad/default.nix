{ nixosSystem }:

nixosSystem "x86_64-linux" ({ pkgs, ... }:
let
  inherit (builtins) floor;
in
{
  imports = [ ./_hardware-configuration.nix ];

  # Use kernel 6.1
  boot.kernelPackages = pkgs.linuxPackages_6_1;
  # Use the systemd-boot EFI boot loader.
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;

  # Set your time zone.
  time.timeZone = "Asia/Tokyo";

  networking.hostName = "thinkpad"; # Define your hostname.
  networking.networkmanager.enable = true;

  services.autorandr = {
    enable = true;
    profiles = {
      docked = {
        fingerprint = {
          DP-2 = "00ffffffffffff0009d13580455400002b1f0103803c22782a3355ac524ea026105054a56b80d1c0b300a9c08180810081c001010101565e00a0a0a029503020350055502100001a000000ff005a414d30303431353031390a20000000fd00184c1e873c000a202020202020000000fc0042656e5120504432373035510a015b020344f14f5d5e5f6061101f22212004131203012309070783010000e200cf6d030c001000383c20006001020367d85dc401788003e305c301e30f1800e6060501575748565e00a0a0a029503020350055502100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000007c";
        };
        config = {
          DP-2 = {
            enable = true;
            primary = true;
            mode = "2560x1440";
          };
        };
      };
    };
  };

  services.logind = {
    lidSwitchExternalPower = "ignore";
    lidSwitchDocked = "ignore";
  };

  # Enable CUPS to print documents
  services.printing.enable = true;

  # Enable sound
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11";


  user.name = "sei40kr";

  modules.desktop.gdm.enable = true;
  modules.desktop.theme.active = "whitesur";

  modules.desktop.apps.bitwarden.enable = true;
  modules.desktop.apps.discord.enable = true;
  modules.desktop.apps.dunst.enable = true;
  modules.desktop.apps.gnome.pomodoro.enable = true;
  modules.desktop.apps.qbittorrent.enable = true;
  modules.desktop.apps.slack.enable = true;
  modules.desktop.apps.zeal.enable = true;
  modules.desktop.apps.zoom.enable = true;

  modules.desktop.browsers.chrome.enable = true;

  modules.desktop.media.documents.ebook.enable = true;
  modules.desktop.media.video.vlc.enable = true;

  modules.dev.lang.go.enable = true;
  modules.dev.lang.java.enable = true;
  modules.dev.lang.javascript.enable = true;
  modules.dev.lang.kotlin.enable = true;
  modules.dev.lang.lua.enable = true;
  modules.dev.lang.nix.enable = true;
  modules.dev.lang.rust.enable = true;
  modules.dev.lang.shell.enable = true;
  modules.dev.lang.sql.enable = true;
  modules.dev.lang.web.enable = true;
  modules.dev.tools.aws.enable = true;

  modules.editors.fonts.code = {
    package = pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; };
    name = "Iosevka Nerd Font";
    size = 17;
  };
  modules.editors.datagrip.enable = true;
  modules.editors.dataspell.enable = true;
  modules.editors.emacs = {
    enable = true;
    doom.enable = true;
  };
  modules.editors.idea.enable = true;
  modules.editors.nvim = {
    enable = true;
    manpager.enable = true;
  };

  modules.i18n.japanese.enable = true;

  modules.services.docker.enable = true;
  modules.services.k8s = {
    roles = [ "master" "node" ];
    easyCerts = true;
    masterHostname = "localhost";
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
})
