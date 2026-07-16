{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkForce;
  outfit = pkgs.google-fonts.override { fonts = [ "Outfit" ]; };
in
{
  imports = [
    ./hardware-configuration.nix
    inputs.lanzaboote.nixosModules.lanzaboote
    inputs.self.nixosModules.host-shared
  ];

  # Enable automatic garbage collection
  nix.gc = {
    automatic = true;
    dates = ''
      *-*-* 03:00:00
    '';
  };

  # Use systemd-boot with Secure Boot via Lanzaboote
  boot.loader.systemd-boot.enable = mkForce false;
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot";
  };
  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/var/lib/sbctl";
    autoGenerateKeys.enable = true;
    autoEnrollKeys.enable = true;
  };

  # Set your time zone.
  time.timeZone = "Asia/Tokyo";
  time.hardwareClockInLocalTime = true;

  networking.hostName = "torrent"; # Define your hostname.
  networking.networkmanager.enable = true;
  networking.interfaces.enp0s31f6.useDHCP = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # System services backing Noctalia Shell's control-center features:
  # NetworkManager (Wi-Fi) above, plus Bluetooth, power-profile switching, and battery.
  # https://docs.noctalia.dev/v4/getting-started/nixos
  hardware.bluetooth.enable = true;
  services.power-profiles-daemon.enable = true;
  services.upower.enable = true;

  services.greetd.enable = true;
  modules.desktop.regreet = {
    enable = true;
    theme = {
      package = pkgs.whitesur-gtk-theme;
      name = "WhiteSur-light-solid";
    };
    iconTheme = {
      package = pkgs.whitesur-icon-theme;
      name = "WhiteSur";
    };
  };

  # Enable CUPS to print documents
  services.printing.enable = true;

  # Enable sound
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    wireplumber = {
      enable = true;
      extraConfig = {
        "my-default-devices" = {
          "monitor.alsa.rules" = [
            {
              matches = [
                {
                  "node.name" = "alsa_output.pci-0000_01_00.1.hdmi-stereo";
                }
              ];
              actions = {
                update-props = {
                  "priority.driver" = 1050;
                  "priority.session" = 1050;
                };
              };
            }
            {
              matches = [
                {
                  "node.name" = ''~alsa_input\.usb-Razer_Inc_Razer_Seiren_Mini_.*'';
                }
              ];
              actions = {
                update-props = {
                  "priority.driver" = 1050;
                  "priority.session" = 1050;
                };
              };
            }
          ];
        };
      };
    };
  };

  programs.gnupg.agent.enable = true;

  programs.nix-ld.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11";

  users.users.sei40kr = {
    uid = 1000;
    description = "The primary user account";
    isNormalUser = true;
    group = "users";
    extraGroups = [ "wheel" ];
    shell = pkgs.zsh;
  };

  services.ollama.enable = true;

  modules.desktop.wm.niri.enable = true;
  modules.desktop.fontconfig = {
    enable = true;
    fonts.sansSerif = {
      packages = [
        outfit
        pkgs.noto-fonts-cjk-sans
      ];
      names = [
        "Outfit"
        "Noto Sans CJK JP"
      ];
    };
  };
  # name routes through fontconfig (→ Outfit); package is only needed to
  # satisfy regreet, which requires a non-null font package.
  modules.desktop.de.defaultFonts.ui = {
    package = outfit;
    name = "sans-serif";
    size = 11;
  };
  # Wallpaper for the login screen (regreet), niri, and Noctalia. Decoupled from
  # any GTK theme: application theming is now driven by Noctalia (see
  # modules.desktop.apps.noctalia-shell).
  modules.desktop.de.background.image = {
    path = ./wallpapers/tokyo-night.jpg;
    mode = "fill";
  };

  modules.desktop.apps.steam.enable = true;

  i18n.extraLocaleSettings.LC_CTYPE = "ja_JP.UTF-8";

  modules.shell.zsh.enable = true;

  modules.services.docker.enable = true;
  modules.services.ssh.enable = true;

  environment.systemPackages = with pkgs; [
    efibootmgr
    sbctl
    bottom
    ghq
    strace
    tcpdump
    hugo
    discord
    slack
    zeal
  ];
}
