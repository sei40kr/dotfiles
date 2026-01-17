{
  inputs,
  lib,
  config,
  pkgs,
  ...
}:
let
  inherit (lib) mkForce;
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

  services.openvpn.servers.work = {
    autoStart = false;
    updateResolvConf = true;
    config = ''
      client
      dev tun
      proto udp
      nobind
      resolv-retry infinite
      fragment 1472
      mssfix 1300
      keepalive 10 60
      persist-tun
      persist-key
      verb 3
      auth SHAKE256
      cipher AES-256-GCM
      remote-cert-tls server
      ca ${config.age.secrets."work-vpn-ca".path}
      tls-version-min 1.2
      tls-cipher TLS-ECDHE-ECDSA-WITH-AES-256-GCM-SHA384
      tls-crypt /var/lib/openvpn/work-tc.key
      reneg-sec 0
      auth-nocache
      auth-user-pass /var/lib/openvpn/work-auth.txt
      config ${config.age.secrets."work-vpn-remotes".path}
    '';
  };
  age.secrets."work-vpn-ca" = {
    file = ./secrets/work-vpn-ca.crt.age;
    owner = "root";
    mode = "0440";
  };
  age.secrets."work-vpn-remotes" = {
    file = ./secrets/work-vpn-remotes.conf.age;
    owner = "root";
    mode = "0440";
  };

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
  modules.desktop.theme.active = "whitesur";

  modules.desktop.apps.steam.enable = true;

  modules.i18n.japanese.enable = true;

  modules.shell.zsh.enable = true;

  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };
  modules.services.docker = {
    enable = true;
    compose.enable = true;
  };
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
