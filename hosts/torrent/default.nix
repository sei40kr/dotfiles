{ nixosSystem }:

nixosSystem "x86_64-linux" (
  {
    lib,
    config,
    pkgs,
    ...
  }:
  let
    inherit (lib) mkForce;
  in
  {
    imports = [ ./_hardware-configuration.nix ];

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
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    networking.interfaces.enp0s31f6.useDHCP = true;

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

    user.name = "sei40kr";

    services.ollama.enable = true;

    modules.desktop.wm.sway.enable = true;
    modules.desktop.wm.xmonad.enable = true;
    modules.desktop.theme.active = "whitesur";

    modules.desktop.apps.bitwarden.enable = true;
    modules.desktop.apps.gnome.pomodoro.enable = true;
    modules.desktop.apps.polybar.openweathermap.cityId = 1860234;
    modules.desktop.apps.steam.enable = true;

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
      package = pkgs.nerd-fonts.iosevka;
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

    services.jellyfin = {
      enable = true;
      openFirewall = true;
    };
    modules.services.docker = {
      enable = true;
      compose.enable = true;
    };
    modules.services.google-drive.enable = true;
    modules.services.ssh.enable = true;

    modules.shell.apps.fastfetch.enable = true;
    modules.shell.git.enable = true;
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
)
