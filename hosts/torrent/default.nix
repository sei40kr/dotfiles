{ nixosSystem }:

nixosSystem "x86_64-linux" ({ pkgs, ... }: {
  imports = [ ./_hardware-configuration.nix ];

  # Use kernel 6.1
  boot.kernelPackages = pkgs.linuxPackages_6_1;
  # Use the systemd-boot EFI boot loader.
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;

  # Set your time zone.
  time.timeZone = "Asia/Tokyo";

  networking.hostName = "torrent"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  networking.interfaces.enp0s31f6.useDHCP = true;

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


  modules = {
    desktop = {
      gdm.enable = true;
      wm.sway.enable = true;
      theme.active = "whitesur";

      apps = {
        bitwarden.enable = true;
        discord.enable = true;
        gnome.pomodoro.enable = true;
        slack.enable = true;
        zeal.enable = true;
      };

      browsers = {
        chrome.enable = true;
      };

      media = {
        documents.ebook.enable = true;
        video.vlc.enable = true;
      };
    };

    dev = {
      lang = {
        cc.enable = true;
        go.enable = true;
        java.enable = true;
        lua.enable = true;
        r.enable = true;
        nix.enable = true;
        python = {
          enable = true;
          enablePoetry = true;
        };
        rust.enable = true;
        scala = {
          enable = true;
          bloop.enable = true;
        };
        shell.enable = true;
        sql.enable = true;
        web.enable = true;
      };
      tools = {
        ansible.enable = true;
        aws.enable = true;
        jupyter.enable = true;
      };
    };

    editors = {
      datagrip.enable = true;
      dataspell.enable = true;
      emacs = {
        enable = true;
        doom.enable = true;
      };
      idea.enable = true;
    };

    i18n.japanese.enable = true;

    services = {
      docker = {
        enable = true;
        compose.enable = true;
      };
      # flexget.enable = true;
      google-drive.enable = true;
      jellyfin = {
        enable = true;
        openFirewall = true;
      };
      ssh.enable = true;
    };

    shell = {
      apps.fastfetch.enable = true;
      bottom.enable = true;
      ghq.enable = true;
      strace.enable = true;
      tcpdump.enable = true;
      git.enable = true;
      hugo.enable = true;
      oj.enable = true;
      tmux = {
        enable = true;
        autoRun = true;
      };
      zsh.enable = true;
    };

    term.kitty.enable = true;
  };

  user.name = "sei40kr";

  modules.term.colorschemes.active = "tokyo-night";

  modules.term.font = {
    package = pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; };
    name = "Iosevka Nerd Font";
    size = 17;
  };
  modules.editors.fonts.code = {
    package = pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; };
    name = "Iosevka Nerd Font";
    size = 17;
  };
})
