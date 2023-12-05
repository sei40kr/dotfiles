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

  networking.hostName = "thinkpad"; # Define your hostname.

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
      gnome.enable = true;
      theme.active = "whitesur";

      apps = {
        bitwarden.enable = true;
        discord.enable = true;
        dunst.enable = true;
        gnome = {
          font-viewer.enable = true;
          nautilus.enable = true;
          pomodoro.enable = true;
        };
        qbittorrent.enable = true;
        slack.enable = true;
        zeal.enable = true;
        zoom.enable = true;
      };

      browsers = {
        chrome = {
          enable = true;
          webapps = {
            gmail.enable = true;
            google-calendar.enable = true;
            google-maps.enable = true;
            google-photos.enable = true;
            youtube-music.enable = true;
          };
        };
      };

      media = {
        documents.ebook.enable = true;
        gnome.evince.enable = true;
        video.vlc.enable = true;
      };
    };

    dev = {
      go.enable = true;
      java.enable = true;
      javascript.enable = true;
      kotlin.enable = true;
      lua.enable = true;
      nix.enable = true;
      rust.enable = true;
      shell.enable = true;
      sql.enable = true;
      web.enable = true;
      tools = {
        aws.enable = true;
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
      nvim = {
        enable = true;
        manpager.enable = true;
      };
    };

    i18n.japanese.enable = true;

    services = {
      gnome = { sushi.enable = true; };
      ssh.enable = true;
    };

    shell = {
      apps = {
        neofetch.enable = true;
      };
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
