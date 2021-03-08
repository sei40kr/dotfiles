{ lib, pkgs, ... }:

with lib;
with lib.my; {
  imports = [ ./hardware-configuration.nix ../personal.nix ];

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  networking.interfaces.eno1.useDHCP = true;

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "Asia/Tokyo";

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  modules = {
    desktop = {
      sway.enable = true;

      apps = {
        bitwarden.enable = true;
        cheese.enable = true;
        # geary.enable = true;
        gnomeBooks.enable = true;
        # gnomeCalendar.enable = true;
        # gnomeContacts.enable = true;
        gnomeFileRoller.enable = true;
        gnomeFontViewer.enable = true;
        gnome-pomodoro.enable = true;
        nautilus.enable = true;

        slack.enable = true;
        seahorse.enable = true;
      };

      browsers = {
        chromium.enable = true;
        firefox.enable = true;
        qutebrowser.enable = true;
      };

      media = {
        eog.enable = true;
        evince.enable = true;
        totem.enable = true;
      };

      term.termite.enable = true;
    };

    dev = {
      ansible.enable = true;
      aws-cli.enable = true;
      circleci-cli.enable = true;
      cc.enable = true;
      go.enable = true;
      google-cloud-sdk.enable = true;
      groovy.enable = true;
      haskell.enable = true;
      java.enable = true;
      kotlin.enable = true;
      python.enable = true;
      r.enable = true;
      ruby = {
        enable = true;
        rails.enable = true;
      };
      rust.enable = true;
      scala = {
        enable = true;
        bloop.enable = true;
      };
      shell.enable = true;
      spring-boot.enable = true;
      sql.enable = true;
      travis.enable = true;
      web.enable = true;
    };

    editors = {
      datagrip.enable = true;
      emacs = {
        enable = true;
        doom.enable = true;
      };
      idea.enable = true;
      neovim = {
        enable = true;
        manpager.enable = true;
      };
      vscodium.enable = true;
    };

    services = {
      cupsd.enable = true;
      # deluge = {
      #   enable = true;
      #   openFirewall = true;
      #   web.enable = true;
      # };
      docker = {
        enable = true;
        autoPrune.enable = true;
        compose.enable = true;
      };
      # flexget.enable = true;
      fstrim.enable = true;
      jellyfin = {
        enable = true;
        openFirewall = true;
      };
      psd.enable = true;
      rclone = {
        enable = true;
        enableGoogleDrive = true;
        enableGooglePhotos = true;
      };
      sshd.enable = true;
      transmission.enable = true;
    };

    shell = {
      zsh.enable = true;
      tmux = {
        enable = true;
        autoRun.enable = true;
      };
      atcoder-tools.enable = true;
      bat.enable = true;
      git.enable = true;
      hugo.enable = true;
      online-judge-tools.enable = true;
      tools = {
        exa.enable = true;
        htop.enable = true;
        prettyping.enable = true;
        strace.enable = true;
        tcpdump.enable = true;
      };
    };

    theme.active = "zelda";
  };
}
