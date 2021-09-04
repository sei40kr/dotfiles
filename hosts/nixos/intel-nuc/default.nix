{ lib, pkgs, ... }:

with lib; {
  imports = [ ./hardware-configuration.nix ];

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
      themes.active = "material-design";

      apps = {
        bitwarden.enable = true;
        calendar.enable = true;
        discord.enable = true;
        file-roller.enable = true;
        font-viewer.enable = true;
        geary.enable = true;
        gnome-books.enable = true;
        gnome-todo.enable = true;
        gnome-weather.enable = true;
        nautilus.enable = true;
        pomodoro.enable = true;
        seahorse.enable = true;
        slack.enable = true;
        zeal.enable = true;
      };

      browsers = {
        google-chrome.enable = true;
        firefox.enable = true;
        qutebrowser.enable = true;
      };

      media = {
        eog.enable = true;
        evince.enable = true;
        totem.enable = true;
      };
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
      jupyter.enable = true;
      kotlin.enable = true;
      latex.enable = true;
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
    };

    services = {
      docker = {
        enable = true;
        autoPrune.enable = true;
        compose.enable = true;
      };
      # flexget.enable = true;
      jellyfin = {
        enable = true;
        openFirewall = true;
      };
      psd.enable = true;
      sushi.enable = true;
      transmission.enable = true;
    };

    shell = {
      exa.enable = true;
      htop.enable = true;
      prettyping.enable = true;
      strace.enable = true;
      tcpdump.enable = true;
      atcoder-tools.enable = true;
      bat.enable = true;
      git.enable = true;
      hugo.enable = true;
      online-judge-tools.enable = true;
      tmux = {
        enable = true;
        autoRun = true;
      };
      zsh.enable = true;
    };

    term.alacritty.enable = true;
  };

  services = {
    fstrim.enable = true;
    openssh = {
      enable = true;
      permitRootLogin = "no";
    };
    printing.enable = true;
    upower.enable = true;
  };

  user = {
    isNormalUser = true;
    name = "sei40kr";
    uid = 1000;
    extraGroups = [ "wheel" ];
    shell = pkgs.zsh;
  };
}
