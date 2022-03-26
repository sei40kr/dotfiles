{ lib, pkgs, ... }:

with lib; {
  imports = [ ./hardware-configuration.nix ];

  # Use kernel 5.16
  boot.kernelPackages = pkgs.linuxPackages_5_16;
  # Use the systemd-boot EFI boot loader.
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;

  # Set your time zone.
  time.timeZone = "Asia/Tokyo";

  networking.hostName = "taku"; # Define your hostname.
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
      gnome.enable = true;
      theme.active = "orchis";

      apps = {
        bitwarden.enable = true;
        discord.enable = true;
        gnome = {
          calendar.enable = true;
          font-viewer.enable = true;
          geary.enable = true;
          nautilus.enable = true;
          pomodoro.enable = true;
          weather.enable = true;
        };
        notion.enable = true;
        slack.enable = true;
        zeal.enable = true;
      };

      browsers = {
        chrome.enable = true;
      };

      media = {
        documents = {
          enable = true;
          ebook.enable = true;
        };
        gnome = {
          evince.enable = true;
          totem.enable = true;
        };
      };
    };

    dev = {
      ansible.enable = true;
      aws-cli.enable = true;
      cc.enable = true;
      go.enable = true;
      java.enable = true;
      jupyter.enable = true;
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

    editors = {
      datagrip.enable = true;
      dataspell.enable = true;
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
      gnome = {
        sushi.enable = true;
      };
      jellyfin = {
        enable = true;
        openFirewall = true;
      };
      psd.enable = true;
      ssh.enable = true;
    };

    shell = {
      bottom.enable = true;
      exa.enable = true;
      ghq.enable = true;
      prettyping.enable = true;
      strace.enable = true;
      tcpdump.enable = true;
      bat.enable = true;
      git.enable = true;
      hugo.enable = true;
      oj.enable = true;
      tmux = {
        enable = true;
        autoRun = true;
      };
      zsh.enable = true;
    };

    term.alacritty.enable = true;
  };

  user.name = "sei40kr";

  modules.term.theme.active = "tokyo-night";
}
