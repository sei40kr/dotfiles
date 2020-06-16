{ config, lib, options, pkgs, ... }:

{
  my.userName = "sei40kr";
  my.user = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "video" "networkmanager" ];
    shell = pkgs.zsh;
  };

  modules = {
    desktop = {
      xmonad.enable = true;

      apps = {
        geary.enable = true;
        gnomeCalendar.enable = true;
        gnomeContacts.enable = true;
        gnomePomodoro.enable = true;
        nautilus.enable = true;

        slack.enable = true;
        deluge = {
          enable = true;
          enableWebUI = true;
        };
        rofi.enable = true;
        seahorse.enable = true;
      };

      browsers = {
        chromium.enable = true;
        qutebrowser.enable = true;
      };

      term.alacritty.enable = true;

      tools = {
        psd.enable = true;
        randomBackground = {
          enable = true;
          imageDirectory =
            "${pkgs.pantheon.elementary-wallpapers}/share/backgrounds";
        };
        scrot.enable = true;
      };

      i18n.japanese.enable = true;
    };

    media = {
      parole.enable = true;
      ristretto.enable = true;
    };

    dev = {
      editors = {
        emacs.enable = true;
        idea.enable = true;
      };

      tools = {
        git = {
          enable = true;
          enableGitFlow = true;
          enableGitCrypt = true;
        };
        zeal.enable = true;

        # Infrastructure & CI Tools
        awsShell.enable = true;
        googleCloudSdk.enable = true;
        circleciCli.enable = true;
        travis.enable = true;
      };

      cc.enable = true;
      go.enable = true;
      groovy.enable = true;
      haskell.enable = true;
      java.enable = true;
      kotlin.enable = true;
      python.enable = true;
      r.enable = true;
      ruby = {
        enable = true;
        enableRails = true;
      };
      rust.enable = true;
      scala.enable = true;
      sh.enable = true;
      web.enable = true;
    };

    services = {
      docker = {
        enable = true;
        enableAutoPrune = true;
      };
      fstrim.enable = true;
    };

    shell = {
      zsh.enable = true;
      tmux.enable = true;

      tools = {
        sshd.enable = true;

        bat.enable = true;
        exa.enable = true;
        flexget.enable = true;
        htop.enable = true;
        strace.enable = true;
        tcpdump.enable = true;
      };
    };

    themes = {
      preferDarkTheme = true;

      gtkIconTheme = {
        package = pkgs.gnome3.adwaita-icon-theme;
        name = "Adwaita";
      };
    };
  };
}
