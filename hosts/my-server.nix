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
        bitwarden.enable = true;
        evince.enable = true;
        geary.enable = true;
        gnomeCalendar.enable = true;
        gnomeContacts.enable = true;
        gnomeFontViewer.enable = true;
        gnomePomodoro.enable = true;
        nautilus.enable = true;

        slack.enable = true;
        rofi.enable = true;
        seahorse.enable = true;
      };

      browsers = {
        chromium.enable = true;
        qutebrowser.enable = true;
      };

      term.alacritty.enable = true;

      tools = {
        clipmenu.enable = true;
        randomBackground.enable = true;
        scrot.enable = true;
      };

      x11.startx.enable = true;

      i18n.japanese.enable = true;
    };

    media = {
      eog.enable = true;
      totem.enable = true;
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
        dockerCompose.enable = true;
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
      deluge = {
        enable = true;
        enableWebUI = true;
      };
      docker = {
        enable = true;
        enableAutoPrune = true;
      };
      flexget.enable = true;
      fstrim.enable = true;
      psd.enable = true;
      sshd.enable = true;
    };

    shell = {
      zsh.enable = true;
      tmux.enable = true;

      tools = {
        bat.enable = true;
        exa.enable = true;
        htop.enable = true;
        prettyping.enable = true;
        strace.enable = true;
        tcpdump.enable = true;
      };
    };

    themes.kaguya.enable = true;
  };
}
