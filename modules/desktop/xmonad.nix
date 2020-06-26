{ config, lib, options, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.xmonad;
  home-manager = config.home-manager.users."${config.my.userName}";
in {
  options.modules.desktop.xmonad = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    themeConfig = mkOption { type = types.path; };

    autoRepeatDelay = mkOption {
      type = types.int;
      default = 150;
    };

    autoRepeatInterval = mkOption {
      type = types.int;
      default = 30;
    };
  };

  config = mkIf cfg.enable {
    modules = {
      desktop = {
        xdgUserDirs.enable = mkForce true;
        xsecurelock.enable = mkForce true;

        apps = {
          dunst.enable = mkForce true;
          polybar.enable = mkForce true;
        };

        config.gtk.enable = mkForce true;

        tools.xbindkeys.enable = mkForce true;
      };

      services.picom.enable = mkForce true;
    };

    # Enable X.Org Server + startx
    services.xserver = {
      enable = true;
      displayManager.startx.enable = true;
    };
    my.home.home.file.".xinitrc".text = ''
      ./.xsession
    '';

    # Enable X Session + Xmonad
    my.home.xsession = {
      enable = true;

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
    my.packages = with pkgs; [ xorg.xmessage gxmessage ]; # required by Xmonad
    my.home.home.file.".xmonad/src/Lib/Theme.hs".source = cfg.themeConfig;
    my.home.home.file.".xmonad/build".source = <config/xmonad/build>;
    my.home.home.file.".xmonad/package.yaml".text = ''
      name:                my-xmonad
      version:             0.1.0.0
      github:              "sei40kr/dotfiles"
      license:             MIT
      author:              "Seong Yong-ju"
      maintainer:          "sei40kr@gmail.com"
      copyright:           "2020 Seong Yong-ju"

      extra-source-files: []

      # Metadata used when publishing your package
      # synopsis:            Short description of your package
      # category:            Web

      # To avoid duplicated efforts in documentation and dealing with the
      # complications of embedding Haddock markup inside cabal files, it is
      # common to point users to the README.md file.
      description: ""

      dependencies:
      - base >=4.7 && <5
      - containers
      - directory
      - xmonad >=0.15 && <0.16
      - xmonad-contrib >=0.15 && <0.16

      library:
        source-dirs: ${home-manager.home.homeDirectory}/.xmonad/src

      executables:
        my-xmonad:
          main:                Main.hs
          source-dirs:         ${<config/xmonad/app>}
          ghc-options:
          - -threaded
          - -rtsopts
          - -with-rtsopts=-N
          dependencies:
          - my-xmonad
    '';
    my.home.home.file.".xmonad/shell.nix".source = <config/xmonad/shell.nix>;
    my.home.home.file.".xmonad/stack.yaml".source = <config/xmonad/stack.yaml>;
    my.home.home.file.".xmonad/stack.yaml.lock".source =
      <config/xmonad/stack.yaml.lock>;
    my.xsession.init = ''
      xset r rate ${toString cfg.autoRepeatDelay} ${
        toString cfg.autoRepeatInterval
      }
    '';
  };
}
