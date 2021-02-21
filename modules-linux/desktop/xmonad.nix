{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.xmonad;
in {
  options.modules.desktop.xmonad = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    themeConfig = mkOption { type = types.path; };

    polybarStartCommand = mkOption {
      type = with types; either path str;
      visible = false;
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

        x11 = {
          xbindkeys.enable = mkForce true;
          xsession.enable = mkForce true;
        };
      };

      services.picom.enable = mkForce true;
    };

    home-manager.users.${config.user.name}.xsession.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    user.packages = with pkgs; [ xorg.xmessage gxmessage ]; # required by Xmonad
    home.file.".xmonad/src/Lib/Actions.hs".text = ''
      module Lib.Actions
        ( spawnPolybar
        , spawnRofi
        , spawnStartup
        )
      where
      import           Data.List
      import           XMonad
      import           XMonad.Util.Run

      -- TODO Safely escape the command
      spawnPolybar :: X ()
      ${if config.modules.desktop.apps.polybar.enable then ''
        spawnPolybar = unsafeSpawn "${cfg.polybarStartCommand}"
      '' else ''
        spawnPolybar = return ()
      ''}

      -- TODO Safely escape the command
      spawnRofi :: [String] -> X ()
      ${if config.modules.desktop.apps.polybar.enable then ''
        spawnRofi modi = safeSpawn
          "${config.modules.desktop.apps.rofi.package}/bin/rofi"
          ["-show", intercalate "," modi]
      '' else ''
        spawnRofi = return ()
      ''}

      spawnStartup :: X ()
      spawnStartup = spawnPolybar
    '';
    home.file.".xmonad/src/Lib/Theme.hs".source = cfg.themeConfig;
    home.file.".xmonad/build".source = "${configDir}/xmonad/build";
    home.file.".xmonad/package.yaml".text = ''
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
        source-dirs: ${homeDir}/.xmonad/src

      executables:
        my-xmonad:
          main:                Main.hs
          source-dirs:         ${configDir}/xmonad/app
          ghc-options:
          - -threaded
          - -rtsopts
          - -with-rtsopts=-N
          dependencies:
          - my-xmonad
    '';
    home.file.".xmonad/shell.nix".source = "${configDir}/xmonad/shell.nix";
    home.file.".xmonad/stack.yaml".source = "${configDir}/xmonad/stack.yaml";
    home.file.".xmonad/stack.yaml.lock".source =
      "${configDir}/xmonad/stack.yaml.lock";
  };
}
