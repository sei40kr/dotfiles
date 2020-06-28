{ config, lib, options, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.apps.polybar;
  pythonEnv = pkgs.python3.withPackages
    (pythonPackages: with pythonPackages; [ pygobject3 dbus-python ]);
  polybarStart = pkgs.writeShellScriptBin "polybar-start" "PATH=${
      escapeShellArg "${pythonEnv}/bin"
    }:\${PATH} ${pkgs.polybar}/bin/polybar top";
  polybarConfig = pkgs.writeText "polybar-config" ''
    [section/base]
    include-file = ${cfg.themeConfig}

    ${readFile <config/polybar/config>}
  '';
in {
  options.modules.desktop.apps.polybar = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    themeConfig = mkOption { type = with types; either path str; };
  };

  config = mkIf cfg.enable {
    modules.desktop.xmonad.polybarStartExecutable =
      "${polybarStart}/bin/polybar-start";

    my.packages = with pkgs; [ polybar material-design-icons ];
    my.home.xdg.configFile."polybar/config".text = ''
      [section/base]
      include-file = ${polybarConfig}

      [module/gnome-pomodoro]
      exec = ${<config/polybar/scripts/gnome-pomodoro.py>}
      click-left = ${pkgs.gnome3.pomodoro}/bin/gnome-pomodoro

      [module/fcitx]
      exec = ${<config/polybar/scripts/fcitx.py>}
      exec-if = [ -x ${escapeShellArg "${pkgs.fcitx}/bin/fcitx-remote"} ]
      click-left = ${pkgs.fcitx}/bin/fcitx-configtool
    '';
  };
}
