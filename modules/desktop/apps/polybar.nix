{ config, lib, options, pkgs, ... }:

with lib;
let
  pythonForScripts = pkgs.python3.withPackages
    (pythonPackages: with pythonPackages; [ pygobject3 dbus-python ]);
in {
  options.modules.desktop.apps.polybar.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.polybar.enable {
    my.home.services.polybar = {
      enable = true;
      config = {
        "section/base".include-file = "${<config/polybar/config>}";
        "module/gnome-pomodoro" = {
          exec = "${<config/polybar/scripts/gnome-pomodoro.py>}";
          click-left = "${pkgs.gnome3.pomodoro}/bin/gnome-pomodoro";
        };
        "module/fcitx" = {
          exec = "${<config/polybar/scripts/fcitx.py>}";
          exec-if = "[ -x ${escapeShellArg "${pkgs.fcitx}/bin/fcitx-remote"} ]";
          click-left = "${pkgs.fcitx}/bin/fcitx-configtool";
        };
      };
      script = "polybar top &";
    };
    my.home.xdg.configFile."polybar/config".onChange =
      "systemctl --user restart polybar.service";
    my.packages = with pkgs; [ material-design-icons ];
    my.home.systemd.user.services.polybar.Service = {
      Environment = mkForce
        "PATH=${pythonForScripts}/bin:${config.my.home.services.polybar.package}/bin:/run/wrappers/bin";
    };
  };
}
