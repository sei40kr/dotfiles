{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.apps.gnome.pomodoro;
in
{
  options.modules.desktop.apps.gnome.pomodoro = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ gnome-pomodoro ];

    modules.desktop.dconf = {
      enable = true;
      settings = {
        "org/gnome/pomodoro/preferences" = {
          enabled-plugins = [
            "notifications"
            "sounds"
          ];
        };
      };
    };
  };
}
