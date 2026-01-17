{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.gnome.pomodoro;
in
{
  options.modules.desktop.apps.gnome.pomodoro = {
    enable = mkEnableOption "GNOME Pomodoro";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.gnome-pomodoro ];

    dconf.settings."org/gnome/pomodoro/preferences" = {
      enabled-plugins = [
        "notifications"
        "sounds"
      ];
    };
  };
}
