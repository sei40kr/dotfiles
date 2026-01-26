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
    home.packages = [
      (pkgs.gnome-pomodoro.overrideAttrs (oldAttrs: {
        postInstall = (oldAttrs.postInstall or "") + ''
          # Remove original desktop entry to avoid duplication with our custom one
          rm -f $out/share/applications/org.gnome.Pomodoro.desktop
        '';
      }))
    ];

    xdg.desktopEntries."org.gnome.Pomodoro" = {
      name = "Pomodoro";
      comment = "A simple time management utility";
      exec = "gnome-pomodoro";
      icon = "gnome-pomodoro";
      terminal = false;
      type = "Application";
      categories = [
        "GTK"
        "GNOME"
        "Utility"
        "Clock"
      ];
      settings = {
        Keywords = "timer;";
        StartupNotify = "false";
        X-GNOME-UsesNotifications = "true";
        DBusActivatable = "true";
      };
      actions = {
        start-stop = {
          name = "Start/Stop";
          exec = "gnome-pomodoro --start-stop";
        };
        pause-resume = {
          name = "Pause/Resume";
          exec = "gnome-pomodoro --pause-resume";
        };
      };
    };

    dconf.settings."org/gnome/pomodoro/preferences" = {
      enabled-plugins = [
        "notifications"
        "sounds"
      ];
    };
  };
}
