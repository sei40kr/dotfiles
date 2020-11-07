{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.gnomePomodoro.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.gnomePomodoro.enable {
    modules.desktop = {
      apps.rofi.systemMenuItems = {
        "Start/Stop Pomodoro" =
          "${pkgs.gnome3.pomodoro}/bin/gnome-pomodoro --start-stop";
        "Pause/Resume Pomodoro" =
          "${pkgs.gnome3.pomodoro}/bin/gnome-pomodoro --pause-resume";
      };
      backends = {
        dbus = {
          enable = mkForce true;
          packages = with pkgs; [ gnome3.pomodoro ];
        };
        gsettingsDesktopSchemas = {
          enable = true;
          packages = with pkgs; [ gnome3.pomodoro ];
        };
      };
    };

    my.packages = with pkgs; [ gnome3.pomodoro ];
  };
}
