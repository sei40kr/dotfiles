{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.dunst.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.dunst.enable {
    my.packages = with pkgs; [ dunst libnotify ];
    my.home.xdg = {
      configFile."dunst/dunstrc" = {
        source = <config/dunst/dunstrc>;
        onChange = "systemctl --user restart dunst.service";
      };
      dataFile."dbus-1/services/org.knopwob.dunst.service".source =
        "${pkgs.dunst}/share/dbus-1/services/org.knopwob.dunst.service";
    };
    my.home.systemd.user.services.dunst = {
      Unit = {
        Description = "Dunst notification daemon";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Type = "dbus";
        BusName = "org.freedesktop.Notifications";
        ExecStart = "${pkgs.dunst}/bin/dunst";
      };
    };
  };
}
