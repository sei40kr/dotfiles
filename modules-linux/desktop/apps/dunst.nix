{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.apps.dunst;
in {
  options.modules.desktop.apps.dunst = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    package = mkOption {
      type = types.package;
      default = pkgs.unstable.dunst.override { dunstify = true; };
    };
  };

  config = mkIf cfg.enable {
    modules.desktop.backends.dbus = {
      enable = mkForce true;
      packages = [ cfg.package ];
    };

    my.packages = [ cfg.package ];
    my.home.xdg.configFile."dunst/dunstrc".source = <config/dunst/dunstrc>;
    my.home.systemd.user.services.dunst = {
      Unit = {
        Description = "Dunst notification daemon";
        Documentation = "man:dunst(1)";
        PartOf = [ "graphical-session.target" ];
        X-Restart-Triggers = [ "${<config/dunst/dunstrc>}" ];
      };
      Service = {
        Type = "dbus";
        BusName = "org.freedesktop.Notifications";
        ExecStart = "${cfg.package}/bin/dunst";
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
