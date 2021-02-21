{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.dunst;
in {
  options.modules.desktop.apps.dunst = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    package = mkOption {
      type = types.package;
      default = pkgs.dunst.override { dunstify = true; };
    };
  };

  config = mkIf cfg.enable {
    modules.desktop.backends.dbus = {
      enable = mkForce true;
      packages = [ cfg.package ];
    };

    user.packages = [ cfg.package ];
    home.configFile."dunst/dunstrc".source = "${configDir}/dunst/dunstrc";
    home-manager.users.${config.user.name}.systemd.user.services.dunst = {
      Unit = {
        Description = "Dunst notification daemon";
        Documentation = "man:dunst(1)";
        PartOf = [ "graphical-session.target" ];
        X-Restart-Triggers = [ "${configDir}/dunst/dunstrc" ];
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
