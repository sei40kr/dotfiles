{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.dunst;
in {
  options.modules.desktop.apps.dunst = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ dunst ];

    systemd.user.services.dunst = {
      description = "Dunst notification daemon";
      documentation = [ "man:dunst(1)" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "dbus";
        BusName = "org.freedesktop.Notifications";
        ExecStart = "${pkgs.dunst}/bin/dunst";
      };
    };
  };
}
