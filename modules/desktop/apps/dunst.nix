{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.dunst.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.dunst.enable {
    modules.desktop.backends.dbus = {
      enable = mkForce true;
      packages = with pkgs; [ dunst ];
    };

    my.packages = with pkgs; [ dunst libnotify ];
    my.home.xdg.configFile."dunst/dunstrc" = {
      source = <config/dunst/dunstrc>;
      onChange = "systemctl --user restart dunst.service";
    };
    my.home.systemd.user.services.dunst = {
      Unit = {
        Description = "Dunst notification daemon";
        Documentation = "man:dunst(1)";
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Type = "dbus";
        BusName = "org.freedesktop.Notifications";
        ExecStart = "${pkgs.dunst}/bin/dunst";
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
