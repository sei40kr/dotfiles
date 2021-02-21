{ config, home-manager, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.backends.glibNetworking.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.backends.glibNetworking.enable {
    modules.desktop.backends.dbus = {
      enable = mkForce true;
      packages = with pkgs; [ glib-networking ];
    };

    services.udev.packages = with pkgs; [ libmtp ];

    home-manager.users.${config.user.name}.systemd.user.services.glib-pacrunner =
      {
        Unit.Description = "GLib proxy auto-configuration service";
        Service = {
          Type = "dbus";
          BusName = "org.gtk.GLib.PACRunner";
          ExecStart = "${pkgs.glib-networking}/libexec/glib-pacrunner";
        };
      };
  };
}
