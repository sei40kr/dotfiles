{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.backends.tracker.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.backends.tracker.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ tracker ];
      };
      glibNetworking.enable = mkForce true;
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ tracker ];
      };
    };

    my.packages = with pkgs; [ tracker ];
    my.home.systemd.user.services.tracker-store = {
      Unit.Description = "Tracker metadata database store and lookup manager";
      Service = {
        Type = "dbus";
        BusName = "org.freedesktop.Tracker1";
        ExecStart = "${pkgs.tracker}/libexec/tracker-store";
        Restart = "on-failure";
        # Don't restart after tracker daemon -k (aka tracker-control -k)
        RestartPreventExitStatus = "SIGKILL";
      };
    };
  };
}
