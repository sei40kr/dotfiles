{ config, home-manager, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.backends.trackerMiners.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.backends.trackerMiners.enable {
    modules.desktop.backends = {
      dbus = {
        enable = mkForce true;
        packages = with pkgs; [ tracker-miners ];
      };
      glibNetworking.enable = mkForce true;
      gsettingsDesktopSchemas = {
        enable = mkForce true;
        packages = with pkgs; [ tracker-miners ];
      };
      tracker.enable = mkForce true;
    };

    user.packages = with pkgs; [ tracker-miners ];
    home-manager.users.${config.user.name}.systemd.user.services = {
      tracker-extract = {
        Unit.Description = "Tracker metadata extractor";
        Service = {
          Type = "dbus";
          BusName = "org.freedesktop.Tracker1.Miner.Extract";
          ExecStart = "${pkgs.tracker-miners}/libexec/tracker-extract";
          Restart = "on-abnormal";
          # Don't restart after tracker daemon -k (aka tracker-control -k)
          RestartPreventExitStatus = "SIGKILL";
        };
      };

      tracker-miner-fs = {
        Unit.Description = "Tracker file system data miner";
        Service = {
          Type = "dbus";
          BusName = "org.freedesktop.Tracker1.Miner.Files";
          ExecStart = "${pkgs.tracker-miners}/libexec/tracker-miner-fs";
          Restart = "on-failure";
          # Don't restart after tracker daemon -k (aka tracker-control -k)
          RestartPreventExitStatus = "SIGKILL";
        };
      };

      tracker-miner-rss = {
        Unit.Description = "Tracker RSS/ATOM feed data miner";
        Service = {
          Type = "dbus";
          BusName = "org.freedesktop.Tracker1.Miner.RSS";
          ExecStart = "${pkgs.tracker-miners}/libexec/tracker-miner-rss";
          Restart = "on-failure";
          # Don't restart after tracker daemon -k (aka tracker-control -k)
          RestartPreventExitStatus = "SIGKILL";
        };
      };

      tracker-writeback = {
        Unit.Description = "Tracker data writeback service";
        Service = {
          Type = "dbus";
          BusName = "org.freedesktop.Tracker1.Writeback";
          ExecStart = "${pkgs.tracker-miners}/libexec/tracker-writeback";
          Restart = "on-failure";
          # Don't restart after tracker daemon -k (aka tracker-control -k)
          RestartPreventExitStatus = "SIGKILL";
        };
      };
    };
  };
}
