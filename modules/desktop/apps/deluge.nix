{ config, lib, options, pkgs, ... }:

with lib;
(let cfg = config.modules.desktop.apps.deluge;
in {
  options.modules.desktop.apps.deluge.enable = mkOption {
    type = types.bool;
    default = false;
  };

  options.modules.desktop.apps.deluge.enableWebUI = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf cfg.enable {
    my.packages = with pkgs; [ deluge ];

    systemd.user.services = {
      deluged = {
        Unit = {
          Description = "Deluge BitTorrent Daemon";
          After = [ "network.target" ];
        };
        Install.WantedBy = [ "multi-user.target" ];
        Service = {
          ExecStart = "${pkgs.deluge}/bin/deluged --do-not-daemonize";
          Restart = "on-success";
          LimitNOFILE = 4096;
        };
      };
      deluge-web = mkIf cfg.enableWebUI {
        Unit = {
          Description = "Deluge BitTorrent Web UI";
          After = [ "network.target" "deluged.service" ];
          Requires = [ "deluged.service" ];
        };
        Install.WantedBy = [ "multi-user.target" ];
        # TODO Deluge 2 or above supports --do-not-daemonize option
        Service.ExecStart = "${pkgs.deluge}/bin/deluge-web";
      };
    };
  };
})
