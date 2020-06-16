{ config, lib, options, pkgs, ... }:

with lib;
(let
  cfg = config.modules.services.deluge;
  package = pkgs.callPackage <packages/deluge.nix> { };
in {
  options.modules.services.deluge = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    enableWebUI = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    my.packages = [ package ];

    my.home.systemd.user.services = {
      deluged = {
        Unit = {
          Description = "Deluge BitTorrent Daemon";
          After = [ "network.target" ];
        };
        Install.WantedBy = [ "multi-user.target" ];
        Service = {
          ExecStart = "${package}/bin/deluged --do-not-daemonize";
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
        Service.ExecStart = "${package}/bin/deluge-web --do-not-daemonize";
      };
    };
  };
})
