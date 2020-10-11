{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.services.deluge;
  downloadDir =
    config.home-manager.users."${config.my.userName}".xdg.userDirs.download;
  proxy = import <secrets/config/proxy.nix>;
  proxyConfig = {
    proxy = {
      anonymous_mode = true;
      force_proxy = true;
      hostname = proxy.hostName;
      password = proxy.password;
      port = proxy.port;
      proxy_hostnames = true;
      proxy_peer_connections = true;
      proxy_tracker_connections = true;
      type = 3;
      username = proxy.userName;
    };
  };
  listenPorts = {
    from = elemAt (cfg.config.listen_ports or [ 6881 6889 ]) 0;
    to = elemAt (cfg.config.listen_ports or [ 6881 6889 ]) 1;
  };
in {
  options.modules.services.deluge = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    enableProxy = mkOption {
      type = types.bool;
      default = true;
    };

    config = mkOption {
      type = types.attrs;
      default = {
        new_release_check = false;
        autoadd_location = downloadDir;
        dont_count_slow_torrents = true;
        download_location = downloadDir;
        move_completed_path = downloadDir;
      };
    };

    openFirewall = mkOption {
      default = false;
      type = types.bool;
      description = ''
        Whether to open the firewall for the ports in
        <option>modules.services.deluge.config.listen_ports</option>.
        It does NOT apply to the daemon port nor the web UI port. To access those
        ports secuerly check the documentation
        <link xlink:href="https://dev.deluge-torrent.org/wiki/UserGuide/ThinClient#CreateSSHTunnel"/>
        or use a VPN or configure certificates for deluge.
      '';
    };

    package = mkOption {
      type = types.package;
      default = pkgs.unstable.deluge;
    };

    web = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };

      port = mkOption {
        type = types.port;
        default = 8112;
        description = ''
          Deluge web UI port.
        '';
      };

      openFirewall = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Open ports in the firewall for deluge web daemon
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    networking.firewall = {
      allowedTCPPortRanges = [ listenPorts ];
      allowedUDPPortRanges = [ listenPorts ];
    };

    my.packages = [ cfg.package ];
    my.home.xdg.configFile."deluge/core.conf".text = builtins.toJSON
      (cfg.config // (optionalAttrs cfg.enableProxy proxyConfig));
    my.home.systemd.user.services = {
      deluged = {
        Unit = {
          Description = "Deluge BitTorrent Daemon";
          After = [ "network.target" ];
        };
        Service = {
          ExecStart = "${cfg.package}/bin/deluged --do-not-daemonize";
          Restart = "on-success";
          LimitNOFILE = 4096;
        };
      };
      deluge-web = mkIf cfg.web.enable {
        Unit = {
          Description = "Deluge BitTorrent Web UI";
          After = [ "network.target" "deluged.service" ];
          Requires = [ "deluged.service" ];
        };
        Service.ExecStart = "${cfg.package}/bin/deluge-web --do-not-daemonize";
      };
    };
  };
}
