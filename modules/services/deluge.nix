{ config, lib, options, pkgs, ... }:

with lib;
let
  home = config.users.users."${config.my.userName}".home;
  configHome = config.home-manager.users."${config.my.userName}".xdg.configHome;
  cfg = config.modules.services.deluge;
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

    listenPorts = {
      from = mkOption {
        type = types.int;
        default = 6881;
      };

      to = mkOption {
        type = types.int;
        default = 6889;
      };
    };

    proxy = mkOption {
      type = types.submodule {
        options = {
          hostName = mkOption { type = types.str; };
          port = mkOption {
            type = types.int;
            default = 8080;
          };
          userName = mkOption { type = types.str; };
          password = mkOption { type = types.str; };
        };
      };
    };
  };

  config = mkIf cfg.enable {
    my.packages = with pkgs; [ unstable.deluge ];
    my.home.xdg.configFile."deluge/core.conf".text = ''
      {
          "file": 1,
          "format": 1
      }{
          "add_paused": false,
          "allow_remote": false,
          "auto_manage_prefer_seeds": false,
          "auto_managed": true,
          "autoadd_enable": false,
          "autoadd_location": ${builtins.toJSON "${home}/Downloads"},
          "cache_expiry": 60,
          "cache_size": 512,
          "compact_allocation": false,
          "copy_torrent_file": false,
          "daemon_port": 58846,
          "del_copy_torrent_file": false,
          "dht": true,
          "dont_count_slow_torrents": false,
          "download_location": ${builtins.toJSON "${home}/Downloads"},
          "download_location_paths_list": [],
          "enabled_plugins": [],
          "enc_in_policy": 1,
          "enc_level": 1,
          "enc_out_policy": 1,
          "enc_prefer_rc4": true,
          "geoip_db_location": "/usr/share/GeoIP/GeoIP.dat",
          "ignore_limits_on_local_network": true,
          "info_sent": 0.0,
          "listen_interface": "",
          "listen_ports": ${
            builtins.toJSON (with cfg.listenPorts; [ from to ])
          },
          "listen_random_port": 49243,
          "listen_reuse_port": true,
          "listen_use_sys_port": false,
          "lsd": true,
          "max_active_downloading": 3,
          "max_active_limit": 8,
          "max_active_seeding": 5,
          "max_connections_global": 200,
          "max_connections_per_second": 20,
          "max_connections_per_torrent": -1,
          "max_download_speed": -1.0,
          "max_download_speed_per_torrent": -1,
          "max_half_open_connections": 20,
          "max_upload_slots_global": 4,
          "max_upload_slots_per_torrent": -1,
          "max_upload_speed": -1.0,
          "max_upload_speed_per_torrent": -1,
          "move_completed": false,
          "move_completed_path": ${builtins.toJSON "${home}/Downloads"},
          "move_completed_paths_list": [],
          "natpmp": true,
          "new_release_check": true,
          "outgoing_interface": "",
          "outgoing_ports": [
              0,
              0
          ],
          "path_chooser_accelerator_string": "Tab",
          "path_chooser_auto_complete_enabled": true,
          "path_chooser_max_popup_rows": 20,
          "path_chooser_show_chooser_button_on_localhost": true,
          "path_chooser_show_hidden_files": false,
          "peer_tos": "0x00",
          "plugins_location": ${builtins.toJSON "${configHome}/deluge/plugins"},
          "pre_allocate_storage": true,
          "prioritize_first_last_pieces": false,
          "proxy": {
              "anonymous_mode": true,
              "force_proxy": true,
              "hostname": ${builtins.toJSON cfg.proxy.hostName},
              "password": ${builtins.toJSON cfg.proxy.password},
              "port": ${builtins.toJSON cfg.proxy.port},
              "proxy_hostnames": true,
              "proxy_peer_connections": true,
              "proxy_tracker_connections": true,
              "type": 3,
              "username": ${builtins.toJSON cfg.proxy.userName}
          },
          "queue_new_to_top": false,
          "random_outgoing_ports": true,
          "random_port": true,
          "rate_limit_ip_overhead": true,
          "remove_seed_at_ratio": false,
          "seed_time_limit": 180,
          "seed_time_ratio_limit": 7.0,
          "send_info": false,
          "sequential_download": false,
          "share_ratio_limit": 2.0,
          "shared": false,
          "stop_seed_at_ratio": false,
          "stop_seed_ratio": 2.0,
          "super_seeding": false,
          "torrentfiles_location": ${builtins.toJSON "${home}/Downloads"},
          "upnp": true,
          "utpex": true
      }
    '';
    networking.firewall = {
      allowedTCPPortRanges = [ cfg.listenPorts ];
      allowedUDPPortRanges = [ cfg.listenPorts ];
    };
    my.home.systemd.user.services = {
      deluged = {
        Unit = {
          Description = "Deluge BitTorrent Daemon";
          After = [ "network.target" ];
        };
        Service = {
          ExecStart = "${pkgs.unstable.deluge}/bin/deluged --do-not-daemonize";
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
        Service.ExecStart =
          "${pkgs.unstable.deluge}/bin/deluge-web --do-not-daemonize";
      };
    };
  };
}
