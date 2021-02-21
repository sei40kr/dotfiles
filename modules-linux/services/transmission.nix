{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.transmission;
  downloadDir = "${homeDir}/Downloads";
  settings_json =
    (import "${configDir}/transmission-daemon/settings_json.nix") {
      inherit downloadDir;
    };
  rpc-authentication-required =
    settings_json.rpc-authentication-required or false;
  rpc-password = settings_json.rpc-password or "";
  rpc-port = settings_json.rpc-port or 9091;
  rpc-username = settings_json.rpc-username or "";
  config_json = import "${configDir}/transmission-remote-gtk/config_json.nix" {
    inherit rpc-authentication-required rpc-password rpc-port rpc-username;
  };
  peer-port = settings_json.peer-port or 51413;
  peer-port-random-on-start = settings_json.peer-port-random-on-start or false;
  randomPeerPortRange = {
    from = settings_json.peer-port-random-low or 49152;
    to = settings_json.peer-port-random-high or 65535;
  };
in {
  options.modules.services.transmission = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    openFirewall = mkOption {
      default = false;
      type = types.bool;
    };
  };

  config = mkIf cfg.enable {
    networking.firewall = {
      allowedTCPPorts = optionals cfg.openFirewall [ peer-port ];
      allowedUDPPorts = optionals cfg.openFirewall [ peer-port ];
      allowedTCPPortRanges =
        optionals (peer-port-random-on-start && cfg.openFirewall)
        [ randomPeerPortRange ];
      allowedUDPPortRanges =
        optionals (peer-port-random-on-start && cfg.openFirewall)
        [ randomPeerPortRange ];
    };

    user.packages = with pkgs; [ transmission transmission-remote-gtk ];
    home.configFile = {
      "transmission-daemon/settings.json".text = builtins.toJSON settings_json;
      "transmission-remote-gtk/config.json".text = builtins.toJSON config_json;
    };
    home-manager.users.${config.user.name}.systemd.user.services.transmission-daemon =
      {
        Unit = {
          Description = "Transmission BitTorrent Daemon";
          After = [ "network.target" ];
          X-Restart-Triggers =
            [ "%h/.config/transmission-daemon/settings.json" ];
        };
        Service = {
          Type = "notify";
          ExecStartPre = ''
            ${pkgs.coreutils}/bin/mkdir -p \
              %h/.config/transmission-daemon/blocklists \
              %h/.config/transmission-daemon/resume \
              %h/.config/transmission-daemon/torrents
          '';
          ExecStart =
            "${pkgs.transmission}/bin/transmission-daemon -f --log-error";
          ExecReload = "/bin/kill -s HUP $MAINPID";
          NoNewPrivileges = true;
        };
        Install.WantedBy = [ "multi-user.target" ];
      };
  };
}
