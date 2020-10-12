{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.services.transmission;
  home = config.users.users."${config.my.userName}".home;
  downloadDir = "${home}/Downloads";
  settings_json = (import <config/transmission-daemon/settings_json.nix>) {
    inherit downloadDir;
  };
  rpc-authentication-required =
    settings_json.rpc-authentication-required or false;
  rpc-password = settings_json.rpc-password or "";
  rpc-port = settings_json.rpc-port or 9091;
  rpc-username = settings_json.rpc-username or "";
  config_json = import <config/transmission-remote-gtk/config_json.nix> {
    inherit rpc-authentication-required rpc-password rpc-port rpc-username;
  };
  peer-port = settings_json.peer-port or 51413;
  peer-port-random-on-start = settings_json.peer-port-random-on-start or false;
  randomPeerPortRange = {
    from = settings_json.peer-port-random-low or 49152;
    to = settings_json.peer-port-random-high or 65535;
  };
  proxyCredential = import <secrets/config/proxy.nix>;
  proxy = with proxyCredential;
    "https://${userName}:${password}@${hostName}:${toString port}";
in {
  options.modules.services.transmission = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    enableProxy = mkOption {
      type = types.bool;
      default = true;
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

    my.packages = with pkgs; [ transmission transmission-remote-gtk ];
    my.home.xdg.configFile = {
      "transmission-daemon/settings.json".text = builtins.toJSON settings_json;
      "transmission-remote-gtk/config.json".text = builtins.toJSON config_json;
    };
    my.home.systemd.user.services.transmission-daemon = {
      Unit = {
        Description = "Transmission BitTorrent Daemon";
        After = [ "network.target" ];
      };
      Service = {
        Type = "notify";
        ExecStart =
          "${pkgs.transmission}/bin/transmission-daemon -f --log-error";
        ExecReload = "/bin/kill -s HUP $MAINPID";
        Environment =
          [ "http_proxy=${proxy}" "HTTPS_PROXY=${proxy}" "NO_PROXY=localhost" ];
        NoNewPrivileges = true;
      };
      Install.WantedBy = [ "multi-user.target" ];
    };
  };
}
