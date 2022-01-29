{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.calibre-web;

  inherit (config.dotfiles) secretsDir;
  adminPassword = import "${secretsDir}/calibre-web-admin-password.nix";

  dataDir = "calibre-web";

  settings = concatStringsSep "," [
    "config_calibre_dir = '/var/lib/${dataDir}/books'"
    "config_converterpath = '${pkgs.calibre}/bin/ebook-convert'"
    "config_port = ${toString cfg.port}"
    "config_uploading = 1"
  ];
in {
  options.modules.services.calibre-web = with types; {
    enable = mkBoolOpt false;

    port = mkOpt int 8083;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ calibre-web ];

    systemd.services.calibre-web = {
      description =
        "Web app for browsing, reading and downloading eBooks stored in a Calibre database";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        Environment = [ "CALIBRE_DBPATH=/var/lib/${dataDir}" ];
        ExecStartPre = pkgs.writeShellScript "calibre-web-pre-start" ''
          mkdir -p /var/lib/${dataDir}/books

          # Set admin's password
          ${pkgs.calibre-web}/bin/calibre-web \
            -s admin:${escapeShellArg adminPassword} >/dev/null

          __RUN_MIGRATIONS_AND_EXIT=1 ${pkgs.calibre-web}/bin/calibre-web
          ${pkgs.sqlite}/bin/sqlite3 /var/lib/${dataDir}/app.db \
            "UPDATE settings SET ${settings}"
        '';
        ExecStart = "${pkgs.calibre-web}/bin/calibre-web";
        Restart = "on-failure";
        User = "calibre-web";
        Group = "calibre-web";
        StateDirectory = "calibre-web";
      };
    };

    users = {
      users.calibre-web = {
        isSystemUser = true;
        group = "calibre-web";
      };
      groups.calibre-web = { };
    };

    # TODO Extract these proxy settings to a module
    services.nginx = {
      enable = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      enableReload = true;
      clientMaxBodySize = "50m";
      virtualHosts = {
        "calibre-web.yong-ju.me" = {
          listen = [
            {
              port = 10080;
              addr = "*";
            }
            {
              port = 10443;
              addr = "*";
              ssl = true;
            }
          ];
          enableACME = true;
          forceSSL = true;
          http2 = true;
          extraConfig = ''
            proxy_buffer_size       128k;
            proxy_buffers           4 256k;
            proxy_busy_buffers_size 256k;
          '';
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString cfg.port}";
          };
        };
      };
    };
    security.acme = {
      acceptTerms = true;
      defaults.email = "sei40kr@gmail.com";
    };

    networking.firewall.allowedTCPPorts = [ 10080 10443 ];
  };
}
