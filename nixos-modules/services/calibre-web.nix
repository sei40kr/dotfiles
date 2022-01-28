{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.calibre-web;

  inherit (config.dotfiles) secretsDir;
  adminPassword = import "${secretsDir}/calibre-web-admin-password.nix";

  app_db = "/var/lib/calibre-web/app.db";
  gdrive_db = "/var/lib/calibre-web/gdrive.db";

  wrapper = pkgs.writeShellScript "calibre-web" ''
    exec ${pkgs.calibre-web}/bin/calibre-web -p ${app_db} -g ${gdrive_db} "$@"
  '';

  settings = concatStringsSep "," [
    "config_calibre_dir = '${cfg.libraryDir}'"
    "config_converterpath = '${pkgs.calibre}/bin/ebook-convert'"
    "config_port = ${toString cfg.port}"
    "config_uploading = 1"
  ];
in {
  options.modules.services.calibre-web = with types; {
    enable = mkBoolOpt false;

    port = mkOpt int 8083;

    libraryDir = mkOpt str "${config.user.home}/Calibre Library";
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ calibre-web ];

    systemd.services.calibre-web = {
      description = "Calibre-Web";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStartPre = pkgs.writeShellScript "calibre-web-pre-start" ''
          __RUN_MIGRATIONS_AND_EXIT=1 ${wrapper}

          ${pkgs.sqlite}/bin/sqlite3 ${app_db} "UPDATE settings SET ${settings}"
          if [ ! -f ${escapeShellArg "${cfg.libraryDir}/metadata.db"} ]; then
            echo "Invalid Calibre library" >&2
            exit 1
          fi
        '';
        ExecStart = wrapper;
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

    # Set admin's password
    system.activationScripts.calibre-web.text = ''
      ${wrapper} -s admin:${escapeShellArg adminPassword} >/dev/null
    '';

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
