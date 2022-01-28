{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.calibre-web;

  inherit (config.dotfiles) secretsDir;
  inherit (config.home-manager.users.${config.user.name}.xdg) configHome;

  adminPassword = import "${secretsDir}/calibre-web-admin-password.nix";
  settings = concatStringsSep "," [
    "config_calibre_dir = '${cfg.libraryDir}'"
    "config_converterpath = '${pkgs.calibre}/bin/ebook-convert'"
    "config_port = ${toString cfg.port}"
    "config_uploading = 1"
  ];

  calibre-web-start = pkgs.writeShellScript "calibre-web-start" ''
    mkdir -p ${configHome}/calibre-web
    exec ${pkgs.calibre-web}/bin/calibre-web -p ${configHome}/calibre-web/app.db \
                                             -g ${configHome}/calibre-web/gdrive.db \
                                             "$@"
  '';
  calibre-web-pre-start = pkgs.writeShellScript "calibre-web-pre-start" ''
    __RUN_MIGRATIONS_AND_EXIT=1 ${calibre-web-start}

    ${pkgs.sqlite}/bin/sqlite3 ${configHome}/calibre-web/app.db "UPDATE settings SET ${settings}"
    if [ ! -f ${escapeShellArg "${cfg.libraryDir}/metadata.db"} ]; then
      echo "Invalid Calibre library" >&2
      exit 1
    fi
  '';
in {
  options.modules.services.calibre-web = with types; {
    enable = mkBoolOpt false;

    port = mkOpt int 8083;

    libraryDir = mkOpt str "${config.user.home}/Calibre Library";
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ calibre-web ];

    systemd.user.services.cps = {
      description = "Calibre-Web";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStartPre = calibre-web-pre-start;
        ExecStart = calibre-web-start;
        Restart = "on-failure";
      };
    };

    system.activationScripts.calibre-web.text = ''
      ${calibre-web-start} -s admin:${escapeShellArg adminPassword} >/dev/null
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
