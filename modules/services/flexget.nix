{ config, lib, options, pkgs, ... }:

with lib;
let
  cfg = config.modules.services.flexget;
  package = with pkgs;
    flexget.overrideAttrs (oldAttrs: {
      propagatedBuildInputs = oldAttrs.propagatedBuildInputs
        ++ [ python3Packages.deluge-client ];
    });
in {
  options.modules.services.flexget = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    proxy = mkOption {
      type = with types; nullOr str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    my.packages = with pkgs; [ package ];
    my.home.xdg.configFile."flexget/config.yml".text = ''
      templates:
        proxy:
          ${optionalString (cfg.proxy != null) "proxy: ${cfg.proxy}"}

      ${readFile <config/flexget/config.yml>}
    '';

    my.home.systemd.user.services.flexget = {
      Unit = {
        Description = "FlexGet Daemon";
        X-Restart-Triggers = [ "${<config/flexget/config.yml>}" ];
      };
      Install.WantedBy = [ "multi-user.target" ];
      Service = {
        ExecStart = "${pkgs.flexget}/bin/flexget daemon start";
        ExecStop = "${pkgs.flexget}/bin/flexget daemon stop";
        ExecReload = "${pkgs.flexget}/bin/flexget daemon reload";
        Restart = "on-failure";
        PrivateTmp = true;
      };
    };
  };
}
