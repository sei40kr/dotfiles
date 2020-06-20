{ config, lib, options, pkgs, ... }:

with lib;
let
  package = with pkgs;
    flexget.overrideAttrs (oldAttrs: {
      propagatedBuildInputs = oldAttrs.propagatedBuildInputs
        ++ [ python3Packages.deluge-client ];
    });
in {
  options.modules.services.flexget.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.flexget.enable {
    my.packages = with pkgs; [ package ];
    my.home.xdg.configFile."flexget/config.yml".source =
      <config/flexget/config.yml>;

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
