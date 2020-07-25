{ config, lib, pkgs, ... }:

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
    my.home.xdg.configFile = {
      "flexget/config.yml".source = <config/flexget/config.yml>;
      "flexget/proxy.yml".source = <config/flexget/proxy.yml>;
    };
    my.home.systemd.user.services.flexget = {
      Unit = {
        Description = "FlexGet Daemon";
        X-Restart-Triggers =
          [ "%h/.config/flexget/config.yml" "%h/.config/flexget/proxy.yml" ];
      };
      Service = {
        ExecStart = "${package}/bin/flexget daemon start";
        ExecStop = "${package}/bin/flexget daemon stop";
        ExecReload = "${package}/bin/flexget daemon reload";
        Restart = "on-failure";
        PrivateTmp = true;
      };
    };
  };
}
