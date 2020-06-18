{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.services.flexget.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.flexget.enable
    (let package = pkgs.callPackage <packages/flexget.nix> { };
    in rec {
      my.packages = with pkgs; [ package ];
      my.home.xdg.configFile."flexget/config.yml" = {
        source = <config/flexget/config.yml>;
        onChange = "systemctl --user reload flexget.service";
      };

      my.home.systemd.user.services.flexget = {
        Unit.Description = "FlexGet Daemon";
        Install.WantedBy = [ "multi-user.target" ];
        Service = {
          ExecStart = "${pkgs.flexget}/bin/flexget daemon start";
          ExecStop = "${pkgs.flexget}/bin/flexget daemon stop";
          ExecReload = "${pkgs.flexget}/bin/flexget daemon reload";
          Restart = "on-failure";
          PrivateTmp = true;
        };
      };
    });
}
