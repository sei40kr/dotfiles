{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let downloadDir = "${config.user.home}/google-drive";
in {
  options.modules.services.flexget.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.flexget.enable {
    user.packages = [ pkgs.flexget ];
    # TODO Install FlexGet config
    home-manager.users.${config.user.name}.systemd.user.services.flexget = {
      Unit = {
        Description = "FlexGet Daemon";
        X-Restart-Triggers = [ "%h/.config/flexget/config.yml" ];
      };
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
