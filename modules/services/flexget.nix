{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let downloadDir = "${homeDir}/google-drive";
in {
  options.modules.services.flexget.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.flexget.enable {
    modules.services.rclone = {
      enable = mkForce true;
      enableGoogleDrive = mkForce true;
    };

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
