{ config, lib, pkgs, ... }:

with lib;
let
  home = config.users.users."${config.my.userName}".home;
  downloadDir = "${home}/google-drive";
  config_yml =
    import <secrets/config/flexget/config_yml.nix> { inherit downloadDir; };
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

    my.packages = [ pkgs.flexget ];
    my.home.xdg.configFile."flexget/config.yml".text =
      generators.toYAML { } config_yml;
    my.home.systemd.user.services.flexget = {
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
