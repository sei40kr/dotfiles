{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.tools.flexget.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.flexget.enable {
    my.packages = [ pkgs.flexget ];

    systemd.user.services.flexget = {
      Unit.Description = "FlexGet Daemon";
      Install.WantedBy = [ "multi-user.target" ];
      Service = {
        ExecStart = "${pkgs.flexget}/bin/flexget daemon start";
        ExecStop = "${pkgs.flexget}/bin/flexget daemon stop";
        ExecReload = "${pkgs.flexget}/bin/flexget daemon reload";
        Restart = "on-failure";
        PrivateTmp = true;
        WorkingDirectory = config.home.homeDirectory;
      };
    };

    xdg.configFile."flexget/config.yml" = {
      source = <config/flexget/config.yml>;
      onChange = "systemctl --user restart flexget.service";
    };
  };
}
