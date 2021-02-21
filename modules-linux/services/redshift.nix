{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.services.redshift.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.redshift.enable {
    user.packages = with pkgs; [ redshift ];
    home.configFile."redshift/redshift.conf".source =
      "${configDir}/redshift/redshift.conf";
    home-manager.users.${config.user.name}.systemd.user.services.redshift = {
      Unit = {
        Description = "Redshift colour temperature adjuster";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
        X-Restart-Triggers = [ "%h/.config/redshift/redshift.conf" ];
      };
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        ExecStart = "${pkgs.redshift}/bin/redshift";
        RestartSec = 3;
        Restart = "always";
      };
    };
  };
}
