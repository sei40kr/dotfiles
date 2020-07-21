{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.services.redshift.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.redshift.enable {
    my.packages = with pkgs; [ redshift ];
    my.home.xdg.configFile."redshift/redshift.conf".source =
      <config/redshift/redshift.conf>;
    my.home.systemd.user.services.redshift = {
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
