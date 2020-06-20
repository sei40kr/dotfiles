{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.services.picom.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.picom.enable {
    my.packages = with pkgs; [ picom ];
    my.home.xdg.configFile."picom/picom.conf".source =
      <config/picom/picom.conf>;
    my.home.systemd.user.services.picom = {
      Unit = {
        Description = "Picom X11 compositor";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
        X-Restart-Triggers = [ "${<config/picom/picom.conf>}" ];
      };
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        ExecStart = "${pkgs.picom}/bin/picom";
        Restart = "always";
        RestartSec = 3;
        # Temporarily fixes corrupt colours with Mesa 18.
        Environment = [ "allow_rgb10_configs=false" ];
      };
    };
  };
}
