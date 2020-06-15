{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.tools.picom.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.tools.picom.enable {
    my.packages = with pkgs; [ picom ];
    my.home.xdg.configFile."picom/picom.conf" = {
      source = <config/picom/picom.conf>;
      onChange = "systemctl --user restart picom.service";
    };
    my.home.systemd.user.services.picom = {
      Unit = {
        Description = "Picom X11 compositor";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
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
