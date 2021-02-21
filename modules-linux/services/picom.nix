{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.services.picom.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.picom.enable {
    user.packages = with pkgs; [ picom ];
    home.configFile."picom/picom.conf".source = "${configDir}/picom/picom.conf";
    home-manager.users.${config.user.name}.systemd.user.services.picom = {
      Unit = {
        Description = "Picom X11 compositor";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
        X-Restart-Triggers = [ "${configDir}/picom/picom.conf" ];
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
