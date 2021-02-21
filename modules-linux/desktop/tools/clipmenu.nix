{ config, home-manager, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.tools.clipmenu.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.tools.clipmenu.enable {
    modules.desktop.apps.rofi.customEntries."Clear Clipboard History" =
      "${pkgs.clipmenu}/bin/clipdel -d '.*'";

    user.packages = with pkgs; [ clipmenu ];
    home-manager.users.${config.user.name}.systemd.user.services.clipmenu = {
      Unit = {
        Description = "Clipboard management daemon";
        After = [ "graphical-session.target" ];
      };
      Service.ExecStart = "${pkgs.clipmenu}/bin/clipmenud";
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
