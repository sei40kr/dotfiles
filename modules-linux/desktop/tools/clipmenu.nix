{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.tools.clipmenu.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.tools.clipmenu.enable {
    my.home.services.clipmenu.enable = true;

    modules.desktop.apps.rofi.systemMenuItems."Clear Clipboard History" =
      "${pkgs.clipmenu}/bin/clipdel -d '.*'";
  };
}
