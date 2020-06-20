{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.tools.clipmenu.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.tools.clipmenu.enable {
    my.home.services.clipmenu.enable = true;
  };
}
