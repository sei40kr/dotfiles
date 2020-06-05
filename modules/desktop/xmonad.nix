{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.xmonad.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.xmonad.enable {
    xsession.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    # picom
    services.picom = {
      enable = true;
    };
  };
}
