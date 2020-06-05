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
    home.file.".xmonad/xmonad.hs".source = <config/xmonad/xmonad.hs>;

    # picom
    services.picom = {
      enable = true;
    };

    # polybar
    services.polybar = {
      enable = true;
      config = <config/polybar/config>;
      script = ''
        polybar top &
        polybar bottom &
      '';
    };
    home.file."polybar-scripts".source = <config/polybar/scripts>;
  };
}
