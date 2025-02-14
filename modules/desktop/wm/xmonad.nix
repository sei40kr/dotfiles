{
  lib,
  config,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.wm.xmonad;
in
{
  options.modules.desktop.wm.xmonad = {
    enable = mkEnableOption "XMonad";
  };

  config = mkIf cfg.enable {
    services.xserver.enable = true;
    services.xserver.windowManager.xmonad = {
      enable = true;
      extraPackages = hPkgs: [
        hPkgs.xmonad-contrib_0_18_1
        hPkgs.xmonad-extras
      ];
      config = ../../../config/xmonad/xmonad.hs;
    };
    services.xserver.desktopManager.runXdgAutostartIfNone = true;
    services.picom = {
      enable = true;
      vSync = true;
    };

    modules.desktop.apps.feh.enable = true;
    modules.desktop.apps.polybar.enable = true;
    modules.term.sensible.enable = true;

    environment.systemPackages = with pkgs; [ xsel ];
  };
}
