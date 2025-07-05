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
    services.xserver = {
      enable = true;
      exportConfiguration = true;
      enableTearFree = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ../../../config/xmonad/xmonad.hs;
      };
      desktopManager.runXdgAutostartIfNone = true;
    };
    services.picom = {
      enable = true;
      backend = "glx";
      vSync = true;
      settings = {
        unredir-if-possible = true;
        unredir-if-possible-exclude = [
          "class_g = 'Google-chrome'"
          "class_g = 'firefox'"
          "class_g = 'Vivaldi-stable'"
        ];
      };
    };

    modules.desktop.apps.feh.enable = true;
    modules.desktop.apps.polybar.enable = true;
    modules.term.sensible.enable = true;

    environment.systemPackages = with pkgs; [ xsel ];
  };
}
