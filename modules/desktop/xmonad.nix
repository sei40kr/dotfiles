{ config, lib, options, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.xmonad;
in {
  options.modules.desktop.xmonad = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    autoRepeatDelay = mkOption {
      type = types.int;
      default = 150;
    };

    autoRepeatInterval = mkOption {
      type = types.int;
      default = 30;
    };
  };

  config = mkIf cfg.enable {
    modules.desktop = {
      xdgUserDirs.enable = mkForce true;
      gtk.enable = mkForce true;
      xsecurelock.enable = mkForce true;
      picom.enable = mkForce true;

      apps = {
        dunst.enable = mkForce true;
        polybar.enable = mkForce true;
      };
    };

    # Enable X.Org Server + startx
    services.xserver = {
      enable = true;
      displayManager.startx.enable = true;
    };
    my.home.home.file.".xinitrc".text = ''
      ./.xsession
    '';

    # Enable X Session + Xmonad
    my.home.xsession = {
      enable = true;

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
    my.home.home.file.".xmonad/xmonad.hs".source = <config/xmonad/xmonad.hs>;

    my.xsession.init = ''
      xset r rate ${toString cfg.autoRepeatDelay} ${
        toString cfg.autoRepeatInterval
      }
    '';
  };
}
