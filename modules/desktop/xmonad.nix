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
    modules = {
      desktop = {
        xdgUserDirs.enable = mkForce true;
        gtk.enable = mkForce true;
        xsecurelock.enable = mkForce true;

        apps = {
          dunst.enable = mkForce true;
          polybar.enable = mkForce true;
        };
      };

      services.picom.enable = mkForce true;
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
    my.packages = with pkgs; [ xorg.xmessage ]; # required by Xmonad
    my.home.home.file.".xmonad/app".source = <config/xmonad/app>;
    my.home.home.file.".xmonad/src".source = <config/xmonad/src>;
    my.home.home.file.".xmonad/build".source = <config/xmonad/build>;
    my.home.home.file.".xmonad/package.yaml".source =
      <config/xmonad/package.yaml>;
    my.home.home.file.".xmonad/shell.nix".source = <config/xmonad/shell.nix>;
    my.home.home.file.".xmonad/stack.yaml".source = <config/xmonad/stack.yaml>;
    my.home.home.file.".xmonad/stack.yaml.lock".source =
      <config/xmonad/stack.yaml.lock>;
    my.xsession.init = ''
      xset r rate ${toString cfg.autoRepeatDelay} ${
        toString cfg.autoRepeatInterval
      }
    '';
  };
}
