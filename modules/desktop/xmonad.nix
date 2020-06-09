{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.xmonad.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.xmonad.enable {
    # Install Xserver + Xmonad as root
    services.xserver = {
      enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };

    # Install user Xmonad configuration
    my.home.home.file.".xmonad/xmonad.hs".source = <config/xmonad/xmonad.hs>;

    # XDG User Directories
    my.home.xdg.userDirs.enable = true;

    # Picom
    my.home.systemd.user.services.picom = {
      Unit = {
        Description = "Picom X11 compositor";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
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
    my.home.xdg.configFile."picom/picom.conf" = {
      source = <config/picom/picom.conf>;
      onChange = "systemctl --user restart picom.service";
    };

    # Install Fontconfig
    my.home.fonts.fontconfig.enable = mkForce true;
    my.home.xdg.configFile."fontconfig/conf.d/10-hinting-none.conf".source =
      "${pkgs.fontconfig}/share/fontconfig/conf.avail/10-hinting-none.conf";
    my.home.xdg.configFile."fontconfig/conf.d/10-sub-pixel-rgb.conf".source =
      "${pkgs.fontconfig}/share/fontconfig/conf.avail/10-sub-pixel-rgb.conf";
    my.home.xdg.configFile."fontconfig/conf.d/11-lcdfilter-default.conf".source =
      "${pkgs.fontconfig}/share/fontconfig/conf.avail/11-lcdfilter-default.conf";
    my.home.xdg.configFile."fontconfig/conf.d/66-noto-sans.conf".source =
      "${pkgs.fontconfig}/share/fontconfig/conf.avail/66-noto-sans.conf";
    my.home.xdg.configFile."fontconfig/conf.d/66-noto-serif.conf".source =
      "${pkgs.fontconfig}/share/fontconfig/conf.avail/66-noto-serif.conf";
    my.home.xdg.configFile."fontconfig/conf.d/66-noto-mono.conf".source =
      "${pkgs.fontconfig}/share/fontconfig/conf.avail/66-noto-mono.conf";

    # Sesssion Lock: xss-lock + XSecureLock
    my.home.services.screen-locker = {
      enable = true;
      lockCmd = with pkgs;
        "${xsecurelock}/libexec/xsecurelock/dimmer -l -- ${xsecurelock}/bin/xsecurelock";
      xssLockExtraOptions = [ "-n" ];
    };

    # Polybar
    my.home.services.polybar = {
      enable = true;
      config = <config/polybar/config>;
      script = ''
        polybar top &
        polybar bottom &
      '';
    };
    my.home.home.file."polybar-scripts".source = <config/polybar/scripts>;

    my.xsession.init = ''
      . "''${XDG_CONFIG_HOME:-''${HOME}/.config}/user-dirs.dirs"

      rm -f /tmp/.xmonad-workspace-log
      mkfifo /tmp/.xmonad-workspace-log
    '';

    my.packages = with pkgs; [
      # Fonts
      noto-fonts
      noto-fonts-emoji
      # XSecureLock
      xsecurelock
      # Polybar
      material-design-icons
    ];
  };
}
