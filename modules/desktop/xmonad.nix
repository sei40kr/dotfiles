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

    # X Session
    xsession.enable = true;

    # XDG User Directories
    xdg.userDirs.enable = true;

    # Picom
    systemd.user.services.picom = {
      Unit = {
        Description = "Picom X11 compositor";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
      Service = {
        ExecStart = "${pkgs.picom}/bin/picom";
        Restart = "always";
        RestartSec = 3;
        # Temporarily fixes corrupt colours with Mesa 18.
        Environment = [ "allow_rgb10_configs=false" ];
      };
    };
    xdg.configFile."picom/picom.conf".source = <config/picom/picom.conf>;

    # Fontconfig
    fonts.fontconfig.enable = true;
    xdg.configFile = {
      "fontconfig/conf.d/10-hinting-none.conf".source =
        "${pkgs.fontconfig}/share/fontconfig/conf.avail/10-hinting-none.conf";
      "fontconfig/conf.d/10-sub-pixel-rgb.conf".source =
        "${pkgs.fontconfig}/share/fontconfig/conf.avail/10-sub-pixel-rgb.conf";
      "fontconfig/conf.d/11-lcdfilter-default.conf".source =
        "${pkgs.fontconfig}/share/fontconfig/conf.avail/11-lcdfilter-default.conf";
      "fontconfig/conf.d/66-noto-sans.conf".source =
        "${pkgs.fontconfig}/share/fontconfig/conf.avail/66-noto-sans.conf";
      "fontconfig/conf.d/66-noto-serif.conf".source =
        "${pkgs.fontconfig}/share/fontconfig/conf.avail/66-noto-serif.conf";
      "fontconfig/conf.d/66-noto-mono.conf".source =
        "${pkgs.fontconfig}/share/fontconfig/conf.avail/66-noto-mono.conf";
      "fontconfig/conf.d/70-noto-cjk.conf".source =
        "${pkgs.fontconfig}/share/fontconfig/conf.avail/70-noto-cjk.conf";
    };

    # Polybar
    services.polybar = {
      enable = true;
      config = <config/polybar/config>;
      script = ''
        polybar top &
        polybar bottom &
      '';
    };
    home.file."polybar-scripts".source = <config/polybar/scripts>;

    my.packages = with pkgs; [
      # Fonts
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      # Picom
      picom
      mesa
      # Polybar
      material-design-icons
    ];
  };
}
