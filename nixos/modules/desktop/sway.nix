{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  desktopCfg = config.modules.desktop;
  cfg = desktopCfg.sway;
  inherit (desktopCfg) autoRepeat background fonts;

  package = pkgs.sway.override {
    extraSessionCommands = ''
      export WLR_NO_HARDWARE_CURSORS=1

      # SDL
      export SDL_VIDEODRIVER=wayland

      # QT
      export QT_QPA_PLATFORM=wayland-egl
      export QT_WAYLAND_DISABLE_WINDOWDECORATION=1

      # Fix for some Java AWT applications
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
    withGtkWrapper = true;
  };
  backgroundCommand =
    if background.image != null then
      "output * bg ${background.image.path} ${background.image.mode} ${background.color}"
    else "output * bg ${background.color} solid_color";
in
{
  options.modules.desktop.sway = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [ package pkgs.swaybg ];

    home.configFile."sway/config" = {
      source = pkgs.substituteAll {
        src = ../../../config/sway/config;
        inherit backgroundCommand;
        titlebarFontName = fonts.titlebar.name or fonts.ui.name;
        titlebarFontSize = fonts.titlebar.size or fonts.ui.size;
        innerGaps = desktopCfg.gaps.inner;
        outerGaps = desktopCfg.gaps.outer - desktopCfg.gaps.inner;
        autoRepeatDelay = autoRepeat.delay;
        autoRepeatInterval = autoRepeat.interval;
      };
      onChange = ''
        SWAYSOCK=''${XDG_RUNTIME_DIR:-/run/user/$UID}/sway-ipc.$UID.$(${pkgs.procps}/bin/pgrep -x sway || true).sock
        if [[ -S $SWAYSOCK ]]; then
          ${package}/bin/swaymsg -s $SWAYSOCK reload
        fi
      '';
    };

    systemd.user.targets = {
      sway-session = {
        description = "sway compositor session";
        documentation = [ "man:systemd.special(7)" ];
        bindsTo = [ "graphical-session.target" ];
        wants = [ "graphical-session-pre.target" "autostart.target" ];
        after = [ "graphical-session-pre.target" ];
      };
      autostart = { partOf = [ "sway-session.target" ]; };
    };

    services.xserver = {
      enable = true;
      displayManager.sessionPackages = [ package ];
    };

    security.polkit.enable = true;
    # For screen sharing (this option only has an effect with xdg.portal.enable):
    xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-wlr ];

    modules.desktop.swaylock.enable = true;

    modules.term.sensible.enable = true;
    modules.desktop.apps.dunst.enable = true;
    modules.desktop.apps.rofi.enable = true;
    modules.desktop.apps.waybar.enable = true;

    modules.desktop.dconf.enable = true;
    modules.desktop.fontconfig.enable = true;
    modules.desktop.gtk.enable = true;
    modules.desktop.qt.enable = true;
  };
}
