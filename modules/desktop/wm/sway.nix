{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    types
    mkIf
    all
    attrValues
    mapAttrsToList
    optionalString
    ;
  inherit (lib.my) mkBoolOpt;
  desktopCfg = config.modules.desktop;
  deCfg = desktopCfg.de;
  wmCfg = desktopCfg.wm;
  cfg = wmCfg.sway;

  package = pkgs.swayfx.override {
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
  };

  backgroundCommand =
    if deCfg.background.image != null then
      "output * bg ${deCfg.background.image.path} ${deCfg.background.image.mode} ${deCfg.background.color}"
    else
      "output * bg ${deCfg.background.color} solid_color";

  init-edp-state = pkgs.writeShellScriptBin "init-edp-state" ''
    if ${pkgs.gnugrep}/bin/grep -q open /proc/acpi/button/lid/LID/state; then
      ${package}/bin/swaymsg output eDP-1 enable
    else
      ${package}/bin/swaymsg output eDP-1 disable
    fi
  '';

  onConfigChange = ''
    SWAYSOCK=''${XDG_RUNTIME_DIR:-/run/user/$UID}/sway-ipc.$UID.$(${pkgs.procps}/bin/pgrep -x sway || true).sock
    if [[ -S $SWAYSOCK ]]; then
      ${package}/bin/swaymsg -s $SWAYSOCK reload
    fi
  '';
in
{
  options.modules.desktop.wm.sway = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = all ({ mirror, ... }: mirror == null) (attrValues deCfg.monitors);
        message = "Sway does not support mirroring";
      }
      {
        assertion = all ({ resolution, refreshRate, ... }: resolution != null || refreshRate == null) (
          attrValues deCfg.monitors
        );
        message = "Sway does not support setting refresh rate without resolution";
      }
    ];

    user.packages = [
      package
      pkgs.swaybg
    ];

    home.configFile = {
      "sway/config.d/outputs.conf" = {
        text = builtins.concatStringsSep "\n" (
          mapAttrsToList (
            name:
            {
              enable,
              resolution,
              refreshRate,
              position,
              scale,
              bitDepth,
              vrr,
              rotation,
              ...
            }@monitor:
            ''
              ${optionalString (!enable) ''
                output ${name} disable
              ''}
              ${optionalString (resolution != null) ''
                output ${name} resolution ${toString resolution.width}x${toString resolution.height}${
                  optionalString (refreshRate != null) "@${toString refreshRate}"
                }
              ''}
              ${optionalString (position != null) ''
                output ${name} position ${toString position.x} ${toString position.y}
              ''}
              ${optionalString (scale != null) ''
                output ${name} scale ${toString scale}
              ''}
              ${optionalString (rotation != null) ''
                output ${name} transform ${optionalString rotation.flipped "flipped-"}${toString rotation.degrees}
              ''}
              ${optionalString monitor.vrr ''
                output ${name} adaptive_sync on
              ''}
              ${optionalString (bitDepth != 8) ''
                output ${name} render_bit_depth ${toString bitDepth}
              ''}
            ''
          ) deCfg.monitors
        );
        onChange = onConfigChange;
      };
      "sway/config" = {
        text = ''
          include config.d/outputs.conf

          ${backgroundCommand}

          bindswitch --reload --locked lid:on output eDP-1 disable
          bindswitch --reload --locked lid:off output eDP-1 enable
          exec_always ${init-edp-state}/bin/init-edp-state

          font ${wmCfg.fonts.titleBar.name} ${toString wmCfg.fonts.titleBar.size}

          gaps inner ${toString wmCfg.gaps.inner}
          gaps outer ${toString (wmCfg.gaps.outer - wmCfg.gaps.inner)}

          input * {
              repeat_delay ${toString deCfg.autoRepeat.delay}
              repeat_rate ${toString deCfg.autoRepeat.interval}
          }

          ${builtins.readFile ../../../config/sway/config}

          exec_always systemctl --user start swayidle.service
          exec_always systemctl --user start waybar.service
          ${optionalString config.modules.i18n.japanese.enable ''
            exec_always systemctl --user start fcitx5-daemon.service
          ''}
        '';
        onChange = onConfigChange;
      };
    };

    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    systemd.user.targets.sway-session = {
      description = "sway compositor session";
      documentation = [ "man:systemd.special(7)" ];
      bindsTo = [ "graphical-session.target" ];
      wants = [ "graphical-session-pre.target" ];
      after = [ "graphical-session-pre.target" ];
    };

    services.xserver.enable = true;
    services.displayManager.sessionPackages = [ package ];

    security.polkit.enable = true;
    # For screen sharing (this option only has an effect with xdg.portal.enable):
    xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-wlr ];

    modules.desktop.de.enable = true;
    modules.desktop.de.wayland = true;

    modules.desktop.swaylock.enable = true;

    modules.desktop.browsers.sensible.enable = true;
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
