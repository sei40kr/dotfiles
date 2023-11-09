{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  desktopCfg = config.modules.desktop;
  cfg = desktopCfg.sway;
  inherit (desktopCfg) autoRepeat background fonts;

  outputType = with types; submodule {
    options = {
      resolution = mkOption {
        type = nullOr outputResolutionType;
        default = null;
        example = {
          width = 1920;
          height = 1080;
          rate = 60;
        };
        description = mdDoc ''
          The resolution of the output.
        '';
      };

      position = mkOption {
        type = nullOr outputPositionType;
        default = null;
        example = {
          x = 0;
          y = 0;
        };
        description = mdDoc ''
          The position of the output.
        '';
      };

      scale = mkOption {
        type = float;
        default = 1.0;
        example = 1.0;
        description = mdDoc ''
          The scale of the output.
        '';
      };

      adaptiveSync = mkOption {
        type = nullOr bool;
        default = null;
        example = false;
        description = mdDoc ''
          Enable adaptive synchronization on the output.
        '';
      };

      renderBitDepth = mkOption {
        type = nullOr (enum [ 8 10 ]);
        default = null;
        example = 8;
        description = mdDoc ''
          The render bit depth of the output.
        '';
      };
    };
  };

  outputResolutionType = with types; submodule {
    options = {
      width = mkOption {
        type = int;
        example = 1920;
        description = "The width of the output.";
      };

      height = mkOption {
        type = int;
        example = 1080;
        description = "The height of the output.";
      };

      rate = mkOption {
        type = nullOr int;
        example = 60;
        description = "The refresh rate of the output.";
      };
    };
  };

  outputPositionType = with types; submodule {
    options = {
      x = mkOption {
        type = int;
        example = 0;
        description = "The x position of the output.";
      };

      y = mkOption {
        type = int;
        example = 0;
        description = "The y position of the output.";
      };
    };
  };

  package = pkgs.sway.override {
    sway-unwrapped = pkgs.swayfx-unwrapped;

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
    if background.image != null then
      "output * bg ${background.image.path} ${background.image.mode} ${background.color}"
    else "output * bg ${background.color} solid_color";

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
  options.modules.desktop.sway = with types; {
    enable = mkBoolOpt false;

    outputs = mkOption {
      type = attrsOf outputType;
      default = { };
      example = {
        HDMI-A-1 = {
          resolution = {
            width = 1920;
            height = 1080;
            rate = 60;
          };
          position = {
            x = 0;
            y = 0;
          };
          scale = 1;
          adaptiveSync = false;
          renderBitDepth = 8;
        };
      };
      description = mdDoc ''
        The outputs to configure.
      '';
    };
  };

  config = mkIf cfg.enable {
    user.packages = [ package pkgs.swaybg ];

    home.configFile = {
      "sway/config.d/outputs.conf" = {
        text = builtins.concatStringsSep "\n" (mapAttrsToList
          (name: output: ''
            ${optionalString (output.resolution != null) ''
              output ${name} resolution ${toString output.resolution.width}x${toString output.resolution.height}${optionalString (output.resolution.rate != null) "@${toString output.resolution.rate}"}
            ''}
            ${optionalString (output.position != null) ''
              output ${name} position ${toString output.position.x} ${toString output.position.y}
            ''}
            ${optionalString (output.scale != null) ''
              output ${name} scale ${toString output.scale}
            ''}
            ${optionalString (output.adaptiveSync != null) ''
              output ${name} adaptive_sync ${if output.adaptiveSync then "on" else "off"}
            ''}
            ${optionalString (output.renderBitDepth != null) ''
              output ${name} render_bit_depth ${toString output.renderBitDepth}
            ''}
          '')
          cfg.outputs);
        onChange = onConfigChange;
      };
      "sway/config" = {
        text = ''
          include config.d/outputs.conf

          ${backgroundCommand}

          bindswitch --reload --locked lid:on output eDP-1 disable
          bindswitch --reload --locked lid:off output eDP-1 enable
          exec_always ${init-edp-state}/bin/init-edp-state

          font ${fonts.titlebar.name or fonts.ui.name} ${toString (fonts.titlebar.size or fonts.ui.size)}

          gaps inner ${toString desktopCfg.gaps.inner}
          gaps outer ${toString (desktopCfg.gaps.outer - desktopCfg.gaps.inner)}

          input * {
              repeat_delay ${toString autoRepeat.delay}
              repeat_rate ${toString autoRepeat.interval}
          }

          ${builtins.readFile ../../../config/sway/config}
        '';
        onChange = onConfigChange;
      };
    };

    environment.sessionVariables.NIXOS_OZONE_WL = "1";

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
