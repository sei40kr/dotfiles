{ config, lib, pkgs, ... }:

let
  inherit (lib) mdDoc mkEnableOption mkIf mkOption types;
  inherit (lib.my.extraTypes) font;
  inherit (types) attrsOf bool either enum float int nullOr path str submodule;
  cfg = config.modules.desktop.de;

  monitorResolutionType = submodule {
    options = {
      width = mkOption {
        type = int;
        example = 1920;
        description = mdDoc ''
          The width of the monitor.
        '';
      };

      height = mkOption {
        type = int;
        example = 1080;
        description = mdDoc ''
          The height of the monitor.
        '';
      };
    };
  };

  monitorPositionType = submodule {
    options = {
      x = mkOption {
        type = int;
        example = 0;
        description = mdDoc ''
          The x position of the monitor.
        '';
      };

      y = mkOption {
        type = int;
        example = 0;
        description = mdDoc ''
          The y position of the monitor.
        '';
      };
    };
  };

  monitorRotationType = submodule {
    options = {
      degrees = mkOption {
        type = enum [ 0 90 180 270 ];
        default = 0;
        description = mdDoc ''
          The degrees to rotate the monitor.
        '';
      };

      flipped = mkOption {
        type = bool;
        default = false;
        description = mdDoc ''
          Whether to flip the monitor.
        '';
      };
    };
  };

  monitorType = submodule {
    options = {
      enable = (mkEnableOption (mdDoc ''
        Whether to enable the monitor.
      '')) // { default = true; };

      resolution = mkOption {
        type = nullOr monitorResolutionType;
        default = null;
        example = { width = 1920; height = 1080; };
        description = mdDoc ''
          The resolution of the monitor.
          If not specified, the default resolution of the monitor will be used.
        '';
      };

      refreshRate = mkOption {
        type = nullOr int;
        default = null;
        example = 60;
        description = mdDoc ''
          The refresh rate of the monitor.
          If not specified, the default refresh rate of the monitor will be used.
        '';
      };

      position = mkOption {
        type = nullOr monitorPositionType;
        default = null;
        example = { x = 0; y = 0; };
        description = mdDoc ''
          The position of the monitor.
        '';
      };

      scale = mkOption {
        type = float;
        default = 1.0;
        description = mdDoc ''
          The scale of the monitor.
        '';
      };

      mirror = mkOption {
        type = nullOr str;
        default = null;
        example = "eDP-1";
        description = mdDoc ''
          The name of the monitor to mirror,
        '';
      };

      bitDepth = mkOption {
        type = enum [ 8 10 ];
        default = 8;
        description = mdDoc ''
          The bit depth of the monitor.
        '';
      };

      vrr = mkOption {
        type = bool;
        default = false;
        description = mdDoc ''
          Whether to enable variable refresh rate on the monitor.
        '';
      };

      rotation = mkOption {
        type = monitorRotationType;
        default = { degrees = 0; flipped = false; };
        description = mdDoc ''
          The rotation of the monitor.
        '';
      };
    };
  };

  backgroundImageType = submodule {
    options = {
      path = mkOption {
        type = either str path;
        example = "/path/to/background.png";
        description = mdDoc ''
          The path to the background image.
        '';
      };

      mode = mkOption {
        type = enum [ "stretch" "fill" "fit" "center" "tile" ];
        default = "fill";
        description = mdDoc ''
          The mode to use for the background image.
        '';
      };
    };
  };
in
{
  options.modules.desktop.de = {
    enable = (mkEnableOption (mdDoc ''
      Set this to true when you want to use a desktop environment.
    '')) // { visible = false; };

    wayland = (mkEnableOption (mdDoc ''
      Set this to true when you want to use a Wayland session.
    '')) // { default = false; };

    monitors = mkOption {
      type = attrsOf monitorType;
      default = { };
      example = {
        "HDMI-1" = {
          enable = true;
          resolution = { width = 1920; height = 1080; };
          refreshRate = 60;
          position = { x = 0; y = 0; };
          scale = 1.0;
          mirror = null;
          bitDepth = 8;
          rotation = {
            degrees = 0;
            flipped = false;
          };
        };
      };
      description = mdDoc ''
        The monitors to configure.
      '';
    };

    autoRepeat = {
      delay = mkOption {
        type = int;
        default = 200;
        description = mdDoc ''
          The delay in milliseconds before a key starts repeating.
        '';
      };

      interval = mkOption {
        type = int;
        default = 30;
        description = mdDoc ''
          The interval in milliseconds between key repeats.
        '';
      };
    };

    defaultFonts = {
      ui = mkOption {
        type = font;
        default = {
          package = pkgs.cantarell-fonts;
          name = "Cantarell";
          size = 11.0;
        };
        description = mdDoc ''
          The default font to use for UI elements.
        '';
      };

      fixed = mkOption {
        type = font;
        default = {
          package = pkgs.source-code-pro;
          name = "Source Code Pro";
          size = 10.0;
        };
        description = mdDoc ''
          The default font to use for fixed width text.
        '';
      };

      document = mkOption {
        type = font;
        default = {
          package = pkgs.cantarell-fonts;
          name = "Cantarell";
          size = 11.0;
        };
        description = mdDoc ''
          The default font to use for documents.
        '';
      };
    };

    background = {
      image = mkOption {
        type = nullOr backgroundImageType;
        default = null;
        example = {
          path = "/path/to/background.png";
          mode = "fill";
        };
        description = mdDoc ''
          The background image to use.
        '';
      };

      color = mkOption {
        type = str;
        default = "#404040";
        description = mdDoc ''
          The background color to use.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (mkIf (!cfg.wayland) xclip)
      (mkIf cfg.wayland wl-clipboard)
    ];

    fonts.packages = [
      (mkIf (cfg.defaultFonts.ui.package != null) cfg.defaultFonts.ui.package)
      (mkIf (cfg.defaultFonts.fixed.package != null) cfg.defaultFonts.fixed.package)
      (mkIf (cfg.defaultFonts.document.package != null) cfg.defaultFonts.document.package)
    ];

    environment.sessionVariables.NIXOS_OZONE_WL = mkIf cfg.wayland "1";
  };
}
