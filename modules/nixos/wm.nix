{
  config,
  inputs,
  lib,
  ...
}:

let
  inherit (lib)
    mdDoc
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  inherit (types)
    attrsOf
    bool
    enum
    float
    int
    nullOr
    str
    submodule
    ;
  inherit (inputs.self.lib.extraTypes) fontType;

  cfg = config.modules.desktop.wm;
  deCfg = config.modules.desktop.de;

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
        type = enum [
          0
          90
          180
          270
        ];
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
      enable =
        (mkEnableOption (mdDoc ''
          Whether to enable the monitor.
        ''))
        // {
          default = true;
        };

      resolution = mkOption {
        type = nullOr monitorResolutionType;
        default = null;
        example = {
          width = 1920;
          height = 1080;
        };
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
        example = {
          x = 0;
          y = 0;
        };
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
        type = enum [
          8
          10
        ];
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
        default = {
          degrees = 0;
          flipped = false;
        };
        description = mdDoc ''
          The rotation of the monitor.
        '';
      };
    };
  };
in
{
  options.modules.desktop.wm = {
    monitors = mkOption {
      type = attrsOf monitorType;
      default = { };
      example = {
        "HDMI-1" = {
          enable = true;
          resolution = {
            width = 1920;
            height = 1080;
          };
          refreshRate = 60;
          position = {
            x = 0;
            y = 0;
          };
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

    gaps = {
      inner = mkOption {
        type = int;
        default = 16;
        description = mdDoc "The size of the inner gaps.";
      };

      outer = mkOption {
        type = int;
        default = 32;
        description = mdDoc "The size of the outer gaps. Must be greater than or equal to the inner gaps.";
      };
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

    fonts = {
      titleBar = mkOption {
        type = fontType;
        default = deCfg.defaultFonts.ui;
        description = mdDoc "The font used for the title bar.";
      };
    };
  };

  config = {
    assertions = [
      {
        assertion = cfg.gaps.inner <= cfg.gaps.outer;
        message = "The outer gaps must be greater than or equal to the inner gaps.";
      }
    ];

    fonts.packages = [ (mkIf (cfg.fonts.titleBar.package != null) cfg.fonts.titleBar.package) ];
  };
}
