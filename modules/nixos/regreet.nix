{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins) floor toString;
  inherit (lib)
    getExe
    mkEnableOption
    mkForce
    mkIf
    mkOption
    types
    ;
  inherit (types)
    nullOr
    package
    str
    submodule
    ;

  cfg = config.modules.desktop.regreet;
  deCfg = config.modules.desktop.de;
  wmCfg = config.modules.desktop.wm;
  niriCfg = config.programs.niri;

  backgroundFit =
    if deCfg.background.image == null then
      "Contain"
    else if deCfg.background.image.mode == "fill" then
      "Cover"
    else if deCfg.background.image.mode == "fit" then
      "Contain"
    else if deCfg.background.image.mode == "stretch" then
      "Fill"
    else
      throw "Unsupported background mode '${deCfg.background.image.mode}' for regreet. Supported modes: fill, fit, stretch";
in
{
  options.modules.desktop.regreet = {
    enable = mkEnableOption "ReGreet";

    theme = mkOption {
      type = nullOr (submodule {
        options = {
          package = mkOption {
            type = package;
            description = "GTK theme package";
          };
          name = mkOption {
            type = str;
            description = "GTK theme name";
          };
        };
      });
      default = null;
      description = "GTK theme";
    };

    iconTheme = mkOption {
      type = nullOr (submodule {
        options = {
          package = mkOption {
            type = package;
            description = "Icon theme package";
          };
          name = mkOption {
            type = str;
            description = "Icon theme name";
          };
        };
      });
      default = null;
      description = "Icon theme";
    };
  };

  config = mkIf cfg.enable {
    programs.regreet = {
      enable = true;
      settings = mkIf (deCfg.background.image != null) {
        background = {
          path = toString deCfg.blurredBackgroundImage;
          fit = backgroundFit;
        };
      };
      theme = mkIf (cfg.theme != null) cfg.theme;
      iconTheme = mkIf (cfg.iconTheme != null) cfg.iconTheme;
      font = {
        inherit (deCfg.defaultFonts.ui) package name;
        size = floor deCfg.defaultFonts.ui.size;
      };
    };

    environment.etc."niri/greeter.kdl" = mkIf wmCfg.niri.enable {
      text = ''
        include "/etc/niri/outputs.kdl"

        input {
          keyboard {
            repeat-delay ${toString wmCfg.autoRepeat.delay}
            repeat-rate ${toString wmCfg.autoRepeat.interval}
          }
        }

        hotkey-overlay {
          skip-at-startup
        }

        window-rule {
          geometry-corner-radius 16
          clip-to-geometry true
        }

        animations {
          off
        }

        recent-windows {
          off
        }

        layout {
          background-color "${deCfg.background.color}"
        }

        spawn-at-startup "${getExe config.programs.regreet.package}"
      '';
    };
    services.greetd.settings.default_session.command = mkIf wmCfg.niri.enable (
      mkForce "${pkgs.dbus}/bin/dbus-run-session ${getExe niriCfg.package} -c /etc/niri/greeter.kdl"
    );
  };
}
