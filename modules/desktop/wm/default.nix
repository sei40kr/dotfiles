{ config, lib, ... }:

let
  inherit (lib) mdDoc mkIf mkOption types;
  inherit (lib.my.extraTypes) font;
  cfg = config.modules.desktop.wm;
  deCfg = config.modules.desktop.de;
in
{
  options.modules.desktop.wm = {
    gaps = {
      inner = mkOption {
        type = types.int;
        default = 16;
        description = mdDoc ''
          The size of the inner gaps.
        '';
      };

      outer = mkOption {
        type = types.int;
        default = 32;
        description = mdDoc ''
          The size of the outer gaps.
          Must be greater than or equal to the inner gaps.
        '';
      };
    };

    fonts = {
      titleBar = mkOption {
        type = font;
        default = deCfg.defaultFonts.ui;
        description = ''
          The font used for the title bar.
        '';
      };
    };
  };

  config = {
    assertions = [{
      assertion = cfg.gaps.inner <= cfg.gaps.outer;
      message = "The outer gaps must be greater than or equal to the inner gaps.";
    }];

    fonts.packages = [
      (mkIf (cfg.fonts.titleBar.package != null) cfg.fonts.titleBar.package)
    ];
  };
}
