{
  config,
  inputs,
  lib,
  ...
}:

let
  inherit (lib)
    mdDoc
    mkIf
    mkOption
    types
    ;
  inherit (types) int;
  inherit (inputs.self.lib.extraTypes) fontType;

  cfg = config.modules.desktop.wm;
  deCfg = config.modules.desktop.de;
in
{
  imports = [ inputs.self.nixosModules.de ];

  options.modules.desktop.wm = {
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
