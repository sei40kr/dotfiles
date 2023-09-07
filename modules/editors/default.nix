{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors;

  fontType = with types; submodule {
    options = {
      package = mkOpt (nullOr package) null;
      name = mkOpt str null;
      size = mkOpt int null;
    };
  };
in
{
  options.modules.editors = with types; {
    fonts = {
      code = mkOpt fontType {
        name = "monospace";
        size = 12;
      };
      ui = mkOpt fontType {
        name = "sans-serif";
        size = 11;
      };
    };
  };

  config = {
    fonts.packages = [
      (mkIf (cfg.fonts.code.package != null) cfg.fonts.code.package)
      (mkIf (cfg.fonts.ui.package != null) cfg.fonts.ui.package)
    ];
  };
}
