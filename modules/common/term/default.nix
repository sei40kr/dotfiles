{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.term;
  fontType = with types;
    submodule {
      options = {
        package = mkOpt (nullOr package) null;
        name = mkOpt str null;
        size = mkOpt int null;
      };
    };
in {
  options.modules.term = with types; {
    font = mkOpt fontType {
      name = "monospace";
      size = 10;
    };
    theme = {
      colorscheme = mkOpt str null;

      colors = {
        fg = mkOpt str null;
        bg = mkOpt str null;
        cursor = mkOpt str null;

        black = mkOpt str null;
        red = mkOpt str null;
        green = mkOpt str null;
        yellow = mkOpt str null;
        blue = mkOpt str null;
        magenta = mkOpt str null;
        cyan = mkOpt str null;
        white = mkOpt str null;
      };
    };
  };

  config.user.packages =
    optionals (cfg.font.package != null) [ cfg.font.package ];
}
