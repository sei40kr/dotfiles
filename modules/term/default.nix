{ config, lib, ... }:

let
  inherit (lib)
    mdDoc
    mkIf
    mkOption
    types
    ;
  inherit (types)
    float
    int
    nullOr
    package
    str
    submodule
    ;
  inherit (lib.my) mkOpt;

  cfg = config.modules.term;

  fontType = submodule {
    options = {
      package = mkOpt (nullOr package) null;
      name = mkOpt str null;
      size = mkOpt int null;
    };
  };
in
{
  imports = [
    ./kitty.nix
    ./sensible.nix
    ./wezterm.nix
    ./colorschemes
  ];

  options.modules.term = {
    font = mkOpt fontType {
      name = "monospace";
      size = 12;
    };

    bgOpacity = mkOption {
      type = float;
      default = 1.0;
      example = 0.95;
      description = mdDoc ''
        Opacity of the background color.
      '';
    };

    bgBlur = mkOption {
      type = int;
      default = 20;
      example = 20;
      description = mdDoc ''
        Blur radius of the background.
      '';
    };
  };

  config = {
    assertions = [
      {
        assertion = 0.0 <= cfg.bgOpacity && cfg.bgOpacity <= 1.0;
        message = "modules.term.bgOpacity must be between 0.00 and 1.00";
      }
      {
        assertion = 0 <= cfg.bgBlur;
        message = "modules.term.bgBlur must be greater than or equal to 0";
      }
    ];

    fonts.packages = mkIf (cfg.font.package != null) [ cfg.font.package ];
  };
}
