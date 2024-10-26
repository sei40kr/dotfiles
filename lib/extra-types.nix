{ lib, ... }:

let
  inherit (lib) mdDoc mkOption types;
  inherit (types)
    nullOr
    package
    str
    float
    submodule
    ;
in
{
  extraTypes = {
    font = submodule {
      options = {
        package = mkOption {
          type = nullOr package;
          default = null;
          description = mdDoc ''
            The font package to use.
          '';
        };

        name = mkOption {
          type = str;
          example = "sans-serif";
          description = mdDoc ''
            The font name to use.
          '';
        };

        size = mkOption {
          type = float;
          example = 10.0;
          description = mdDoc ''
            The font size in points.
          '';
        };
      };
    };
  };
}
