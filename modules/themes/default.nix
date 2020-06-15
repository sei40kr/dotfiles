{ config, lib, options, pkgs, ... }:

with lib;
let
  themeType = types.submodule {
    options = {
      package = mkOption {
        type = with types; nullOr package;
        default = null;
      };
      name = mkOption { type = types.str; };
    };
  };
in {
  options.modules.themes = {
    preferDarkTheme = mkOption {
      type = types.bool;
      default = false;
    };

    gtkTheme = mkOption {
      type = with types; nullOr themeType;
      default = null;
    };
    gtkIconTheme = mkOption {
      type = with types; nullOr themeType;
      default = null;
    };
  };
}
