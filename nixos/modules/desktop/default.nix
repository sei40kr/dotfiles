{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  fontType = with types; submodule {
    options = {
      package = mkOpt (nullOr package) null;
      name = mkOpt str null;
      size = mkOpt int null;
    };
  };

  cfg = config.modules.desktop;
  fonts = cfg.fonts;
in
{
  options.modules.desktop = with types; {
    enable = mkBoolOpt false;

    autoRepeat = {
      delay = mkOpt int 200;
      interval = mkOpt int 30;
    };

    fonts = {
      ui = mkOpt fontType {
        package = pkgs.cantarell-fonts;
        name = "Cantarell";
        size = 11;
      };

      fixed = mkOpt fontType {
        package = pkgs.source-code-pro;
        name = "Source Code Pro";
        size = 10;
      };

      document = mkOpt fontType {
        package = pkgs.cantarell-fonts;
        name = "Cantarell";
        size = 11;
      };

      titlebar = mkOpt (nullOr fontType) null;
    };
  };

  config = mkIf cfg.enable {
    fonts.fonts = with pkgs; [
      my.sensible-terminal-wayland
      (mkIf (fonts.ui.package != null) fonts.ui.package)
      (mkIf (fonts.fixed.package != null) fonts.fixed.package)
      (mkIf (fonts.document.package != null) fonts.document.package)
      (mkIf (fonts.titlebar != null && fonts.titlebar.package != null) fonts.titlebar.package)
    ];
  };
}
