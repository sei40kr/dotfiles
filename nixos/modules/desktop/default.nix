{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  backgroundImageType = with types; submodule {
    options = {
      path = mkOpt (either str path) null;
      mode = mkOpt (enum [ "stretch" "fill" "fit" "center" "tile" ]) null;
    };
  };
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

    background = {
      color = mkOpt str "#404040";
      image = mkOpt (nullOr backgroundImageType) null;
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

    gaps = {
      inner = mkOpt int 16;
      outer = mkOpt int 32;
    };
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = cfg.gaps.inner <= cfg.gaps.outer;
      message = "The 'modules.desktop.gaps.outer' must be equal to or greater than 'modules.desktop.gaps.inner'.";
    }];

    fonts.packages = with pkgs; [
      (mkIf (fonts.ui.package != null) fonts.ui.package)
      (mkIf (fonts.fixed.package != null) fonts.fixed.package)
      (mkIf (fonts.document.package != null) fonts.document.package)
      (mkIf (fonts.titlebar != null && fonts.titlebar.package != null) fonts.titlebar.package)
    ];

    systemd.user.targets.autostart = {
      description = "Current graphical user session";
      documentation = [ "man:systemd.special(7)" ];
    };
  };
}
