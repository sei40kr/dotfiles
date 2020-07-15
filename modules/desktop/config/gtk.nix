{ config, lib, options, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.config.gtk;
in {
  options.modules.desktop.config.gtk = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    font = mkOption {
      type = with types; nullOr str;
      default = null;
    };

    preferDarkTheme = mkOption {
      type = types.bool;
      default = false;
    };

    gtk3ExtraCss = mkOption {
      type = with types; nullOr path;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    modules.desktop = {
      backends = {
        dconf.enable = true;
        gsettingsDesktopSchemas = {
          enable = mkForce true;
          packages = with pkgs; [ gtk3 ];
        };
      };
      config.fontconfig.enable = true;
      fonts.enable = true;
    };

    my.home.gtk = {
      enable = true;
      gtk2.extraConfig = optionalString (cfg.font != null) ''
        gtk-font-name = '${cfg.font}'
      '';
      gtk3 = {
        extraConfig = {
          gtk-application-prefer-dark-theme = cfg.preferDarkTheme;
          gtk-font-name = mkIf (cfg.font != null) cfg.font;
          # Menu delay
          gtk-menu-popup-delay = 0;
          # Disable mouse paste
          gtk-enable-primary-paste = false;
        };
        extraCss =
          optionalString (cfg.gtk3ExtraCss != null) readFile cfg.gtk3ExtraCss;
      };
    };
  };
}
