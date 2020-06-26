{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.config.gtk.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.config.gtk.enable {
    modules.desktop = {
      backends = {
        dconf.enable = true;
        gsettingsDesktopSchemas = {
          enable = mkForce true;
          packages = with pkgs; [ gtk3 ];
        };
      };
      fonts.enable = true;
    };

    my.home.gtk.enable = true;
  };
}
