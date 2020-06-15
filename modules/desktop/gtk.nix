{ config, lib, options, pkgs, ... }:

with lib;
let themeCfg = config.modules.themes;
in {
  options.modules.desktop.gtk.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.gtk.enable {
    modules.desktop = {
      backends.dconf.enable = true;
      fonts.enable = true;
    };

    my.home.gtk = {
      enable = true;

      iconTheme = themeCfg.gtkIconTheme;
      theme = themeCfg.gtkTheme;

      gtk3.extraConfig.gtk-application-prefer-dark-theme =
        themeCfg.preferDarkTheme;
    };
  };
}
