{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.gtk.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.gtk.enable {
    modules.desktop = {
      backends.dconf.enable = true;
      fonts.enable = true;
    };

    my.home.gtk.enable = true;
  };
}
