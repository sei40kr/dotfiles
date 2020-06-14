{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.gtk.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.gtk.enable {
    modules.desktop.fonts.enable = true;

    my.packages = with pkgs; [ gnome3.adwaita-icon-theme ];
  };
}
