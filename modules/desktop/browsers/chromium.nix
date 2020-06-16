{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.browsers.chromium.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.browsers.chromium.enable {
    modules.desktop.gtk.enable = mkForce true;

    my.home.programs.chromium.enable = true;
  };
}
