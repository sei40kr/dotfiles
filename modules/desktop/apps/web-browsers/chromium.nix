{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.webBrowsers.chromium.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.webBrowsers.chromium.enable {
    my.home.programs.chromium.enable = true;
  };
}
