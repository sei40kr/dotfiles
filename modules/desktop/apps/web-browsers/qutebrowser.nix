{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.webBrowsers.qutebrowser.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.webBrowsers.qutebrowser.enable {
    my.home.programs.qutebrowser.enable = true;
  };
}
