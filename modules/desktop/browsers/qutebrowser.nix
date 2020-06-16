{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.browsers.qutebrowser.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.browsers.qutebrowser.enable {
    my.home.programs.qutebrowser = {
      enable = true;
      extraConfig = "config.source('${<config/qutebrowser/config.py>}')";
    };
  };
}
