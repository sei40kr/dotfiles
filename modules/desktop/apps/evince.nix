{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.evince.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.evince.enable {
    my.packages = with pkgs; [ evince ];
  };
}
