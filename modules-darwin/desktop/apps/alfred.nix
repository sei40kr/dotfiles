{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.alfred.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.alfred.enable {
    user.packages = with pkgs.my; [ alfred ];
  };
}
