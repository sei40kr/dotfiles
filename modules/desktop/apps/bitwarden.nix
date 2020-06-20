{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.bitwarden.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.bitwarden.enable {
    my.packages = with pkgs; [ bitwarden ];
  };
}
