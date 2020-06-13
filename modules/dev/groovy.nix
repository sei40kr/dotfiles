{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.groovy.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.groovy.enable {
    modules.dev.tools.jenv = {
      enable = mkForce true;
      pluginsToEnable = [ "groovy" "maven" "gradle" ];
    };

    my.packages = with pkgs; [ groovy maven gradle ];
  };
}
