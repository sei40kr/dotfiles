{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.groovy.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.groovy.enable {
    modules.dev.tools = {
      jenv = {
        enable = mkForce true;
        pluginsToEnable = [ "groovy" ];
      };
      maven.enable = mkForce true;
      gradle.enable = mkForce true;
    };

    my.packages = with pkgs; [ groovy ];
  };
}
