{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.java.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.java.enable {
    modules.dev.tools.jenv = {
      enable = mkForce true;
      pluginsToEnable = [ "maven" "gradle" ];
    };

    my.packages = with pkgs; [ jdk11 maven gradle ];
  };
}
