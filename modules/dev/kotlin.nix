{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.kotlin.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.kotlin.enable {
    modules.dev.tools.jenv = {
      enable = mkForce true;
      pluginsToEnable = [ "maven" "gradle" ];
    };

    my.packages = with pkgs; [ kotlin maven gradle ];
  };
}
