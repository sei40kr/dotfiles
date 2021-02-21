{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.groovy.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.groovy.enable {
    # TODO groovy-language-server
    modules.dev.tools = {
      jenv = {
        enable = mkForce true;
        plugins = [ "groovy" ];
      };
      maven.enable = mkForce true;
      gradle.enable = mkForce true;
    };

    user.packages = with pkgs; [ groovy ];
  };
}
