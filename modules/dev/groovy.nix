{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.groovy.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.groovy.enable {
    my.packages = with pkgs; [ groovy ];
  };
}
