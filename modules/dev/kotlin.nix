{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.kotlin.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.kotlin.enable {
    my.packages = with pkgs; [ kotlin ];
  };
}
