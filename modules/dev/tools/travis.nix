{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.tools.travis.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.travis.enable {
    my.packages = with pkgs; [ travis ];
  };
}
