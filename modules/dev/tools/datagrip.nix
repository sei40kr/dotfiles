{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.tools.datagrip.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.datagrip.enable {
    my.packages = with pkgs; [ jetbrains.datagrip ];
  };
}
