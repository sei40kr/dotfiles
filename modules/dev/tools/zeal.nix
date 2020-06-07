{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.tools.zeal.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.zeal.enable {
    my.packages = with pkgs; [ zeal ];
  };
}
