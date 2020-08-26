{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.jupyter.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.jupyter.enable {
    my.packages = with pkgs.python37Packages; [ jupyter matplotlib numpy ];
  };
}
