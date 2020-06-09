{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.shell.tools.tcpdump.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.tcpdump.enable {
    my.packages = with pkgs; [ tcpdump ];
  };
}
