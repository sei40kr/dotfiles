{ config, lib, pkgs, ... }:

with lib; {
  options.modules.term.iterm2.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.term.iterm2.enable {
    my.packages = with pkgs; [ iterm2 ];
  };
}
