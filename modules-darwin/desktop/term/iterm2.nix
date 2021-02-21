{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.term.iterm2.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.term.iterm2.enable {
    user.packages = with pkgs; [ iterm2 ];
  };
}
