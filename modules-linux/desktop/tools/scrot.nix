{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.tools.scrot.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.tools.scrot.enable {
    user.packages = with pkgs; [ scrot ];
  };
}
