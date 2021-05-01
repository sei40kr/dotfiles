{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.discord.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.discord.enable {
    user.packages = with pkgs; [ discord ];
  };
}
