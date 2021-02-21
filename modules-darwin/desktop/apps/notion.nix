{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.notion.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.notion.enable {
    user.packages = with pkgs.my; [ notion ];
  };
}
