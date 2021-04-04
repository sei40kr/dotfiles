{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.firefox;
in {
  options.modules.desktop.browsers.firefox.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf cfg.enable { user.packages = with pkgs; [ firefox ]; };
}
