{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.dash;
in {
  options.modules.desktop.apps.dash = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ my.dash ]; };
}
