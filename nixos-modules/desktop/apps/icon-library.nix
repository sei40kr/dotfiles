{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.icon-library;
in {
  options.modules.desktop.apps.icon-library = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ icon-library ]; };
}
