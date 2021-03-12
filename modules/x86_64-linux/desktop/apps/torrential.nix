{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.torrential;
in {
  options.modules.desktop.apps.torrential = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ torrential ]; };
}
